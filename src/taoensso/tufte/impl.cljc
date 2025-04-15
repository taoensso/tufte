(ns ^:no-doc taoensso.tufte.impl
  "Private implementation details.
  `profiled` -> [<result> <derefable-and-mergeable-pstats>].

  Profiling consists of:
    1. State init   ; On  thread
    2. Capture      ; On  thread
    3. State deref  ; On  thread
    4. ?Merging     ; Off thread, on demand (deferred cost)
    5. ?Realization ; Off thread, on demand (deferred cost)

  Basic implementation:
    - Capture [<id> <elapsed>]s into single mutable acc
      - May compact acc      to id-times,  {<id> (<time>         ...)}
      - May compact id-times to id-sstats, {<id> (<stats/sstats> ...)}
    - Merge pours (read-only) acc0 + acc1 into id-times
      - May compact id-times to id-sstats, {<id> (<stats/sstats> ...)}
    - Realization:
        - Generates {<id> <stats/sstats>} from id-times.
        - Merges with id-sstats."

  (:require
   [clojure.string  :as str]
   [taoensso.truss  :as truss]
   [taoensso.encore :as enc]
   [taoensso.encore.stats :as stats])

  #?(:clj
     (:import [java.util LinkedList Stack]
              [java.util.concurrent ArrayBlockingQueue]))

  #?(:cljs
     (:require-macros
      [taoensso.tufte.impl :refer [mt-acc mt-add mt-count atom?]])))

;;;; Mutable accumulators

(deftype Time     [id ^long t location-info])
(deftype TimeSpan [^long t0 ^long t1])
(comment (enc/qb 1e6 (Time. :foo 1000 nil))) ; 33.59

#?(:clj
   (do
     (defmacro ^:private mt-acc     [] `(enc/if-cljs (cljs.core/array) (LinkedList.)))
     (defmacro ^:private mt-add [mt x] `(enc/if-cljs (.push   ~mt ~x) (.add  ~(with-meta mt {:tag 'LinkedList}) ~x)))
     (defmacro ^:private mt-count [mt] `(enc/if-cljs (alength ~mt)    (.size ~(with-meta mt {:tag 'LinkedList}))))))

(comment (enc/qb 1e6 (mt-acc) (atom nil))) ; [29.14 57.76]

;;;; PStats (Profiling Stats)
;; API-level state we'll return from `profiled`: derefable, mergeable

(deftype PStats [pd ^long t1 tspans realized_]
  #?@(:clj  [clojure.lang.IDeref    (deref     [_]           @realized_)]
      :cljs [             IDeref   (-deref     [_]           @realized_)])
  #?@(:clj  [clojure.lang.IPending (isRealized [_] (realized? realized_))]
      :cljs [             IPending (-realized? [_] (realized? realized_))]))

;;;; PData (Profiling Data)
;; Implementation-level state while profiling,
;;   - id-times:  ?{<id> (<time>         ...)}
;;   - id-sstats: ?{<id> (<stats/sstats> ...)}

(declare ^:private deref-pdata)
(deftype PState [acc id-times id-sstats])
(deftype  PData [^long nmax ^long t0 pstate_]
  #?@(:clj  [clojure.lang.IDeref  (deref [this] (deref-pdata this))]
      :cljs [             IDeref (-deref [this] (deref-pdata this))]))

(defn new-pdata-local   [^long nmax] (let [t0 (enc/now-nano*)] (PData. nmax t0 (volatile! (PState. (mt-acc)   nil nil)))))
(defn new-pdata-dynamic [^long nmax] (let [t0 (enc/now-nano*)] (PData. nmax t0 (atom      (PState. (atom nil) nil nil)))))

(comment (enc/qb 1e6 (new-pdata-local 10) (new-pdata-dynamic 10))) ; [98.18 138.28]

(declare ^:private deref-pstats)
(defn- deref-pdata "PData->PStats" [^PData pd]
  ;; NB (.-acc pd) should never be mutated from this point!
  (let [t1     (enc/now-nano*)
        t0     (.-t0 pd)
        tspans (list (TimeSpan. t0 t1))]

    (PStats. pd t1 tspans (delay (deref-pstats pd t1 tspans)))))

(comment (enc/qb 1e6 @(new-pdata-local 10))) ; 245.08

(def ^:dynamic *pdata* "nnil iff dynamic profiling active" nil)

#?(:clj
   (let [;; Stack to support `profile/d` nesting
         ^ThreadLocal stack-proxy (proxy [ThreadLocal] [] (initialValue [] (Stack.)))
         ^ThreadLocal pdata-proxy (proxy [ThreadLocal] [])]

     (defn pdata-local-get [] (.get pdata-proxy)) ; => nnil iff thread-local profiling active
     (defn pdata-local-pop []
       (let [^Stack stack (.get stack-proxy)]
         (if-let [stashed (when-not (.empty stack) (.pop stack))]
           (do (.set pdata-proxy stashed) stashed)
           (do (.set pdata-proxy nil)     nil))))

     (defn pdata-local-push [v]
       (let [^Stack stack  (.get stack-proxy)]
         (if-let [to-stash (.get pdata-proxy)]
           (do (.push stack to-stash) (.set pdata-proxy v) v)
           (do                        (.set pdata-proxy v) v)))))

   :cljs ; Note single-threaded platform
   (let [stack #js [] ; To support `profile/d` nesting
         pdata_ (volatile! false)]

     (defn pdata-local-get [] @pdata_) ; => nnil iff thread-local profiling active
     (defn pdata-local-pop []
       (if-let [stashed (.pop stack)]
         (vreset! pdata_ stashed)
         (vreset! pdata_ nil)))

     (defn pdata-local-push [v]
       (if-let [to-stash @pdata_]
         (do (.push stack to-stash) (vreset! pdata_ v))
         (do                        (vreset! pdata_ v))))))

(comment
  (pdata-local-push "foo")
  (pdata-local-pop)
  (enc/qb 1e6 *pdata* (pdata-local-get)) ; [63.7 48.77]
  (enc/qb 1e6  ; [507.58 74.62]
    (binding [*pdata* "foo"])
    (try (pdata-local-push "foo") (finally (pdata-local-pop))))

  (do
    (pdata-local-push "pd1")
    (pdata-local-push "pd2")
    [(let [pd (pdata-local-get)] (pdata-local-pop) pd)
     (let [pd (pdata-local-get)] (pdata-local-pop) pd)]))

;;;; TimeSpan utils

(deftype ElapsedTimeAcc [^long tsum ^long max-t1])
(let [sort-tspans (fn [tspans] (sort-by (fn [^TimeSpan tspan] (.-t0 tspan)) tspans))]
  (defn- tspans->tsum
    "Returns `tsum` (elapsed time) given collection of `TimeSpan`s.
    Based on https://codereview.stackexchange.com/a/126927."
    ^long [tspans]
    (if (empty? tspans)
      0
      (let [sorted-tspans (sort-tspans tspans)] ; O(n.logn)
        (.-tsum ^ElapsedTimeAcc
          (reduce
            (fn [^ElapsedTimeAcc acc ^TimeSpan tspan]
              (let [t1     (.-t1     tspan)
                    max-t1 (.-max-t1 acc)]
                (if (> t1 max-t1)
                  (let [t0   (.-t0   tspan)
                        tsum (.-tsum acc)]
                    (ElapsedTimeAcc. (+ tsum (- t1 (Math/max t0 max-t1))) t1))
                  acc)))
            (ElapsedTimeAcc. 0 0)
            sorted-tspans))))))

(comment
  (tspans->tsum nil)
  (tspans->tsum [])
  (tspans->tsum [(TimeSpan. 1   3) (TimeSpan. 3 6)])
  (tspans->tsum [(TimeSpan. 3   6) (TimeSpan. 1 3)])
  (tspans->tsum [(TimeSpan. 1  10) (TimeSpan. 3 6)])
  (enc/qb 1e6
    (tspans->tsum
      [(TimeSpan. 10 14)
       (TimeSpan.  4 18)
       (TimeSpan. 19 20)
       (TimeSpan. 19 20)
       (TimeSpan. 13 20)])))

(defn- fast-into [c0 c1] (if (> (count c0) (count c1)) (into c0 c1) (into c1 c0)))
(comment (fast-into nil nil))

(defn- merge-tspans [^long nmax ^long t1 tspans0 tspans1]
  (let [tspans2 (fast-into tspans0 tspans1)]
    (if (> (count tspans2) nmax) ; Compact, may lose some accuracy
      (let [tsum (tspans->tsum tspans2)]
        (list (TimeSpan. (- t1 tsum) t1)))
      tspans2)))

(comment
  (merge-tspans 2 50
    (list (TimeSpan. 1 10) (TimeSpan. 5  20))
    (list (TimeSpan. 1 10) (TimeSpan. 20 50))))

;;;;

(defn- times-into-id-times
  "NB treats `from-times` as read-only (may be mutable `acc`)!"
  [to-id-times from-times]
  (not-empty
    (if-let [from-times (enc/force-ref from-times)]
      (persistent!
        (reduce
          (fn [m ^Time in]
            (let [t   (.-t             in)
                  id  (.-id            in)
                  loc (.-location-info in)
                  id* [id loc]]

              ;; We'll use [id loc] as a pseudo id from here on in
              ;; order to track location info. We'll then reverse
              ;; the wrapping on final `deref-pstats`.
              (assoc! m id* (conj (get m id*) t))))

          (transient (or to-id-times {}))
          from-times))

      to-id-times)))

(comment
  (times-into-id-times nil nil)
  (times-into-id-times {}  nil)
  (let [mt (mt-acc)]
    (mt-add mt (Time. :foo 2 nil))
    (times-into-id-times {:foo '(1)} mt)))

(defn- deref-pstats
  "PStats->{:clock _ :stats {<id> <stats/sstats-map>}} (API output)"
  [^PData pd ^long t1 tspans]
  (let [t0      (.-t0      pd)
        pstate_ (.-pstate_ pd)
        ^PState pstate (enc/force-ref pstate_)
        id-times  (.-id-times  pstate)
        id-sstats (.-id-sstats pstate)
        id-times  (times-into-id-times id-times (.-acc pstate))

        public-stats-output ; {<id*> <stats/sstats-map>}
        (when id-times
          (let [;;; Disposable internal state
                id*-sstats_ (volatile! (transient {})) ; {<id*>  <sstats>}
                id*-loc_    (volatile! (transient {})) ; {<id*> <map-or-set>}
                ]

            (persistent!
              (reduce-kv
                (fn [m id times]
                  (let [[id* loc] id ; Reverse [id loc] wrapping done at `times-into-id-times`

                        sstats<times  (stats/summary-stats times)
                        sstats-merged (reduce stats/summary-stats-merge sstats<times
                                        (get id-sstats id))

                        ;; Note that a single id* may have >1 locs and so >1
                        ;; [id loc] entries in id-times. While uncommon, this is
                        ;; is sensible and supported.

                        ;; Final sstats merged from all locations for given id*
                        new-id*-sstats
                        (if-let [old (get @id*-sstats_ id*)]
                          (stats/summary-stats-merge old sstats-merged)
                          (do                            sstats-merged))

                        ;; Location (map or set) for given id*
                        new-id*-loc
                        (if-let [old (get @id*-loc_ id*)]
                          (if (set? old)
                            (conj old loc)
                            (if (= old loc) old #{old loc}))
                          loc)

                        new-id*-entry (assoc @new-id*-sstats :loc new-id*-loc)]

                    (vswap! id*-sstats_ assoc!   id* new-id*-sstats)
                    (vswap! id*-loc_    assoc!   id* new-id*-loc)
                    (do                (assoc! m id* new-id*-entry))))

                (transient {})
                id-times))))]

    {:clock {:t0 t0 :t1 t1 :total (tspans->tsum tspans)}
     :stats public-stats-output}))

(comment @@(new-pdata-local 10))

(defn- merge-sstats-when-needed [^long nmax sstats]
  (if (<= (count sstats) nmax)
    (do                                     sstats)
    (list (reduce stats/summary-stats-merge sstats))))

(defn merge-pstats "Compacting merge"
  ([     ps0 ps1] (merge-pstats nil ps0 ps1))
  ([nmax ps0 ps1]
   (if ps0
     (if ps1
       (let [^PStats ps0       ps0
             ^PStats ps1       ps1
             ^PData  pd0 (.-pd ps0)
             ^PData  pd1 (.-pd ps1)

             nmax (long (or nmax (.-nmax pd0)))
             pd0-t0 (.-t0 pd0)
             ps0-t1 (.-t1 ps0)
             pd1-t0 (.-t0 pd1)
             ps1-t1 (.-t1 ps1)

             pd2-t0  (if (< pd0-t0 pd1-t0) pd0-t0 pd1-t0)
             ps2-t1  (if (> ps0-t1 ps1-t1) ps0-t1 ps1-t1)
             tspans2 (merge-tspans nmax ps2-t1 (.-tspans ps0) (.-tspans ps1))

             ^PState pd0-pstate (enc/force-ref (.-pstate_ pd0))
             ^PState pd1-pstate (enc/force-ref (.-pstate_ pd1))

             pd0-id-times  (times-into-id-times (.-id-times pd0-pstate) (.-acc pd0-pstate))
             pd1-id-times  (times-into-id-times (.-id-times pd1-pstate) (.-acc pd1-pstate))
             pd0-id-sstats (.-id-sstats pd0-pstate)
             pd1-id-sstats (.-id-sstats pd1-pstate)

             ;; All ids in pd0 or pd1
             pd2-ids (keys (conj (or pd0-id-times {}) pd1-id-times))

             ;; Merge pd1 into pd0 to get pd2
             [pd2-id-times pd2-id-sstats]
             (reduce
               (fn [[pd2-id-times pd2-id-sstats] id]
                 (let [pd0-times  (get pd0-id-times  id)
                       pd0-sstats (get pd0-id-sstats id)
                       pd1-times  (get pd1-id-times  id)
                       pd1-sstats (get pd1-id-sstats id)

                       pd2-times  (fast-into pd0-times  pd1-times)
                       pd2-sstats (fast-into pd0-sstats pd1-sstats)]

                   (if (<= (count pd2-times) nmax) ; Common case
                     [(assoc pd2-id-times  id pd2-times)
                      (assoc pd2-id-sstats id pd2-sstats)]

                     ;; Times need compaction
                     (let [sstats<times (stats/summary-stats pd2-times)]
                       [(assoc pd2-id-times  id nil)
                        (assoc pd2-id-sstats id
                          (merge-sstats-when-needed nmax
                            (conj pd2-sstats sstats<times)))]))))

               [pd0-id-times pd0-id-sstats]
               pd2-ids)

             pd2 (PData. nmax pd2-t0 (PState. nil pd2-id-times pd2-id-sstats))]
         (PStats. pd2 ps2-t1 tspans2 (delay (deref-pstats pd2 ps2-t1 tspans2))))

       ps0)
     ps1)))

;;;; Time capture

#?(:clj
   (defmacro ^:private atom? [x]
     `(enc/if-cljs
        (instance?    cljs.core.Atom ~x)
        (instance? clojure.lang.Atom ~x))))

(declare ^:private compact-pstate)
(defn capture-time! [^PData pd id ns-elapsed location-info]
  (let [nmax    (.-nmax    pd)
        pstate_ (.-pstate_ pd)
        ^PState pstate @pstate_
        acc (.-acc pstate)]

    (if (atom? acc)

      ;; Dynamic profiling
      (let [new-time (Time. id ns-elapsed location-info)
            ?pulled-times
            (loop []
              (let [old-times @acc
                    new-times (conj old-times new-time)]
                (if (<= (count new-times) nmax)
                  (if (compare-and-set! acc old-times new-times) nil (recur))
                  (if (compare-and-set! acc old-times nil) new-times (recur)))))]

        (when-let [times ?pulled-times] ; Do compaction, rare
          (let [t0 (enc/now-nano*)]
            ;; Contention against `pstate_` unlikely since we just drained `acc`
            (swap! pstate_ (fn [pstate] (compact-pstate pstate times nmax true)))
            (recur pd :tufte/compaction (- (enc/now-nano*) t0) nil))))

      (do ; Common case: thread-local profiling
        (mt-add acc (Time. id ns-elapsed location-info))

        (when (> (mt-count acc) nmax) ; Do compaction, rare
          (let [t0 (enc/now-nano*)]
            (vreset! pstate_ (compact-pstate pstate acc nmax false))
            (recur pd :tufte/compaction (- (enc/now-nano*) t0) nil)))))))

(defn- compact-pstate [^PState pstate pulled-times ^long nmax dynamic?]
  ;; Note that compaction expense doesn't distort p times unless there's
  ;; p nesting (where outer p time includes inner p's capture time).
  (let [id-times  (.-id-times  pstate)
        id-sstats (.-id-sstats pstate)
        id-times  (times-into-id-times id-times pulled-times)

        [id-times id-sstats]
        (reduce-kv
          (fn [acc id times]
            (if (<= (count times) nmax)
              acc
              (let [[id-times id-sstats] acc
                    sstats<times (stats/summary-stats times)]
                [(assoc id-times  id nil)
                 (assoc id-sstats id
                   (merge-sstats-when-needed nmax
                     (conj (get id-sstats id) sstats<times)))])))

          [id-times id-sstats]
          id-times)

        new-acc (if dynamic? (.-acc pstate) (mt-acc))]

    (PState. new-acc id-times id-sstats)))

(comment
  (try
    (pdata-local-push (new-pdata-local 1e7))
    (enc/qb 1e6 (capture-time! (pdata-local-get) :foo 1 nil))
    (finally (pdata-local-pop)))) ; 98.35

;;;; Output handlers

(enc/defonce handlers_ "{<hid> <handler-fn>}" (atom nil))

#?(:clj
   (enc/defonce ^:private ^ArrayBlockingQueue handler-queue
     "While user handlers should ideally be non-blocking, we'll use a queue
     here to be safe + make sure we never tie up the execution thread."
     (ArrayBlockingQueue. 1024)))

(defn- handle-blocking! [m]
  (enc/run-kv!
    (fn [id f]
      (truss/try* (f m)
        (catch :default e
          (truss/catching ; Esp. nb for Cljs
            (println (str "WARNING: Uncaught Tufte `" id "` handler error\n" e))))))
    @handlers_))

#?(:clj  (declare ^:private handler-thread_))
#?(:cljs (defn handle! [m] (handle-blocking! m) nil))
#?(:clj  (defn handle! [m] (.offer handler-queue m) @handler-thread_ nil))
#?(:clj
   (defonce ^:private handler-thread_
     (delay
       (let [f (fn []
                 (loop []
                   (let [m (.take handler-queue)]
                     ;; Note: just drop if no registered handlers
                     (handle-blocking! m)
                     (recur))))]
         (doto (Thread. f)
           (.setDaemon true)
           (.start))))))

;;;; Formatting

(defn- perc [n d] (str (Math/round (* (/ (double n) (double d)) 100.0)) "%"))
(comment [(perc 1 1) (perc 1 100) (perc 12 44)])

(def ^:dynamic *fmt-opts* {:decimal-separator ".", :thousands-separator ","})
(defn- fmt-num [precision n]
  ;; Impln is inefficient but sufficient, and consistent between clj/s
  (let [n (enc/roundn precision n)
        neg?       (neg?     n)
        n-abs      (Math/abs n)
        n-int-part (long n-abs)
        fmt-opts   *fmt-opts*]
    (str
      (when neg? "-")
      (->>
        (str n-int-part)
        (reverse)
        (partition 3 3 "")
        (mapv str/join)
        (str/join (get fmt-opts :thousands-separator))
        (str/reverse))

      (when-let [n-dec-part (and (pos? (long precision)) (- n-abs n-int-part))]
        (str (get fmt-opts :decimal-separator)
          (enc/substr (str n-dec-part "000000")
            :by-len 2 precision))))))

(comment
  (fmt-num 0 123123123.5555) ; "123,123,124"
  (fmt-num 2 123123123.5555) ; "123,123,123.56"
  (fmt-num 2 123123123)      ; "123,123,123.00"
  (fmt-num 3 123)            ; "123"
  )

(defn- fmt-nsecs  [nanosecs]
  (let [ns (double nanosecs)]
    (cond
      (>= ns 6e10) (str (fmt-num 2 (/ ns 6e10)) "m")
      (>= ns 1e9)  (str (fmt-num 2 (/ ns 1e9))  "s")
      (>= ns 1e6)  (str (fmt-num 0 (/ ns 1e6))  "ms")
      (>= ns 1e3)  (str (fmt-num 0 (/ ns 1e3))  "μs")
      :else        (str (fmt-num 0    ns)       "ns"))))

(comment (fmt-nsecs 1e3))

(def     all-format-columns [:n :min   :p25 :p50   :p75 :p90 :p95 :p99 :max :mean :mad :clock :sum])
(def default-format-columns [:n :min #_:p25 :p50 #_:p75 :p90 :p95 :p99 :max :mean :mad :clock :sum])

(let [migrate        {:n-calls :n, :total :sum} ; For back-compatibility
      format-column? (set all-format-columns)
      format-column  (fn [column] (truss/have format-column? (get migrate column column)))]

  (defn- format-columns [columns]
    (enc/cond
      (identical? columns default-format-columns) default-format-columns
      (identical? columns     all-format-columns)     all-format-columns
      :else (mapv format-column columns))))

(comment (enc/qb 1e6 (format-columns [:min :n-calls :total])))

(def default-format-id-fn (fn [id] (str id)))

;; id-sstats* => {<id> <sstats>} or {<id> <sstats-map>}

(defn get-max-id-width
  [id-sstats*
   {:keys [format-id-fn]
    :or   {format-id-fn default-format-id-fn}}]

  (when id-sstats*
    (reduce-kv
      (fn [^long acc id _sstats*]
        (let [c (count (format-id-fn id))]
          (if (> c acc) c acc)))
      9 ; (count "Accounted")
      id-sstats*)))

(defn format-pstats
  "Given {<id> <sstats>} or {<id> <sstats-map>} pstats, returns a formatted
  table string. Assumes nanosecond clock, and stats based on profiling id'd
  nanosecond times."
  [clock-total id-sstats*
   {:keys [columns sort-fn format-id-fn max-id-width] :as opts
    :or   {columns      default-format-columns
           sort-fn      (fn [ss] (get (enc/force-ref ss) :sum))
           format-id-fn default-format-id-fn}}]

  (when id-sstats*
    (let [columns (format-columns columns)
          clock-total (long clock-total)
          ^long accounted-total
          (reduce-kv
            (fn [^long acc _id ss]
              (+ acc (long (get (enc/force-ref ss) :sum))))
            0 id-sstats*)

          sorted-ids
          (sort-by
            (fn [id] (sort-fn (get id-sstats* id)))
            enc/rcompare
            (keys id-sstats*))

          ^long max-id-width
          (or
            max-id-width
            (get-max-id-width id-sstats* opts))

          column->pattern
          {:id      {:heading "pId" :min-width max-id-width :align :left}
           :n       {:heading "nCalls"}
           :min     {:heading "Min"}
           :p25     {:heading "25% ≤"}
           :p50     {:heading "50% ≤"}
           :p75     {:heading "75% ≤"}
           :p90     {:heading "90% ≤"}
           :p95     {:heading "95% ≤"}
           :p99     {:heading "99% ≤"}
           :max     {:heading "Max"}
           :mean    {:heading "Mean"}
           :mad     {:heading "MAD"   :min-width 5}
           :sum     {:heading "Total" :min-width 6}
           :clock   {:heading "Clock"}}

          sb (enc/str-builder "")

          append-col
          (fn [column s]
            (let [{:keys [min-width align]
                   :or   {min-width 10 align :right}}
                  (get column->pattern column)]

              (enc/sb-append sb
                (enc/format
                  (str "%" (case align :left "-" :right "") min-width "s")
                  s))))]

      ; Write header rows
      (doseq [column (into [:id] columns)]
        (when-not (= :id column)
          (enc/sb-append sb " "))
        (append-col column (get-in column->pattern [column :heading])))

      (enc/sb-append sb "\n\n")

      ; Write id rows
      (doseq [id sorted-ids]
        (let [ssm (enc/force-ref (get id-sstats* id))
              {:keys [n sum mean mad]} ssm]

          (append-col :id (format-id-fn id))
          (doseq [column columns]
            (enc/sb-append sb " ")
            (case column
              :n     (append-col column (fmt-num 0 n))
              :mean  (append-col column (fmt-nsecs mean))
              :mad   (append-col column (str "±" (perc mad mean)))
              :sum   (append-col column (perc sum clock-total))
              :clock (append-col column (fmt-nsecs sum))
              (do    (append-col column (fmt-nsecs (get ssm column))))))

          (enc/sb-append sb "\n")))

      ; Write accounted row
      (enc/sb-append sb "\n")
      (append-col :id "Accounted")
      (doseq [column columns]
        (enc/sb-append sb " ")
        (case column
          :sum   (append-col column (perc accounted-total clock-total))
          :clock (append-col column (fmt-nsecs accounted-total))
          (do    (append-col column ""))))

      ; Write clock row
      (enc/sb-append sb "\n")
      (append-col :id "Clock")
      (doseq [column columns]
        (enc/sb-append sb " ")
        (case column
          :sum   (append-col column "100%")
          :clock (append-col column (fmt-nsecs clock-total))
          (do    (append-col column ""))))

      (enc/sb-append sb "\n")
      (str sb))))

(comment
  (defn rand-vs [n & [max]]
    (take n (repeatedly (partial rand-int (or max Integer/MAX_VALUE)))))

  (println
    (format-pstats (* 1e6 30)
      {:foo (summary-stats (rand-vs 1e4 20))
       :bar (summary-stats (rand-vs 1e2 50))
       :baz (summary-stats (rand-vs 1e5 30))}
      {}) "\n"))

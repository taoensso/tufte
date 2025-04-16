(ns ^:no-doc taoensso.tufte.impl
  "Private implementation details.
  Ultimately returns `PStats` to be merged or derefed.

  Flow:
    1. Init pdata              ; On  thread
    2. Capture times -> pdata  ; On  thread
    3. Deref pdata -> pstats   ; On  thread
    4. ?Merge pstats           ; Off thread, on demand (deferred cost)
    5. ?Realize ?merged pstats ; Off thread, on demand (deferred cost)"

  (:require
   [clojure.string  :as str]
   [taoensso.truss  :as truss]
   [taoensso.encore :as enc]
   [taoensso.encore.stats   :as stats]
   [taoensso.encore.signals :as sigs])

  #?(:cljs
     (:require-macros
      [taoensso.tufte.impl :refer [mt-acc mt-add! mt-count]])))

(comment
  (remove-ns (symbol (str *ns*)))
  (:api (enc/interns-overview)))

;;;; Basic types

(do
  (deftype Time     [id ^long runtime location]) ; Runtime entry
  (deftype TimeSpan [^long t0 ^long t1])         ; Accumulated during PStats merge
  (deftype TimeAcc  [^long tsum ^long tmax])     ; Used during reductions, etc.
  (deftype IdState  [id-times id-sstats])        ; Used during reductions, etc.
  )

;;;; Mutable accumulators (for local-only profiling)

#?(:clj
   (do
     (defmacro ^:private mt-acc   [    ] (if (:ns &env) `(cljs.core/array)                             `(java.util.LinkedList.)))
     (defmacro ^:private mt-add!  [mt x] (if (:ns &env) `(.push   ~mt ~x)  `(.add  ~(with-meta mt {:tag 'java.util.LinkedList}) ~x)))
     (defmacro ^:private mt-count [mt  ] (if (:ns &env) `(alength ~mt)     `(.size ~(with-meta mt {:tag 'java.util.LinkedList}))))))

;;;; Main types

(defrecord RealizedPStats [^long t0 ^long t1 ^long clock-total stats clock])
(deftype           PStats ; Profiling Stats
  [^long nmax ^long t0 ^long t1 acc id-times id-sstats tspans realized_]
  ;; - Public, mergeable, immutable version of `PData`
  ;; - Deref to get `RealizedPStats`
  ;; - All fields but `realized_` used to support merging

  #?@(:clj  [clojure.lang.IDeref  (deref [this] @realized_)]
      :cljs [             IDeref (-deref [this] @realized_)])

  #?@(:clj  [clojure.lang.IPending (isRealized [_] (realized? realized_))]
      :cljs [             IPending (-realized? [_] (realized? realized_))])

  Object
  (toString [x]
    (enc/str-impl x "taoensso.tufte.PStats"
      {:realized? (realized? realized_)})))

(enc/def-print-impl [x PStats] (str "#" x))

(declare
  ^:private ^IdState compact-id-state
  ^:private          deref-pstats)

#?(:clj
   (let [cache_ (java.util.concurrent.ConcurrentHashMap.)]
     ;; Tons of location duplicates in `Time`s, intern to save memory
     (defn- interned [x] (when x (or (.get cache_ x) (.putIfAbsent cache_ x x) x)))))

(comment (enc/qb 1e6 (interned "foo"))) ; 40.88

(deftype PData ; Profiling Data (private)
  [dynamic? ^long nmax ^long t0
   #?(:clj ^:volatile-mutable acc         :cljs ^:mutable acc)
   #?(:clj ^:volatile-mutable id-times    :cljs ^:mutable id-times)
   #?(:clj ^:volatile-mutable id-sstats   :cljs ^:mutable id-sstats)
   #?(:clj ^:volatile-mutable compacting? :cljs ^:mutable compacting?)]

  ;; `acc`       - mutable accumulator or (latom ())
  ;; `id-times`  - ?{<id> [<Time>               ...]}
  ;; `id-sstats` - ?{<id> [<stats/SummaryStats> ...]}
  ;;
  ;; - Invoke [id nsecs location] to capture Time
  ;; - Invoke [] to get immutable `PStats`

  #?@(:clj  [clojure.lang.IDeref (deref  [this] (this))]
      :cljs [             IDeref (-deref [this] (this))])

  #?(:clj clojure.lang.IFn :cljs IFn)

  ;; Captures time, compacting as necessary
  (#?(:clj invoke :cljs -invoke) [this id nsecs-elapsed location]
    (when-let [acc-snap acc]
      (let [new-time (Time. id ^long nsecs-elapsed #?(:clj (interned location) :cljs location))]
        (enc/when-let
          [need-compaction?
           (if dynamic?
             (> (count (acc-snap #(conj % new-time))) nmax)
             (do (mt-add! acc-snap new-time) (> (mt-count acc-snap) nmax)))

           pulled-times
           (if-not dynamic?
             acc-snap
             (and
               (not compacting?) ; Irrelevant for Cljs
               (loop []
                 (let [times (acc-snap)]
                   (when (and (not compacting?) (> (count times) nmax))
                     (if (enc/-cas!? acc-snap times nil) times (recur)))))))]

          (set! compacting? true)
          (let [t0        (enc/now-nano*)
                compacted (compact-id-state nmax id-times id-sstats pulled-times)]
            ;; Safe to `set!` here even when `dynamic?` since we're protected by
            ;; CAS against `acc-snap`. For performance reasons we DO tolerate a
            ;; theoretical risk of losing times under high contention when `nmax`
            ;; is ridiculously, unrealistically small.
            (set! id-times  (.-id-times  compacted))
            (set! id-sstats (.-id-sstats compacted))
            (when-not dynamic? (set! acc (mt-acc)))
            (this :tufte/compaction (- (enc/now-nano*) t0) nil)
            (set! compacting? false))))
      true))

  ;; PData->PStats
  (#?(:clj invoke :cljs -invoke)
    [_]
    (when-let [acc-snap acc]
      (set! acc nil)
      (let [t1     (enc/now-nano*)
            tspans [(TimeSpan. t0 t1)]]
        (PStats.               nmax t0 t1 acc-snap id-times id-sstats tspans
          (delay (deref-pstats nmax t0 t1 acc-snap id-times id-sstats tspans)))))))

;;;;

(def ^:dynamic *pdata* "nnil iff dynamic profiling active" nil)
(defn new-pdata-local   ^PData [^long nmax] (let [t0 (enc/now-nano*)] (PData. false nmax t0 (mt-acc)        nil nil false)))
(defn new-pdata-dynamic ^PData [^long nmax] (let [t0 (enc/now-nano*)] (PData. true  nmax t0 (enc/latom nil) nil nil false)))

(comment
  (enc/qb 1e6 (new-pdata-local 10) (new-pdata-dynamic 10)) ; [49.61 50.54]

  (let [pd (new-pdata-local   10)] (pd :id1 100 nil) @(pd))
  (let [pd (new-pdata-dynamic 10)] (pd :id1 100 nil) @(pd))

  (let [pdl (new-pdata-local   5e6)
        pdd (new-pdata-dynamic 5e6)]

    (enc/qb 1e6 ; [44.19 53.05]
      (pdl :id 100 nil)
      (pdd :id 100 nil))))

;;;; Local PData utils

#?(:clj
   (let [;; Stack to support `profile/d` nesting
         ^ThreadLocal stack-proxy (proxy [ThreadLocal] [] (initialValue [] (java.util.Stack.)))
         ^ThreadLocal pdata-proxy (proxy [ThreadLocal] [])]

     (defn pdata-local-get "nnil iff thread-local profiling active" [] (.get pdata-proxy))
     (defn pdata-local-pop []
       (let [^java.util.Stack stack (.get stack-proxy)]
         (if-let [stashed (when-not (.empty stack) (.pop stack))]
           (do (.set pdata-proxy stashed) stashed)
           (do (.set pdata-proxy nil)     nil))))

     (defn pdata-local-push [v]
       (let [^java.util.Stack stack  (.get stack-proxy)]
         (if-let [to-stash (.get pdata-proxy)]
           (do (.push stack to-stash) (.set pdata-proxy v) v)
           (do                        (.set pdata-proxy v) v)))))

   :cljs ; Single-threaded
   (let [pdata_ (volatile! nil)
         stack #js []]

     (defn pdata-local-get "nnil iff thread-local profiling active" [] @pdata_)
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
  (enc/qb 1e6 *pdata* (pdata-local-get)) ; [22.03 41.5]
  (enc/qb 1e6  ; [231.22 78.72]
    (binding [*pdata* "foo"])
    (try
      (do      (pdata-local-push "foo"))
      (finally (pdata-local-pop))))

  (do
    (pdata-local-push "pd1")
    (pdata-local-push "pd2")
    [(let [pd (pdata-local-get)] (pdata-local-pop) pd)
     (let [pd (pdata-local-get)] (pdata-local-pop) pd)])

  (enc/qb 1e4 ; 130.25
    (try
      (pdata-local-push (new-pdata-local 1e7))
      (dotimes [_ 1e3] ((pdata-local-get) :foo 1 nil))
      (finally          (pdata-local-pop)))))

;;;; Compaction, etc.

(defn- times-into-id-times
  "NB treats `from-times` as read-only (may be mutable `acc`)!"
  [to-id-times from-times]
  (not-empty
    (if-let [from-times (enc/force-ref from-times)]
      (persistent!
        (reduce
          (fn [m ^Time in]
            (let [rt       (.-runtime  in)
                  id       (.-id       in)
                  location (.-location in)
                  id*      [id location]]

              ;; We'll use [id location] as a pseudo id to track location info.
              ;; We'll then reverse the wrapping on final `deref-pstats`.
              (assoc! m id* (conj (get m id*) rt))))

          (transient (or to-id-times {}))
          from-times))

      to-id-times)))

(comment
  (times-into-id-times nil nil)
  (times-into-id-times {}  nil)
  (let [mt (mt-acc)]
    (mt-add! mt (Time. :foo 2 nil))
    (times-into-id-times {:foo '(1)} mt)))

(defn- merge-sstats-when-needed [^long nmax sstats]
  (if (<= (count sstats) nmax)
    (do                                     sstats)
    (list (reduce stats/summary-stats-merge sstats))))

(defn- compact-id-state
  ;; Note that compaction cost doesn't distort p times unless there's
  ;; p nesting (where outer p time includes inner p's capture time).
  ^IdState [nmax id-times id-sstats pulled-times]
  (let [id-times (times-into-id-times id-times pulled-times)]
    (reduce-kv
      (fn [^IdState acc id times]
        (if (<= (count times) ^long nmax)
          acc
          (let [id-times     (.-id-times  acc)
                id-sstats    (.-id-sstats acc)
                sstats<times (stats/summary-stats times)]

            (IdState.
              (assoc id-times  id nil)
              (assoc id-sstats id
                (merge-sstats-when-needed nmax
                  (conj (get id-sstats id) sstats<times)))))))

      (IdState. id-times id-sstats)
      (do       id-times))))

(defn- fast-into [c0 c1] (if (> (count c0) (count c1)) (into c0 c1) (into c1 c0)))
(comment (fast-into nil nil))

(let [tspans->tacc
      (fn [tspans]
        ;; Ref. <https://codereview.stackexchange.com/a/126927>
        (let [sorted-tspans (sort-by (fn [^TimeSpan tspan] (.-t0 tspan)) tspans)] ; O(n.logn)
          (reduce ; O(n)
            (fn [^TimeAcc acc ^TimeSpan tspan]
              (let [t1   (.-t1   tspan)
                    tmax (.-tmax acc)]
                (if (> t1 tmax)
                  (let [t0   (.-t0   tspan)
                        tsum (.-tsum acc)]
                    (TimeAcc. (+ tsum (- t1 (Math/max t0 tmax))) t1))
                  acc)))
            (TimeAcc. 0 0)
            sorted-tspans)))]

  (defn- tspans->tsum
    "Used for calculating total clock time when merging `PStats`."
    ^long [tspans]
    (if (= (count tspans) 1) ; Common case
      (let [[^TimeSpan tspan1] tspans] (- (.-t1 tspan1) (.-t0 tspan1)))
      (.-tsum ^TimeAcc (tspans->tacc tspans))))

  (defn- merge-tspans [^long nmax tspans0 tspans1]
    (let [tspans2 (fast-into tspans0 tspans1)]
      (if (<= (count tspans2) nmax)
        tspans2
        ;; Compact (loses overlap info for future merges!)
        (let [^TimeAcc tacc (tspans->tacc tspans2)
              tsum (.-tsum tacc)
              tmax (.-tmax tacc)]
          [(TimeSpan. (- tmax tsum) tmax)])))))

(comment
  ;; [15 20 20]
  [(tspans->tsum (merge-tspans 1e3 [(TimeSpan. 0 10)] [(TimeSpan. 15 20)]))
   (tspans->tsum (merge-tspans 1e3 [(TimeSpan. 0 20)] [(TimeSpan. 10 15)]))
   (tspans->tsum (merge-tspans 1e3 [(TimeSpan. 0 10)] [(TimeSpan.  5 20)]))])

;;;; PStats realization

(defn- deref-pstats
  ^RealizedPStats [nmax t0 t1 acc id-times id-sstats tspans]
  (let [clock-total (tspans->tsum tspans)
        id-times    (times-into-id-times id-times acc)
        public-stats-map ; {<id*> <stats/sstats-map>}
        (when id-times
          (let [;;; Disposable internal state
                id*-sstats_   (volatile! (transient {})) ; {<id*> <sstats>}
                id*-location_ (volatile! (transient {})) ; {<id*> <map-or-set>}
                ]

            (persistent!
              (reduce-kv
                (fn [m id times]
                  (let [[id* location] id ; Reverse [id location] wrapping done at `times-into-id-times`

                        sstats<times  (stats/summary-stats times)
                        sstats-merged (reduce stats/summary-stats-merge sstats<times
                                        (get id-sstats id))

                        ;; Note that a single id* may have >1 locations and so >1
                        ;; [id location] entries in id-times. While uncommon, this
                        ;; is sensible and supported.

                        ;; Final sstats merged from all locations for given id*
                        new-id*-sstats
                        (if-let [old (get @id*-sstats_ id*)]
                          (stats/summary-stats-merge old sstats-merged)
                          (do                            sstats-merged))

                        ;; Location (map or set) for given id*
                        new-id*-location
                        (if-let [old (get @id*-location_ id*)]
                          (if (set? old)
                            (conj  old location)
                            (if (= old location) old #{old location}))
                          location)

                        new-id*-entry (assoc @new-id*-sstats :loc new-id*-location)]

                    (vswap! id*-sstats_   assoc!   id* new-id*-sstats)
                    (vswap! id*-location_ assoc!   id* new-id*-location)
                    (do                  (assoc! m id* new-id*-entry))))

                (transient {})
                id-times))))]

    ;; {:clock _ :stats {<id> <stats/sstats-map>}}
    (RealizedPStats. t0 t1  clock-total public-stats-map
      {:t0 t0 :t1 t1 :total clock-total})))

;;;; PStats utils

(defn ^:public merge-pstats
  "Merges given `pstats`, compacting as necessary.
  Merged statistics are lossless unless data to merge are very large."
  ([            ] nil)
  ([     ps0    ] ps0)
  ([     ps0 ps1] (merge-pstats nil ps0 ps1))
  ([nmax ps0 ps1]
   (if ps0
     (if ps1
       (let [^PStats ps0 ps0
             ^PStats ps1 ps1

             nmax   (long (or nmax (.-nmax ps0) (.-nmax ps1)))
             ps0-t0 (.-t0 ps0)
             ps0-t1 (.-t1 ps0)
             ps1-t0 (.-t0 ps1)
             ps1-t1 (.-t1 ps1)

             ps2-t0  (Math/min ps0-t0 ps1-t0)
             ps2-t1  (Math/max ps0-t1 ps1-t1)
             tspans2 (merge-tspans nmax (.-tspans ps0) (.-tspans ps1))

             ps0-id-times  (times-into-id-times (.-id-times  ps0) (.-acc ps0))
             ps1-id-times  (times-into-id-times (.-id-times  ps1) (.-acc ps1))
             ps0-id-sstats                      (.-id-sstats ps0)
             ps1-id-sstats                      (.-id-sstats ps1)

             ;; All ids in ps0 or ps1
             ps2-ids (keys (conj (or ps0-id-times {}) ps1-id-times))

             ;; ps2 = ps0 <- ps1
             ^IdState ps2-id-state
             (reduce
               (fn [^IdState acc id]
                 (let [ps2-id-times  (.-id-times  acc)
                       ps2-id-sstats (.-id-sstats acc)

                       ps0-times  (get ps0-id-times  id)
                       ps0-sstats (get ps0-id-sstats id)
                       ps1-times  (get ps1-id-times  id)
                       ps1-sstats (get ps1-id-sstats id)

                       ps2-times  (fast-into ps0-times  ps1-times)
                       ps2-sstats (fast-into ps0-sstats ps1-sstats)]

                   (if (<= (count ps2-times) nmax) ; Common case
                     (IdState.
                       (assoc ps2-id-times  id ps2-times)
                       (assoc ps2-id-sstats id ps2-sstats))

                     ;; Times need compaction
                     (let [sstats<times (stats/summary-stats ps2-times)]
                       (IdState.
                         (assoc ps2-id-times  id nil)
                         (assoc ps2-id-sstats id
                           (merge-sstats-when-needed nmax
                             (conj ps2-sstats sstats<times))))))))

               (IdState. ps0-id-times ps0-id-sstats)
               ps2-ids)

             ps2-id-times  (.-id-times  ps2-id-state)
             ps2-id-sstats (.-id-sstats ps2-id-state)]

         (PStats.               nmax ps2-t0 ps2-t1 nil ps2-id-times ps2-id-sstats tspans2
           (delay (deref-pstats nmax ps2-t0 ps2-t1 nil ps2-id-times ps2-id-sstats tspans2))))
       ps0)
     ps1)))

;;;; Formatting

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

(comment (enc/qb 1e6 (format-columns [:min :n-calls :total]))) ; 152.9

;; id-sstats* => {<id> <sstats>} or {<id> <sstats-map>}
(defn get-max-id-width
  [id-sstats*
   {:keys [format-id-fn]
    :or   {format-id-fn (fn [id] (str id))}}]

  (when id-sstats*
    (reduce-kv
      (fn [^long acc id _sstats*]
        (let [c (count (format-id-fn id))]
          (if (> c acc) c acc)))
      9 ; (count "Accounted")
      id-sstats*)))

(def ^:private format-n (enc/format-num-fn 0 0))
(defn ^:public format-pstats
  "Formats given `pstats` to a string table.
    Accounted < Clock => Some work was done that wasn't tracked by any `p` forms.
    Accounted > Clock => Nested `p` forms, and/or parallel threads.

  Options include:
    `:incl-newline?` - Include terminating system newline? (default true)
    `:columns` ------- Default [:n :min #_:p25 :p50 #_:p75 :p90 :p95 :p99 :max :mean :mad :clock :sum]"

  ([ps] (format-pstats ps nil))
  ([ps
    {:keys [incl-newline? columns sort-fn format-id-fn max-id-width] :as opts
     :or
     {incl-newline? true
      columns       default-format-columns
      sort-fn       (fn [ss] (get (enc/force-ref ss) :sum))
      format-id-fn  (fn [id] (str id))}}]

   (when ps
     (let [{:keys [clock stats]} (enc/force-ref ps)
           clock-total (long (get clock :total))
           id-sstats* stats

           columns (format-columns columns)
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

       ;; Write header rows
       (doseq [column (into [:id] columns)]
         (when-not (= :id column)
           (enc/sb-append sb " "))
         (append-col column (get-in column->pattern [column :heading])))

       (enc/sb-append sb enc/newlines)

       ;; Write id rows
       (doseq [id sorted-ids]
         (let [ssm (enc/force-ref (get id-sstats* id))
               {:keys [n sum mean mad]} ssm]

           (append-col :id (format-id-fn id))
           (doseq [column columns]
             (enc/sb-append sb " ")
             (case column
               :n     (append-col column (format-n n))
               :mean  (append-col column (enc/format-nsecs mean))
               :mad   (append-col column (str "±" (enc/perc mad mean)        "%"))
               :sum   (append-col column (str     (enc/perc sum clock-total) "%"))
               :clock (append-col column (enc/format-nsecs sum))
               (do    (append-col column (enc/format-nsecs (get ssm column))))))

           (enc/sb-append sb enc/newline)))

       ;; Write accounted row
       (enc/sb-append sb enc/newline)
       (append-col :id "Accounted")
       (doseq [column columns]
         (enc/sb-append sb " ")
         (case column
           :sum   (append-col column    (str (enc/perc accounted-total clock-total) "%"))
           :clock (append-col column (enc/format-nsecs accounted-total))
           (do    (append-col column ""))))

       ;; Write clock row
       (enc/sb-append sb enc/newline)
       (append-col :id "Clock")
       (doseq [column columns]
         (enc/sb-append sb " ")
         (case column
           :sum   (append-col column "100%")
           :clock (append-col column (enc/format-nsecs clock-total))
           (do    (append-col column ""))))

       (when incl-newline?
         (enc/sb-append sb enc/newline))

       (str sb)))))

(comment
  (defn rand-vs [n & [max]] (take n (repeatedly (partial rand-int (or max Integer/MAX_VALUE)))))
  (println
    (format-pstats
      {:clock {:total (* 1e6 30)}
       :stats
       {:foo (stats/summary-stats (rand-vs 1e4 20))
        :bar (stats/summary-stats (rand-vs 1e2 50))
        :baz (stats/summary-stats (rand-vs 1e5 30))}})))

(defn ^:public format-grouped-pstats
  "Alpha, subject to change.
  Takes a map of {<profiling-id> <pstats>} and formats a combined
  output string using `format-pstats`.

  See also example Clj project."
  ([m] (format-grouped-pstats m nil))
  ([m {:keys [group-sort-fn format-pstats-opts]
       :or   {group-sort-fn (fn [m] (get-in m [:clock :total] 0))}}]

   (when m
     (let [m ; {<profiling-id> <realised-pstats>}
           (persistent!
             (reduce-kv
               (fn [m k v] (assoc! m k (enc/force-ref v)))
               (transient m)
               m))

           sorted-profiling-ids
           (sort-by (fn [id] (group-sort-fn (get m id)))
             enc/rcompare (keys m))

           ^long max-id-width
           (reduce-kv
             (fn [^long acc _ {:keys [clock stats]}]
               (if-let [c (get-max-id-width stats format-pstats-opts)]
                 (if (> (long c) acc) c acc)
                 acc))
             0 m)

           sep (str "," enc/newline)
           format-pstats-opts
           (assoc format-pstats-opts
             :max-id-width max-id-width)]

       (enc/str-join enc/newlines
         (map (fn [id] (str id sep (format-pstats (get m id) format-pstats-opts))))
         sorted-profiling-ids)))))

;;;; API helpers

#?(:clj (defmacro docstring [    rname] (enc/slurp-resource (str "docs/"    (name rname) ".txt"))))
#?(:clj (defmacro defhelp   [sym rname] `(enc/def* ~sym {:doc ~(eval `(docstring ~rname))} "See docstring")))

#?(:clj
   (defn location-str [location]
     (when-let [{:keys [ns line column]} location]
       (when ns
         (if line
           (if column
             (str ns "[" line "," column "]")
             (str ns "[" line            "]"))
           ns)))))

#?(:clj
   (defn valid-opts! [macro-form macro-env caller opts body]
     (cond
       (not (map? opts))
       (truss/ex-info!
         (str "`" caller "` needs compile-time map opts at " (location-str (enc/get-source macro-form macro-env)) ": "
           `(~caller ~opts ~@body)))

       (not (enc/const-form? (get opts :dynamic?)))
       (truss/ex-info!
         (str "`" caller "` needs compile-time `:dynamic?` value at " (location-str (enc/get-source macro-form macro-env)) ": "
           `(~caller ~opts ~@body)))

       :else opts)))

;;;; Profiling Signals
;; - Filtering relevant for `profiled`, `profile`
;; - Handlers  relevant for `profiled` only

(enc/defonce ^:dynamic *sig-handlers* "?[<wrapped-handler-fn>]" nil)

(defrecord Signal
  ;; Based on `taoensso.telemere.impl/Signal`
  [schema inst, ns coords, id level, #?@(:clj [host thread]),
   sample ctx data, body-result, pstats format-pstats-fn]

  Object (toString [this] (str "taoensso.tufte.Signal" (enc/pr-edn* (into {} this)))))

;; Verbose constructors for readability + to support extra keys
(do     (enc/def-print-impl [x Signal] (str "#taoensso.tufte.Signal"      (enc/pr-edn* (into {} x)))))
#?(:clj (enc/def-print-dup  [x Signal] (str "#taoensso.tufte.impl.Signal" (enc/pr-edn* (into {} x)))))

(defn signal? #?(:cljs {:tag 'boolean}) [x] (instance? Signal x))

(defrecord WrappedSignal [ns id level signal-value_]
  sigs/ISignalHandling
  (allow-signal? [_ spec-filter] (spec-filter ns id level))
  (signal-debug  [_] {:ns ns, :id id, :level level})
  (signal-value  [_ handler-sample-rate]
    (sigs/signal-with-combined-sample-rate handler-sample-rate
      (force signal-value_))))

#?(:clj
   (let [base      (enc/get-env {:as :edn} [:taoensso.tufte/ct-filters<.platform><.edn>])
         ns-filter (enc/get-env {:as :edn} [:taoensso.tufte/ct-ns-filter<.platform><.edn> :taoensso.tufte/ns-pattern])
         id-filter (enc/get-env {:as :edn} [:taoensso.tufte/ct-id-filter<.platform><.edn>])
         min-level (enc/get-env {:as :edn} [:taoensso.tufte/ct-min-level<.platform><.edn> :taoensso.tufte/min-level])]

     (enc/defonce ct-call-filter
       "`SpecFilter` used for compile-time elision, or nil."
       (sigs/spec-filter
         {:ns-filter (or ns-filter (get base :ns-filter))
          :id-filter (or id-filter (get base :id-filter))
          :min-level (or min-level (get base :min-level))}))))

(let [base      (enc/get-env {:as :edn}             [:taoensso.tufte/rt-filters<.platform><.edn>])
      ns-filter (enc/get-env {:as :edn}             [:taoensso.tufte/rt-ns-filter<.platform><.edn>])
      id-filter (enc/get-env {:as :edn}             [:taoensso.tufte/rt-id-filter<.platform><.edn>])
      min-level (enc/get-env {:as :edn, :default 2} [:taoensso.tufte/rt-min-level<.platform><.edn>])]

  (enc/defonce ^:dynamic *rt-call-filter*
    "`SpecFilter` used for runtime filtering, or nil."
    (sigs/spec-filter
      {:ns-filter (or ns-filter (get base :ns-filter))
       :id-filter (or id-filter (get base :id-filter))
       :min-level (or min-level (get base :min-level))})))

(comment (enc/get-env {:as :edn, :return :explain} :taoensso.tufte/rt-filters<.platform><.edn>))

;;;; Handlers

(defrecord HandlerVal [ns-str level ?id ?data pstats pstats-str_ ?file ?line]
  Object (toString [this] (str "taoensso.tufte.HandlerVal" (enc/pr-edn* (into {} this)))))

;; Verbose constructors for readability + to support extra keys
(do     (enc/def-print-impl [x HandlerVal] (str "#taoensso.tufte.HandlerVal"      (enc/pr-edn* (into {} x)))))
#?(:clj (enc/def-print-dup  [x HandlerVal] (str "#taoensso.tufte.impl.HandlerVal" (enc/pr-edn* (into {} x)))))

(enc/defonce handlers_ "{<hid> <handler-fn>}" (atom nil))

#?(:clj
   (enc/defonce ^:private ^java.util.concurrent.ArrayBlockingQueue handler-queue
     "While user handlers should ideally be non-blocking, we'll use a queue
     here to be safe + make sure we never tie up the execution thread."
     (java.util.concurrent.ArrayBlockingQueue. 1024)))

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

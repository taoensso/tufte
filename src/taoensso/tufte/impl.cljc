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
   [taoensso.encore :as enc :refer [have have?]]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte.stats    :as stats])

  #?(:cljs
     (:require-macros
      [taoensso.tufte.impl :refer [mt-acc mt-add! mt-count]])))

(comment (remove-ns 'taoensso.tufte.impl))

;;;; Basic types

(do
  (deftype Time     [id ^long runtime loc-info]) ; Runtime entry
  (deftype TimeSpan [^long t0 ^long t1])         ; Accumulated during PStats merge
  (deftype TimeAcc  [^long tsum ^long tmax])     ; Used during reductions, etc.
  (deftype IdState  [id-times id-sstats]))       ; Used during reductions, etc.

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
  ;; - Public, mergeable, immutable version of `PData`.
  ;; - Deref to get `RealizedPStats`.
  ;; - All fields but `realized_` to support merging.

  #?@(:clj  [clojure.lang.IDeref  (deref [this] @realized_)]
      :cljs [             IDeref (-deref [this] @realized_)])

  #?@(:clj  [clojure.lang.IPending (isRealized [_] (realized? realized_))]
      :cljs [             IPending (-realized? [_] (realized? realized_))])

  Object (toString [_] (str "TuftePStats[" (if (realized? realized_) "ready" "pending") "]")))

#?(:clj (enc/deftype-print-methods PStats))

(declare
  ^:private ^IdState compact-id-state
  ^:private          deref-pstats)

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
  ;; - Invoke [id nsecs loc] to capture Time
  ;; - Invoke [] to get immutable `PStats`

  #?@(:clj  [clojure.lang.IDeref (deref  [this] (this))]
      :cljs [             IDeref (-deref [this] (this))])

  #?(:clj clojure.lang.IFn :cljs IFn)

  ;; Captures time, compacting as necessary
  (#?(:clj invoke :cljs -invoke) [this id nsecs-elapsed loc-info]
    (when-let [acc-snap acc]
      (let [new-time (Time. id ^long nsecs-elapsed loc-info)]
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
            ;; CAS against `acc-snap`. Note that for performance reasons we tolerate
            ;; a theoretical risk of losing times under high contention when `nmax`
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

(def ^:dynamic  *pdata* "nnil iff dynamic profiling active" nil)
(defn ^PData new-pdata-local   [^long nmax] (let [t0 (enc/now-nano*)] (PData. false nmax t0 (mt-acc)        nil nil false)))
(defn ^PData new-pdata-dynamic [^long nmax] (let [t0 (enc/now-nano*)] (PData. true  nmax t0 (enc/latom nil) nil nil false)))

(comment
  (enc/qb 1e6 (new-pdata-local 10) (new-pdata-dynamic 10)) ; [78.4 102.78]

  (let [pd (new-pdata-local   10)] (pd :id1 100 nil) @(pd))
  (let [pd (new-pdata-dynamic 10)] (pd :id1 100 nil) @(pd))

  (let [pdl (new-pdata-local   5e6)
        pdd (new-pdata-dynamic 5e6)]

    (enc/qb 1e6 ; [40.57 69.59]
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
  (enc/qb 1e6 *pdata* (pdata-local-get)) ; [22.42 42.55]
  (enc/qb 1e6  ; [337.03 97.05]
    (binding [*pdata* "foo"])
    (try (pdata-local-push "foo") (finally (pdata-local-pop))))

  (do
    (pdata-local-push "pd1")
    (pdata-local-push "pd2")
    [(let [pd (pdata-local-get)] (pdata-local-pop) pd)
     (let [pd (pdata-local-get)] (pdata-local-pop) pd)])

  (enc/qb 1e3 ; 14.99
    (try
      (pdata-local-push (new-pdata-local 1e7))
      (dotimes [_ 1e3] ((pdata-local-get) :foo 1 nil))
      (finally (pdata-local-pop)))))

;;;; Compaction, etc.

(defn- times-into-id-times
  "NB treats `from-times` as read-only (may be mutable `acc`)!"
  [to-id-times from-times]
  (not-empty
    (if-let [from-times (enc/force-ref from-times)]
      (persistent!
        (reduce
          (fn [m ^Time in]
            (let [rt  (.-runtime  in)
                  id  (.-id       in)
                  loc (.-loc-info in)
                  id* [id loc]]

              ;; We'll use [id loc] as a pseudo id from here on in
              ;; order to track location info. We'll then reverse
              ;; the wrapping on final `deref-pstats`.
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

(defn- ^IdState compact-id-state
  ;; Note that compaction cost doesn't distort p times unless there's
  ;; p nesting (where outer p time includes inner p's capture time).
  [nmax id-times id-sstats pulled-times]
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

(defn- ^RealizedPStats deref-pstats
  [nmax t0 t1 acc id-times id-sstats tspans]
  (let [clock-total (tspans->tsum tspans)
        id-times    (times-into-id-times id-times acc)
        public-stats-map ; {<id*> <stats/sstats-map>}
        (when id-times
          (let [;;; Disposable internal state
                id*-sstats_ (volatile! (transient {})) ; {<id*> <sstats>}
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

    ;; {:clock _ :stats {<id> <stats/sstats-map>}}
    (RealizedPStats. t0 t1 clock-total public-stats-map
      {:t0 t0 :t1 t1 :total clock-total})))

;;;; PStats utils

(defn ^:public merge-pstats
  "Merges given pstats, compacting as necessary.
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

(defn ^:public format-pstats
  "Formats given pstats to a string table.
    Accounted < Clock => Some work was done that wasn't tracked by any p forms.
    Accounted > Clock => Nested p forms, and/or parallel threads."
  ([ps     ] (format-pstats ps nil))
  ([ps opts]
   (when ps
     (let [{:keys [clock stats]} (if (instance? PStats ps) @ps ps)]
       (stats/format-pstats (get clock :total) stats opts)))))

(defn ^:public format-grouped-pstats
  "Alpha, subject to change.
  Takes a map of {<profile-id> <pstats>} and formats a combined
  output string using `format-pstats`.

  See also example Clj project."
  ([m] (format-grouped-pstats m nil))
  ([m {:keys [group-sort-fn format-pstats-opts]
       :or   {group-sort-fn (fn [m] (get-in m [:clock :total] 0))}}]

   (when m
     (let [m ; {<profile-id> <realised-pstats>}
           (persistent!
             (reduce-kv
               (fn [m k v] (assoc! m k (enc/force-ref v)))
               (transient m)
               m))

           sorted-profile-ids
           (sort-by (fn [id] (group-sort-fn (get m id)))
             enc/rcompare (keys m))

           ^long max-id-width
           (reduce-kv
             (fn [^long acc _ {:keys [clock stats]}]
               (if-let [c (stats/get-max-id-width stats format-pstats-opts)]
                 (if (> (long c) acc) c acc)
                 acc))
             0 m)

           sep (str "," enc/newline)
           format-pstats-opts
           (assoc format-pstats-opts
             :max-id-width max-id-width)]

       (enc/str-join enc/newlines
         (map (fn [id] (str id sep (format-pstats (get m id) format-pstats-opts))))
         sorted-profile-ids)))))

;;;; Signals
;; Design shared by: Telemere, Tufte, Timbre

(enc/defonce ^:dynamic *sig-handlers*
  "{<handler-id> (fn wrapped-handler-fn [^ProfilingSignal profiling-signal])}"
  nil)

(defrecord ProfilingSignal
  ;; Based on Telemere Signal
  [instant id level, loc ns line #_column #_file, data pstats pstats-str_]
  sigs/IFilterableSignal (allow-signal? [_ sig-filter] (sig-filter ns id level)))

#?(:clj
   (let [base      (enc/read-sys-val* [:taoensso.tufte/ct-filters])
         ns-filter (enc/read-sys-val* [:taoensso.tufte.ct-ns-filter :taoensso.tufte/ns-pattern])
         id-filter (enc/read-sys-val* [:taoensso.tufte.ct-id-filter])
         min-level (enc/read-sys-val* [:taoensso.tufte.ct-min-level :taoensso.tufte/min-level])]

     (enc/defonce ct-sig-filter
       "`SigFilter` used for compile-time elision, or nil."
       (sigs/sig-filter
         {:ns-filter (or ns-filter (get base :ns-filter))
          :id-filter (or id-filter (get base :id-filter))
          :min-level (or min-level (get base :min-level))}))))

(let [base      (enc/read-sys-val* [:taoensso.tufte/rt-filters])
      ns-filter (enc/read-sys-val* [:taoensso.tufte.rt-ns-filter])
      id-filter (enc/read-sys-val* [:taoensso.tufte.rt-id-filter])
      min-level (enc/read-sys-val* [:taoensso.tufte.rt-min-level])]

  (enc/defonce ^:dynamic *rt-sig-filter*
    "`SigFilter` used for runtime filtering, or nil."
    (sigs/sig-filter
      {:ns-filter (or ns-filter (get base :ns-filter))
       :id-filter (or id-filter (get base :id-filter))
       :min-level (or min-level (get base :min-level))})))

(defn signal-msg
  "Returns signal message for given `ProfilingSignal`.
  Used by `telemere-handler`, `timbre-handler`."
  [^ProfilingSignal ps format-pstats-opts]
  (let [info (enc/assoc-some {:level (.-level ps)} :id (.-id ps) :data (.-data ps))]
    (str
      "Tufte `profile` stats " info ":" enc/newlines
      (format-pstats (.-pstats ps) format-pstats-opts)
      enc/newline)))

(defn signal-level
  "Returns signal level for given `ProfilingSignal`.
  Used by `telemere-handler`, `timbre-handler`."
  [^ProfilingSignal ps signal-level]
  (let [ps-level (.-level ps)]
    (enc/cond
      (nil? signal-level)               ps-level
      (ifn? signal-level) (signal-level ps-level)
      :else                signal-level)))

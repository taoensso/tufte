(ns taoensso.tufte
  "A simple, fast, monitoring profiler for Clojure/Script.

  Usage: wrap+name interesting body exprs with the `p` macro. Then activate
  profiling of these wrapped exprs using the `profiled` or `profile` macros:

    (profiled {} (p :my-fn (my-fn))) ; Returns [<body-result> <?stats-map>]
    (profile  {} (p :my-fn (my-fn))) ; Returns  <body-result>, dispatches
                                     ; ?stats-map to any registered handlers.

  Extensive facilities are provided for compile-time elision and runtime
  filtering.

  See the relevant docstrings for more info:
    `p`, `profiled`, `profile`, `add-handler!` ; Core API

    (p        [opts & body] [id & body]) ; e.g. `(p ::my-pid (do-work))`
    (profiled [opts & body])             ; e.g. `(profiled {:level 2} (my-fn))`
    (profile  [opts & body])             ; e.g. `(profiled {:level 2} (my-fn))`

    (add-handler! [handler-id ns-pattern handler-fn])

  How/where to use this library:
    Tufte is highly optimized: even without elision, you can usually leave
    profiling code in production (e.g. for sampled profiling, or to detect
    unusual performance behaviour). Tufte's stats maps are well suited to
    programmatic inspection + analysis."

  {:author "Peter Taoussanis (@ptaoussanis)"}

  #?(:clj
     (:require
      [taoensso.encore     :as enc]
      [taoensso.tufte.impl :as impl]))

  #?(:clj (:import [taoensso.tufte.impl IdStats Stats Clock]))

  #?(:cljs
     (:require
      [taoensso.encore     :as enc  :refer-macros []]
      [taoensso.tufte.impl :as impl :refer [IdStats Stats Clock]]))

  #?(:cljs (:require-macros [taoensso.tufte :refer [profiled]])))

(enc/assert-min-encore-version [2 85 0])

;;;; Level filtering

;; We distinguish between run and min levels to ensure that it's
;; always possible to set the min-level > any run level (i.e. to
;; disable profiling)
(defn valid-run-level? [x] (if (#{0 1 2 3 4 5}   x) true false))
(defn valid-min-level? [x] (if (#{0 1 2 3 4 5 6} x) true false))

(def ^:private ^:const invalid-run-level-msg         "Invalid Tufte profiling level: should be int e/o #{0 1 2 3 4 5}")
(def ^:private ^:const invalid-min-level-msg "Invalid minimum Tufte profiling level: should be int e/o #{0 1 2 3 4 5 6}")

(defn ^:static valid-run-level [x]
  (or (#{0 1 2 3 4 5} x)
      (throw (ex-info invalid-run-level-msg {:given x :type (type x)}))))

(comment (enc/qb 1e5 (valid-run-level 4))) ; 7.82

(defn ^:static valid-min-level [x]
  (or (#{0 1 2 3 4 5 6} x)
      (throw (ex-info invalid-min-level-msg {:given x :type (type x)}))))

(def ^:dynamic  *min-level* "e/o #{0 1 2 3 4 5 6}" 2)
(defn        set-min-level!
  "Sets root binding of minimum profiling level, e/o #{0 1 2 3 4 5 6}.
    0 => Enable  all profiling.
    6 => Disable all profiling."
  [level]
  (valid-min-level level)
  #?(:cljs (set!             *min-level*        level)
     :clj  (alter-var-root #'*min-level* (fn [_] level))))

(comment (enc/qb 1e6 *min-level*)) ; 25.93

(defmacro with-min-level
  "Executes body with dynamic minimum profiling level, e/o #{0 1 2 3 4 5 6}.
    0 => Enable  all profiling.
    6 => Disable all profiling."
  [level & body]
  (if (integer? level)
    (do
      (valid-min-level level)
      `(binding [*min-level*                ~level ] ~@body))
    `(binding [*min-level* (valid-min-level ~level)] ~@body)))

;;;; Namespace filtering

(def -compile-ns-filter (enc/memoize_ enc/compile-ns-filter))

(def ^:dynamic *ns-filter* "(fn [?ns] -> truthy)." (-compile-ns-filter "*"))

(defn set-ns-pattern!
  "Sets root binding of namespace filter.
  See `compile-ns-filter` docstring for details on `ns-pattern` arg."
  [ns-pattern]
  (let [nsf? (-compile-ns-filter ns-pattern)]
    #?(:cljs (set!             *ns-filter*        nsf?)
       :clj  (alter-var-root #'*ns-filter* (fn [_] nsf?)))))

(defmacro with-ns-pattern
  "Executes body with dynamic namespace filter.
  See `compile-ns-filter` docstring for details on `ns-pattern` arg."
  [ns-pattern & body]
  `(binding [*ns-filter* (-compile-ns-filter ~ns-pattern)]
     ~@body))

(comment
  (def nsf? (compile-ns-filter #{"foo.*" "bar"}))
  (nsf? "foo.bar")
  (with-ns-pattern "foo.baz"    (profiled {} (p {:id "id"} "body")))
  (with-ns-pattern "taoensso.*" (profiled {} (p {:id "id"} "body"))))

;;;; Combo filtering

#?(:clj
   (def ^:private compile-time-min-level
     (when-let [level (enc/read-sys-val "TUFTE_MIN_LEVEL")]
       (println (str "Compile-time (elision) Tufte min-level: " level))
       (valid-min-level level))))

#?(:clj
   (def ^:private compile-time-ns-filter
     (let [ns-pattern (enc/read-sys-val "TUFTE_NS_PATTERN")]
       (when ns-pattern
         (println (str "Compile-time (elision) Tufte ns-pattern: " ns-pattern)))
       (-compile-ns-filter (or ns-pattern "*")))))

#?(:clj ; Called only at macro-expansiom time
   (defn -elide?
     "Returns true iff level or ns are compile-time filtered."
     [level-form ns-str-form]
     (not
       (and
         (or ; Level okay
           (nil? compile-time-min-level)
           (not (valid-run-level? level-form)) ; Not a compile-time level const
           (>= ^long level-form ^long compile-time-min-level))

         (or ; Namespace okay
           (not (string? ns-str-form)) ; Not a compile-time ns-str const
           (compile-time-ns-filter ns-str-form))))))

(defn #?(:clj may-profile? :cljs ^boolean may-profile?)
  "Returns true iff level and ns are runtime unfiltered."
  ([level   ] (may-profile? level *ns*))
  ([level ns]
   (if (>=  ^long (valid-run-level level)
         ;; ^long (valid-min-level *min-level*)
            ^long                  *min-level* ; Assume valid
         )
     (if (*ns-filter* ns) true false)
     false)))

(comment (enc/qb 1e5 (may-profile? 2))) ; 17.74

;;;; Output handlers
;; Handlers are used for `profile` output, let us nicely decouple stat
;; creation and consumption.

(defrecord HandlerVal [ns-str level ?id ?data stats stats-str_ ?file ?line])

(def      handlers_ "{<handler-id> <handler-fn>}" impl/handlers_)
(defn add-handler!
  "Use this to register interest in stats output produced by `profile` calls.
  Each registered `handler-fn` will be called as:

    (handler-fn {:ns-str _ :level _ :?id _ :?data _ :stats _ :stats-str_ _})

  Map args:
    :ns-str     - Namespace string where `profile` call took place
    :level      - Level e/o #{0 1 2 3 4 5}, given in `(profile {:level _} ...)`
    :?id        - Optional id,              given in `(profile {:id    _} ...)`
    :?data      - Optional arb data,        given in `(profile {:data  _} ...)`
    :stats      - Stats map as in `(second (profiled ...))`
    :stats-str_ - `(delay (format-stats stats))`

  Error handling (NB):
    Handler errors will be silently swallowed. Please `try`/`catch` and
    appropriately deal with (e.g. log) possible errors *within* `handler-fn`.

  Async/blocking:
    `handler-fn` should ideally be non-blocking, or reasonably cheap. Handler
     dispatch occurs through a 1-thread 1k-buffer dropping queue.

  Ns filtering:
    Provide an optional `ns-pattern` arg to only call handler for matching
    namespaces. See `compile-ns-filter` docstring for details on `ns-pattern`.

  Handler ideas:
    Save to a db, log, `put!` to an appropriate `core.async` channel, filter,
    aggregate, use for a realtime analytics dashboard, examine for outliers
    or unexpected output, ..."

  ([handler-id handler-fn] (add-handler! handler-id nil handler-fn))
  ([handler-id ns-pattern handler-fn]
   (let [f (if (or (nil? ns-pattern) (= ns-pattern "*"))
             handler-fn
             (let [nsf? (-compile-ns-filter ns-pattern)]
               (fn [m]
                 (when (nsf? (get m :ns-str))
                   (handler-fn m)))))]
     (set (keys (swap! handlers_ assoc handler-id f))))))

(defn remove-handler! [handler-id]
  (set (keys (swap! handlers_ dissoc handler-id))))

(defn add-basic-println-handler!
  "Adds a simple handler that logs `profile` stats output with `println`."
  [{:keys [ns-pattern] :or {ns-pattern "*"}}]
  (add-handler! :basic-println
    ns-pattern
    (fn [m]
      (let [{:keys [stats-str_ ?id ?data]} m
            stats-str (force stats-str_)]
        (println
          (str
            (when ?id   (str "\nid: "   ?id))
            (when ?data (str "\ndata: " ?data))
            "\n" stats-str))))))

(comment (add-basic-println-handler! {}))

;;;; Some low-level primitives

(defn profiling? "Returns e/o #{nil :thread :dynamic}."
  [] (if impl/*pdata_* :dynamic (if (impl/pdata-proxy) :thread)))

(comment (enc/qb 1e6 (profiling?))) ; 51.01

(defn start-profiling-thread!
  "Warning: this is a low-level primitive. Prefer higher-level macros
  like `profile` when possible.

  NB: must be accompanied by a call to `stop-profiling-thread!`
  (e.g. using `try`/`finally`)."
  []
  (impl/pdata-proxy (impl/new-pdata-thread))
  nil)

(defn stop-profiling-thread!
  "Warning: this is a low-level primitive."
  []
  (when-let [pdata (impl/pdata-proxy)]
    (let [result (impl/pdata->Stats pdata)]
      (impl/pdata-proxy nil)
      result)))

;;;; Core macros

#?(:clj
   (defmacro profiled
     "Always executes body, and always returns [<body-result> ?<stats>].

     When [ns level] unelided and [ns level `when`] unfiltered, executes body
     with profiling active.

     Handy if you'd like to consume stats output directly.
     Otherwise see `profile`.

     Compile-time opts:
       :level    - e/o #{0 1 2 3 4 5} ; Default is `5`
       :dynamic? - Use multi-threaded profiling? ; Default is `false`
       :when     - Optional arbitrary conditional form (e.g. boolean expr)

     A comment on laziness:
       Please note that lazy seqs and other forms of laziness (e.g. delays)
       will only contribute to profiling results if/when evaluation actually
       occurs. This is intentional and a useful property. Compare:

       (profiled {}  (delay (Thread/sleep 2000))) ; Doesn't count sleep
       (profiled {} @(delay (Thread/sleep 2000))) ; Counts sleep"

     [opts & body]
     (let [ns-str (str *ns*)]

       (when-not (map? opts)
         (throw
           (ex-info "`tufte/profiled` requires a compile-time map as first arg."
             {:ns-str ns-str :line (:line (meta &form))
              :form (cons 'profiled (cons opts body))})))

       (let [level-form (get opts :level    5)
             dynamic?   (get opts :dynamic? false)
             test-form  (get opts :when     true)]

         (when (integer? level-form) (valid-run-level level-form))

         (if (-elide? level-form ns-str)
           `[(do ~@body)]
           (let [runtime-check
                 (if (= test true) ; Common case
                   `(may-profile? ~level-form ~ns-str)
                   `(and (may-profile? ~level-form ~ns-str) ~test-form))]

             (if dynamic?
               `(if ~runtime-check
                  (let [pdata_# (impl/new-pdata-dynamic)]
                    (binding [impl/*pdata_* pdata_#]
                      (let [result# (do ~@body)
                            stats#  (impl/pdata->Stats @pdata_#)]
                        [result# stats#])))
                  [(do ~@body)])

               `(if ~runtime-check
                  (try
                    (impl/pdata-proxy (impl/new-pdata-thread))
                    (let [result# (do ~@body)
                          stats#  (impl/pdata->Stats (impl/pdata-proxy))]
                      [result# stats#])
                    (finally (impl/pdata-proxy nil)))
                  [(do ~@body)]))))))))

(declare format-stats)

#?(:clj
   (defmacro profile
     "Always executes body, and always returns <body-result>.

     When [ns level] unelided and [ns level `when`] unfiltered, executes body
     with profiling active and dispatches stats to any registered handlers
     (see `add-handler!`).

     Handy if you'd like to consume/aggregate stats output later/elsewhere.
     Otherwise see `profiled`.

     Compile-time opts:
       :level    - e/o #{0 1 2 3 4 5} ; Default is `5`
       :dynamic? - Use multi-threaded profiling? ; Default is `false`
       :when     - Optional arbitrary conditional form (e.g. boolean expr)
       :id       - Optional stats id provided to handlers (e.g. `::my-stats-1`)
       :data     - Optional, any other arbitrary data provided to handlers

     A comment on laziness:
       Please note that lazy seqs and other forms of laziness (e.g. delays)
       will only contribute to profiling results if/when evaluation actually
       occurs. This is intentional and a useful property. Compare:

       (profiled {}  (delay (Thread/sleep 2000))) ; Doesn't count sleep
       (profiled {} @(delay (Thread/sleep 2000))) ; Counts sleep"

     [opts & body]
     (let [ns-str (str *ns*)]

       (when-not (map? opts)
         (throw
           (ex-info "`tufte/profile` requires a compile-time map as first arg."
             {:ns-str ns-str :line (:line (meta &form))
              :form (cons 'profile (cons opts body))})))

       (let [level-form (get opts :level    5)
             dynamic?   (get opts :dynamic? false)
             test-form  (get opts :when     true)
             id-form    (get opts :id)
             data-form  (get opts :data)]

         (when (integer? level-form) (valid-run-level level-form))

         `(let [[result# stats#] (profiled ~opts ~@body)]
            (when stats#
              (impl/handle!
                (->HandlerVal ~ns-str ~level-form ~id-form ~data-form
                  stats# (delay (format-stats stats#))
                  ~*file* ~(:line (meta &form)))))
            result#)))))


(comment
  (profiled {} "body")
  (profiled {:when (chance 0.5)} "body")
  (profile  {:id ::my-id} "body"))

#?(:clj
   (defmacro p
     "Profiling spy. Always executes body, and always returns <body-result>.

     When [ns level] unelided and profiling is active, records execution
     time of body.

     Compile-time opts:
      :id    - Id for this body in stats output (e.g. `::my-fn-call`)
      :level - e/o #{0 1 2 3 4 5} ; Default is `5`"

     {:arglists '([id & body] [opts & body])}
     [s1 & body]
     (let [ns-str  (str *ns*)
           opts    (if (map? s1) s1 {:level 5 :id s1})
           level   (get opts :level)
           id-form (get opts :id)]

       ;; If *any* level is present, it must be a valid compile-time level
       ;; since this macro doesn't offer runtime level checking
       (when level (valid-run-level level))

       (when (nil? id-form)
         (throw
           (ex-info "`tufte/p` requires an id."
             {:ns-str ns-str :line (:line (meta &form))
              :opts opts
              :form (cons 'p (cons s1 body))})))

       (if (-elide? level ns-str)
         `(do ~@body)
         ;; Note no runtime `may-profile?` check
         `(let [~'__pdata-or-pdata_ (or impl/*pdata_* (impl/pdata-proxy))]
            (if ~'__pdata-or-pdata_
              (let [~'__t0     (enc/now-nano*)
                    ~'__result (do ~@body)
                    ~'__t1     (enc/now-nano*)]
                (impl/capture-time! ~'__pdata-or-pdata_ ~id-form
                  (- ~'__t1 ~'__t0))
                ~'__result)
              (do ~@body)))))))

#?(:clj (defmacro pspy "`p` alias" [& args] `(p ~@args)))

(comment
  (p :p1 "body")
  (profiled {} (p :p1))
  (profiled {} (p {:level 5 :id :p1}))
  (profiled {} (p (let [x :foo/id] x) "body"))
  (enc/qb 1e5  (profiled {} 2 (p :p1))) ; 195.56
  (enc/time-ms (profiled {} 2 (enc/qb 1e6 (p :p1)))) ; 2485
  (profiled {:level 2 :when (chance 0.5)} (p :p1 "body"))
  (profiled {} (p :foo (p :bar))))

;;;; Public user utils

(defn compile-ns-filter
  "Returns (fn [?ns]) -> truthy. Some example patterns:
    \"foo.bar\", \"foo.bar.*\", #{\"foo\" \"bar\"},
    {:whitelist [\"foo.bar.*\"] :blacklist [\"baz.*\"]}"
  [ns-pattern] (enc/compile-ns-filter ns-pattern))

(defn chance "Returns true with 0<`p`<1 probability."
  [p] (< ^double (rand) (double p)))

(defn merge-stats
  "Merges stats maps from multiple runs or threads.
  Automatically identifies and merges concurrent time windows."
  [s1 s2]
  (if s1
    (if s2
      (let [^Stats s1 s1
            ^Stats s2 s2
            ^Clock clock1 (.-clock s1)
            ^Clock clock2 (.-clock s2)
            s1-t0 (.-t0 clock1)
            s1-t1 (.-t1 clock1)
            s2-t0 (.-t0 clock2)
            s2-t1 (.-t1 clock2)
            clock-overlap?
            (and
              (not (zero? s1-t0))
              (not (zero? s2-t0))
              (or
                (and (<= s2-t0 s1-t1)
                     (>= s2-t1 s1-t0))
                (and (<= s1-t0 s2-t1)
                     (>= s1-t1 s2-t0))))

            ^Clock clock3
            (if clock-overlap?
              (let [s3-t0 (if (< s1-t0 s2-t0) s1-t0 s2-t0)
                    s3-t1 (if (< s1-t1 s2-t1) s1-t1 s2-t1)]
                (Clock. s3-t0 s3-t1 (- s3-t1 s3-t0)))
              (Clock. 0 0 (+ (.-total clock1) (.-total clock2))))

            m-id-stats1 (.-id-stats-map s1)
            m-id-stats2 (.-id-stats-map s2)
            all-ids (into (set (keys m-id-stats1)) (keys m-id-stats2))

            m-id-stats3
            (reduce
              (fn [m id]
                (let [sid1 (get m-id-stats1 id)
                      sid2 (get m-id-stats2 id)]
                  (if sid1
                    (if sid2
                      (let [^IdStats sid1 sid1
                            ^IdStats sid2 sid2
                            s1-count   (.-count   sid1)
                            s1-time    (.-time    sid1)
                            s1-mad-sum (.-mad-sum sid1)
                            s1-min     (.-min     sid1)
                            s1-max     (.-max     sid1)

                            s2-count   (.-count   sid2)
                            s2-time    (.-time    sid2)
                            s2-mad-sum (.-mad-sum sid2)
                            s2-min     (.-min      sid2)
                            s2-max     (.-max      sid2)

                            s3-count   (+ s1-count   s2-count)
                            s3-time    (+ s1-time    s2-time)
                            s3-mad-sum (+ s1-mad-sum s2-mad-sum)]

                        (assoc m id
                          (IdStats.
                            s3-count
                            s3-time
                            (/ (double s3-time) (double s3-count))
                            s3-mad-sum
                            (/ (double s3-mad-sum) (double s3-count))
                            (if (< s1-min s2-min) s1-min s2-min)
                            (if (> s1-max s2-max) s1-max s2-max))))
                      m #_(assoc m id sid1))
                    (assoc m id sid2))))
              #_(transient m-id-stats1) m-id-stats1 ; Usu. <10 entries
              all-ids)]
        (Stats. clock3 m-id-stats3))
      s1)
    s2))

(defn accounted-time [^Stats stats]
  (reduce-kv
    (fn [^long acc id ^IdStats v]
      (+ acc ^long (.-time v)))
    0 (.-id-stats-map stats)))

(comment
  (accounted-time (second (profiled {})))
  (accounted-time (second (profiled {} (p :p1)))))

(defn stats-accumulator
  "Experimental, subject to change!
  Small util to help merge stats maps from multiple runs or threads.
  Returns a stateful fn with arities:
    ([stats]) ; Accumulates the given stats (you may call this from any thread)
    ([])      ; Deref: returns the merged value of all accumulated stats"
  []
  (let [acc_ (atom nil)
        reduce-stats_
        (delay
          (let [merge-stats (enc/memoize_ merge-stats)]
            (enc/memoize_ (fn [acc] (reduce merge-stats nil acc)))))]

    (fn stats-accumulator
      ([stats] (when stats (swap! acc_ conj stats)))
      ([] (when-let [acc @acc_] (@reduce-stats_ acc))))))

(defn accumulate-stats "Experimental, subject to change!"
  [stats-accumulator [profiled-result profiled-?stats]]
  (when profiled-?stats (stats-accumulator profiled-?stats))
  profiled-result)

(comment
  (enc/qb 1e5 (stats-accumulator)) ; 5.87
  (let [sacc (stats-accumulator)]
    (accumulate-stats sacc (profiled {} (p :p1)))
    (accumulate-stats sacc (profiled {} (p :p2)))
    (sacc)))

#?(:clj
   (defn refer-tufte
     "(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])"
     [] (require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])))

(comment (refer-tufte))

;;;; Stats formatting

(defn- perc [n d] (Math/round (* (/ (double n) (double d)) 100.0)))
(comment (perc 14 24))

(defn- ft [nanosecs]
  (let [ns (long nanosecs)] ; Truncate any fractionals
    (cond
      (>= ns 1000000000) (str (enc/round2 (/ ns 1000000000))  "s") ; 1e9
      (>= ns    1000000) (str (enc/round2 (/ ns    1000000)) "ms") ; 1e6
      (>= ns       1000) (str (enc/round2 (/ ns       1000)) "μs") ; 1e3
      :else              (str                ns              "ns"))))

(defn format-stats
  ([stats]
   (format-stats stats
     (fn [id ^IdStats id-stats] (.-time id-stats))))

  ([stats sort-fn]
   (when stats
     (let [^Stats stats stats
           ^Clock clock (.-clock        stats)
           m-id-stats   (.-id-stats-map stats)
           clock-total  (.-total clock)

           ^long accounted (accounted-time stats)

           sorted-ids
           (sort-by
             (fn [id] (sort-fn id (get m-id-stats id)))
             enc/rcompare
             (keys m-id-stats))

           ^long max-id-width
           (reduce-kv
             (fn [^long acc k v]
               (let [c (count (str k))]
                 (if (> c acc) c acc)))
             #=(count "Accounted Time")
             m-id-stats)]

       #?(:cljs
          (let [sb
                (reduce
                  (fn [acc id]
                    (let [^IdStats id-stats (get m-id-stats id)
                          time (.-time id-stats)]
                      (enc/sb-append acc
                        (str
                          {:id      id
                           :n-calls     (.-count id-stats)
                           :min     (ft (.-min   id-stats))
                           :max     (ft (.-max   id-stats))
                           :mad     (ft (.-mad   id-stats))
                           :mean    (ft (.-mean  id-stats))
                           :time%   (perc time clock-total)
                           :time    (ft   time)}
                          "\n"))))
                  (enc/str-builder)
                  sorted-ids)]

            (enc/sb-append sb "\n")
            (enc/sb-append sb (str "Clock Time: (100%) " (ft clock-total) "\n"))
            (enc/sb-append sb (str "Accounted Time: (" (perc accounted clock-total) "%) " (ft accounted) "\n"))
            (str           sb)))

       #?(:clj
          (let [pattern   (str "%" max-id-width "s %,11d %9s %10s %9s %9s %7d %1s%n")
                s-pattern (str "%" max-id-width  "s %11s %9s %10s %9s %9s %7s %1s%n")
                sb
                (reduce
                  (fn [acc id]
                    (let [^IdStats id-stats (get m-id-stats id)
                          time (.-time id-stats)]
                      (enc/sb-append acc
                        (format pattern id
                          (.-count id-stats)
                          (ft (.-min   id-stats))
                          (ft (.-max   id-stats))
                          (ft (.-mad   id-stats))
                          (ft (.-mean  id-stats))
                          (perc time clock-total)
                          (ft   time)))))

                  (enc/str-builder (format s-pattern "pId" "nCalls" "Min" "Max" "MAD" "Mean" "Time%" "Time"))
                  sorted-ids)]

            (enc/sb-append sb (format s-pattern "Clock Time"     "" "" "" "" "" 100 (ft clock-total)))
            (enc/sb-append sb (format s-pattern "Accounted Time" "" "" "" "" "" (perc accounted clock-total) (ft accounted)))
            (str sb)))))))

;;;; fnp stuff

(defn- fn-sigs [def? fn-name sigs]
  (let [single-arity? (vector? (first sigs))
        sigs    (if single-arity? (list sigs) sigs)
        prepend (if def? "defn_" "fn_")
        get-id  (if single-arity?
                  (fn [fn-name _params] (keyword (str *ns*) (str prepend (name fn-name))))
                  (fn [fn-name  params] (keyword (str *ns*) (str prepend (name fn-name) \_ (count params)))))
        new-sigs
        (map
          (fn [[params & others]]
            (let [has-prepost-map?      (and (map? (first others)) (next others))
                  [?prepost-map & body] (if has-prepost-map? others (cons nil others))]
              (if ?prepost-map
                `(~params ~?prepost-map (p ~(get-id fn-name params) ~@body))
                `(~params               (p ~(get-id fn-name params) ~@body)))))
          sigs)]
    new-sigs))

(defmacro fnp "Like `fn` but wraps fn bodies with `p` macro."
  {:arglists '([name?  [params*] prepost-map? body]
               [name? ([params*] prepost-map? body)+])}
  [& sigs]
  (let [[?fn-name sigs] (if (symbol? (first sigs)) [(first sigs) (next sigs)] [nil sigs])
        new-sigs        (fn-sigs (not :def) (or ?fn-name (gensym "")) sigs)]
    (if ?fn-name
      `(fn ~?fn-name ~@new-sigs)
      `(fn           ~@new-sigs))))

(comment
  (fn-sigs "foo"       '([x]            (* x x)))
  (macroexpand '(fnp     [x]            (* x x)))
  (macroexpand '(fn       [x]            (* x x)))
  (macroexpand '(fnp bob [x] {:pre [x]} (* x x)))
  (macroexpand '(fn       [x] {:pre [x]} (* x x))))

(defmacro defnp "Like `defn` but wraps fn bodies with `p` macro."
  {:arglists
   '([name doc-string? attr-map?  [params*] prepost-map? body]
     [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  [& sigs]
  (let [[fn-name sigs] (enc/name-with-attrs (first sigs) (next sigs))
        new-sigs       (fn-sigs :def fn-name sigs)]
    `(defn ~fn-name ~@new-sigs)))

(comment
  (defnp foo "Docstring"                [x]   (* x x))
  (macroexpand '(defnp foo "Docstring"  [x]   (* x x)))
  (macroexpand '(defn  foo "Docstring"  [x]   (* x x)))
  (macroexpand '(defnp foo "Docstring" ([x]   (* x x))
                                       ([x y] (* x y))))
  (profiled {} (foo 5)))

;;;;

(comment
  (add-basic-println-handler! {})
  (defn sleepy-threads []
    (dotimes [n 5]
      (Thread/sleep 100) ; Unaccounted
      (p :future/outer @(future (Thread/sleep 500)))
      @(future (p :future/inner (Thread/sleep 500)))
      (p :1ms  (Thread/sleep 1))
      (p :2s   (Thread/sleep 2000))
      (p :50ms (Thread/sleep 50))
      (p :rand (Thread/sleep (if (> 0.5 (rand)) 10 500)))
      (p :10ms (Thread/sleep 10))
      "Result"))

  (profile {:level 2 :id ::sleepy-threads} (sleepy-threads))
  (profile {:level 2 :id ::sleepy-thread :dynamic? true
            :data "foo"}
    (sleepy-threads))

  (p :hello "Hello, this is a result") ; Falls through (no data context)

  (defnp arithmetic
    []
    (let [nums (vec (range 1000))]
      (+ (p :fast-sleep (Thread/sleep 1) 10)
         (p :slow-sleep (Thread/sleep 2) 32)
         (p :add  (reduce + nums))
         (p :sub  (reduce - nums))
         (p :mult (reduce * nums))
         (p :div  (reduce / nums)))))

  (profile  {} (dotimes [n 100] (arithmetic)))
  (profile  {} (dotimes [n 1e5] (p :p1 nil))) ; 29.37ms
  (profile  {} (dotimes [n 1e6] (p :p1 nil))) ; 181.65ms
  (profiled {} (dotimes [n 1e6] (p :p1 nil)))
  (profiled {:level 2 :when (chance 0.5)} "body"))

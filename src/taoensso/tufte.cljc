(ns taoensso.tufte
  "A simple, fast, monitoring profiler for Clojure/Script.

  Usage: wrap+name interesting body exprs with the `p` macro. Then activate
  profiling of these wrapped exprs using the `profiled` or `profile` macros:

    (profiled {} (p :my-fn (my-fn))) ; Returns [<body-result> <?pstats>]
    (profile  {} (p :my-fn (my-fn))) ; Returns  <body-result>, dispatches
                                     ; pstats to any registered handlers.

  Provides extensive facilities for compile-time elision and runtime filtering.

  See the relevant docstrings for more info:
    `p`, `profiled`, `profile`, `add-handler!` ; Core API

    (p        [opts & body] [id & body]) ; e.g. `(p ::my-id (do-work))`
    (profiled [opts & body])             ; e.g. `(profiled {:level 2} (my-fn))`
    (profile  [opts & body])             ; e.g. `(profiled {:level 2} (my-fn))`

    (add-handler! [handler-id ns-pattern handler-fn])

  How/where to use this library:
    Tufte profiling is highly optimized: even without elision, you can usually
    leave profiling active in production (e.g. for sampled profiling, or to
    detect unusual performance behaviour). Tufte's `pstats` data is well suited
    to programmatic monitoring."

  {:author "Peter Taoussanis (@ptaoussanis)"}

  (:require
   [taoensso.truss        :as truss]
   [taoensso.encore       :as enc]
   [taoensso.encore.stats :as stats]
   [taoensso.tufte.impl   :as impl #?@(:cljs [:refer [PStats]])])

  #?(:clj  (:import [taoensso.tufte.impl PStats]))
  #?(:cljs (:require-macros [taoensso.tufte :refer [profiled]])))

(comment
  (remove-ns (symbol (str *ns*)))
  (:api (enc/interns-overview)))

(enc/assert-min-encore-version [3 143 1])

;;;; Level filtering
;; Terminology note: we distinguish between call/form and min levels to ensure
;; that it's always possible to set min-level > any call-level to disable profiling.

(defn- valid-call-level? [x] (if (#{0 1 2 3 4 5}   x) true false))
(defn- valid-min-level?  [x] (if (#{0 1 2 3 4 5 6} x) true false))

(def ^:private ^:const invalid-call-level-msg        "Invalid Tufte profiling level: should be int e/o #{0 1 2 3 4 5}")
(def ^:private ^:const invalid-min-level-msg "Invalid minimum Tufte profiling level: should be int e/o #{0 1 2 3 4 5 6}")

(defn- valid-call-level [x] (or (#{0 1 2 3 4 5}   x) (truss/ex-info! invalid-call-level-msg {:given x :type (type x)})))
(defn- valid-min-level  [x] (or (#{0 1 2 3 4 5 6} x) (truss/ex-info! invalid-min-level-msg  {:given x :type (type x)})))
(comment (enc/qb 1e5 (valid-call-level 4))) ; 7.82

(def ^:dynamic *min-level*
  "Integer e/o #{0 1 2 3 4 5 6}, or vector mapping ns-patterns to min-levels:
    [[<ns-pattern> <min-level>] ... [\"*\" <default-min-level>]]

  See `*ns-filter*` for example patterns."
  2)

;;;; Namespace filtering
;; Terminology note: we distinguish loosely between `ns-filter` (which may be a
;; fn or `ns-pattern`) and `ns-pattern` (subtype of `ns-filter`).

(def ^:dynamic *ns-filter*
  "(fn may-profile-ns? [ns]) predicate, or ns-pattern.
  Example ns-patterns:
    #{}, \"*\", \"foo.bar\", \"foo.bar.*\", #{\"foo\" \"bar.*\"},
    {:allow #{\"foo\" \"bar.*\"} :deny #{\"foo.*.bar.*\"}}"
  "*")

(let [fn?         fn?
      compile     (enc/fmemoize (fn [x] (enc/name-filter x)))
      conform?*   (enc/fmemoize (fn [x ns] ((compile x) ns)))
      ;; conform? (enc/fmemoize (fn [x ns] (if (fn? x) (x ns) ((compile x) ns))))
      conform?
      (fn [ns-filter ns]
        (if (fn? ns-filter)
          (ns-filter           ns) ; Intentionally uncached, can be handy
          (conform?* ns-filter ns)))]

  (defn- #?(:clj may-profile-ns? :cljs ^boolean may-profile-ns?)
    "Implementation detail."
    ([          ns] (if (conform? *ns-filter* ns) true false))
    ([ns-filter ns] (if (conform?  ns-filter  ns) true false)))

  (def ^:private ns->?min-level
    "[[<ns-pattern> <min-level>] ... [\"*\" <default-min-level>]], ns -> ?min-level"
    (enc/fmemoize
      (fn [specs ns]
        (enc/rsome
          (fn [[ns-pattern min-level]]
            (when (conform?* ns-pattern ns)
              (valid-min-level min-level)))
          specs)))))

(comment
  (enc/qb 1e6
    (may-profile-ns? "taoensso.tufte")
    (ns->?min-level [[#{"taoensso.*" "foo.bar"} 1] ["*" 2]] "foo.bar")) ; [162.85 136.88]

  (binding [*ns-filter* "foo.baz"]    (profiled {} (p {:id "id"} "body")))
  (binding [*ns-filter* "taoensso.*"] (profiled {} (p {:id "id"} "body"))))

;;;; Combo filtering

(let [valid-min-level valid-min-level
      ns->?min-level  ns->?min-level]

  (defn- get-min-level [default x ns]
    (valid-min-level
      (or
        (if (vector? x) (ns->?min-level x ns) x)
        default))))

(comment
  (get-min-level 6 [["foo" 2]] *ns*) ; Default to 6 (don't profile)
  (let [ns *ns*] (enc/qb 1e6 (get-min-level *min-level* ns))) ; 260.34
  (binding [*min-level* [["taoensso.*" 1] ["*" 4]]] (get-min-level "foo")))

(let [valid-call-level valid-call-level
      may-profile-ns?  may-profile-ns?
      get-min-level    get-min-level]

  (defn #?(:clj may-profile? :cljs ^boolean may-profile?)
    "Implementation detail.
    Returns true iff level and ns are runtime unfiltered."
    ([level   ] (may-profile? level *ns*))
    ([level ns]
     (if (>= ^long (valid-call-level     level)
             (long (get-min-level 6 *min-level* ns)))
       (if (may-profile-ns? *ns-filter* ns) true false)
       false))))

(comment
  (enc/qb 1e6 (may-profile? 2)) ; 468.24

  (binding [*min-level* [["foo.bar" 1] ["*" 3]]
            *ns-filter* "*"]
    (may-profile? 2 "baz")))

;;;; Compile-time filtering

#?(:clj
   (def ^:private compile-time-min-level
     (when-let [level (enc/get-env {:as :edn, :spec [:taoensso.tufte.min-level.edn :taoensso.tufte.min-level :tufte.min-level]})]
       (valid-min-level level)
       (do              level))))

#?(:clj
   (def ^:private compile-time-ns-filter
     (let [ns-pattern (enc/get-env {:as :edn, :spec [:taoensso.tufte.ns-pattern.edn :taoensso.tufte.ns-pattern :tufte.ns-pattern]})]
       (or ns-pattern "*"))))

#?(:clj
   (defn -elide?
     "Returns true iff level or ns are compile-time filtered.
     Called only at macro-expansiom time."
     [level-form ns-str-form]
     (not
       (and
         (or ; Level okay
           (nil? compile-time-min-level)
           (not (valid-call-level? level-form)) ; Not a compile-time level const
           (>= (long level-form) (long (get-min-level 6 compile-time-min-level ns-str-form))))

         (or ; Namespace okay
           (not (string? ns-str-form)) ; Not a compile-time ns-str const
           (may-profile-ns? compile-time-ns-filter ns-str-form))))))

;;;; Low-level primitives

#?(:clj (defn- valid-opts!        [caller x] (if (map? x)            x (truss/ex-info! (str caller " opts must be a const (compile-time) map")              (enc/typed-val x)))))
#?(:clj (defn- valid-opt:dynamic! [caller x] (if (enc/const-form? x) x (truss/ex-info! (str caller " `:dynamic?` opt must be a const (compile-time) value") (enc/typed-val x)))))

(defn profiling? "Returns e/o #{nil :thread :dynamic}."
  [] (if impl/*pdata* :dynamic (when (impl/pdata-local-get) :thread)))

(comment (enc/qb 1e6 (profiling?))) ; 43.91

(def ^:const ^:private default-nmax (long 8e5))
(defn new-pdata
  "Low-level primitive for advanced users.
  Returns a new pdata object for use with `with-profiling` and/or `capture-time!`.
  Deref to get pstats:

    (let [pd (new-pdata)
          t0 (System/nanoTime)]
      (with-profiling pd {}
        (p :foo (Thread/sleep 100))
        (capture-time! pd :bar (- t0 (System/nanoTime))))
      (deref pd))

  Dynamic (thread-safe) by default.
  *WARNING*: don't change this default unless you're very sure the resulting
  pdata object will not be concurrently modified across threads. Concurrent
  modification will lead to bad data and/or exceptions!"
  ([] (new-pdata nil))
  ([{:keys [dynamic? nmax] :or {dynamic? true, nmax default-nmax}}]
   (if dynamic?
     (impl/new-pdata-dynamic nmax)
     (impl/new-pdata-local   nmax)))

  ([dynamic? nmax]
   (if dynamic?
     (impl/new-pdata-dynamic (or nmax default-nmax))
     (impl/new-pdata-local   (or nmax default-nmax)))))

(comment (let [pd (new-pdata)] [(enc/qb 1e6 (pd :foo 100 nil)) @(pd)])) ; 116

#?(:clj
   (defmacro with-profiling
     "Low-level primitive for advanced users.
     Executes body with profiling active, and returns <body-result>.

     If `:dynamic?` is false (default), body's evaluation MUST begin
     and end without interruption on the same thread. This means that
     body CANNOT contain any parking IoC style (e.g. `core.async`)
     macros.

     See `new-pdata` for more info on low-level primitives."
     [pdata {:keys [dynamic?]} & body]
     (valid-opt:dynamic! 'tufte/with-profiling dynamic?)
     (if dynamic?
       `(binding [impl/*pdata* ~pdata] (do ~@body))
       `(binding [impl/*pdata*    nil] ; Ensure no dynamic parent (=>nesting) steals local captures
          (try
            (impl/pdata-local-push ~pdata)
            (do ~@body)
            (finally (impl/pdata-local-pop)))))))

#?(:clj
   (defmacro ^:private profiled*
     "Unconditionally returns [<body-result> <pstats>]."
     [caller dynamic? nmax run-form]
     (valid-opt:dynamic! caller dynamic?)
     (if dynamic?
       `(let [pd# (impl/new-pdata-dynamic (or ~nmax default-nmax))] (binding [impl/*pdata* pd#] [~run-form @pd#]))
       `(let [pd# (impl/new-pdata-local   (or ~nmax default-nmax))]
          (binding [impl/*pdata* nil]
            (try
              (impl/pdata-local-push pd#)
              [~run-form @pd#]
              (finally (impl/pdata-local-pop))))))))

(comment
  @((new-pdata))
  (let [pd (new-pdata)
        t0 (System/nanoTime)]
    ;; Dynamic pdata with non-dynamic `with-profiling` is fine:
    (with-profiling pd {}
      (pd :id1 100 nil)
      (pd :id2 250 nil))
    @(pd))

  (profiled* 'tufte/caller true nil (do (Thread/sleep 1000) :return-val)))

#?(:clj
   (defmacro capture-time!
     "Low-level primitive for advanced users.
     Useful when tracking time across thread boundaries and/or for
     async jobs / callbacks / etc.

     See `new-pdata` for more info on low-level primitives.
     See also `capture-time!*`."
     ([pdata id nano-secs-elapsed] `(~pdata ~id ~nano-secs-elapsed ~(dissoc (enc/get-source &form &env) :file)))
     ([      id nano-secs-elapsed]
      `(when-let [pd# (or impl/*pdata* (impl/pdata-local-get))]
         (pd# ~id ~nano-secs-elapsed ~(dissoc (enc/get-source &form &env) :file))))))

(defn capture-time!*
  "Like `capture-time!` but a function and does not collect callsite location info."
  ([pdata id nano-secs-elapsed] (pdata id nano-secs-elapsed nil))
  ([      id nano-secs-elapsed]
   (when-let [pd (or impl/*pdata* (impl/pdata-local-get))]
     (pd id nano-secs-elapsed nil))))

(comment
  (let [pd (new-pdata)]
    (with-profiling pd {}
      (let [t0 (System/nanoTime)
            _  (Thread/sleep 2200)
            t1 (System/nanoTime)]
        (capture-time! :foo (- t1 t0))))
    @(pd)))

;;;; Main macros

#?(:clj
   (defmacro p
     "Profiling spy.

     Use this to wrap forms that should be timed during profiling:
       - Always executes body and returns <body-result>.
       - When profiling is active (via `profiled` or `profile`),
         records body's execution time.

     Options include:
      `:id`    - Form id for this body in stats output (e.g. `::my-fn-call`)
      `:level` - e/o #{0 1 2 3 4 5} ; Default is `5`"

     {:arglists '([id & body] [opts & body])}
     [s1 & body]
     (let [ns-str  (str *ns*)
           opts    (if (map? s1) s1 {:level 5 :id s1})
           level   (get opts :level)
           id-form (get opts :id)
           loc (or (get opts :loc) (dissoc (enc/get-source &form &env) :file))]

       ;; If *any* level is present, it must be a valid compile-time level
       ;; since this macro doesn't offer runtime level checking
       (when level (valid-call-level level))

       (when (nil? id-form)
         (truss/ex-info! "`tufte/p` requires an id."
           {:loc  loc
            :opts opts
            :form (cons 'p (cons s1 body))}))

       (if (-elide? level ns-str)
         `(do ~@body)
         `(if-let [pd#     (or impl/*pdata* (impl/pdata-local-get))]
            (let  [t0#     (enc/now-nano*)
                   result# (do ~@body)
                   t1#     (enc/now-nano*)]
              ;; Note that capture cost is excluded from p time
              (pd# ~id-form (- t1# t0#) ~loc)
              result#)
            (do ~@body))))))

(comment
  (macroexpand '(p :foo "hello"))
  (let [pd (new-pdata)]
    (with-profiling pd {}
      (p :foo (Thread/sleep 100))
      (p :bar (Thread/sleep 200)))
    @(pd)))

(defn- valid-compile-time-opts [dynamic? nmax]
  (when-not (contains? #{false true} dynamic?) (truss/ex-info! "[profile/d] `:dynamic?` opt must be compile-time bool value" {:value dynamic?}))
  (when-not (integer? nmax)                    (truss/ex-info! "[profile/d] `:nmax` opt must be compile-time integer value"  {:value nmax})))

(comment (valid-compile-time-opts 'sym 'sym))

#?(:clj
   (defmacro profiled
     "Always executes body, and always returns [<body-result> <?pstats>].

     When [ns level] unelided and [ns level `when`] unfiltered, executes body
     with profiling active.

     Handy if you'd like to consume stats output directly.
     Otherwise see `profile`.

     `pstats` objects are derefable and mergeable:
       - @pstats                 => {:clock {:keys [t0 t1 total]}, :stats {<id> {:keys [n sum ...]}}}
       - @(merge-pstats ps1 ps2) => {:clock {:keys [t0 t1 total]}, :stats {<id> {:keys [n sum ...]}}}

     Full set of keys in above `:stats` maps:
       :n :min :max :mean :mad :sum :p25 :p50 :p75 :p90 :p95 :p99 :loc :last

       All values are numerical (longs or doubles), except for `:loc` which
       is a map of `p` callsite location information, or set of such maps, e.g.:
         {:ns \"my-ns\", :file \"/tmp/my-ns.clj\", :line 122, :column 21}

     Compile-time opts:
       `:dynamic?` - Use multi-threaded profiling? ; Default is `false`
       `:nmax` ----- ~Max captures per id before compaction ; Default is 8e5

     Runtime opts:
       `:level` ---- e/o #{0 1 2 3 4 5} ; Default is `5`
       `:when` ----- Optional arbitrary conditional form (e.g. boolean expr)

     Laziness in body:
       Lazy seqs and other forms of laziness (e.g. delays) in body will only
       contribute to profiling results if/when EVALUATION ACTUALLY OCCURS.
       This is intentional and a useful property. Compare:

         (profiled {}  (delay (Thread/sleep 2000))) ; Doesn't count sleep
         (profiled {} @(delay (Thread/sleep 2000))) ; Does    count sleep

     Async code in body:
       Execution time of any code in body that runs asynchronously on a
       different thread will generally NOT be automatically captured by default.

       `:dynamic?` can be used to support capture in cases where Clojure's
       binding conveyance applies (e.g. futures, agents, pmap). Just make sure
       that all work you want to capture has COMPLETED before the `profiled`
       form ends- for example, by blocking on pending futures.

       In other advanced cases (notably core.async `go` blocks), please see
       `with-profiling` and `capture-time!`.

     `core.async` warning:
        `core.async` code can be difficult to profile correctly without a deep
        understanding of precisely what it's doing under-the-covers.

        Some general recommendations that can help keep things simple:

          - Try minimize the amount of code + logic in `go` blocks. Use `go`
            blocks for un/parking to get the data you need, then pass the data
            to external fns. Profile these fns (or in these fns), not in your
            `go` blocks.

          - In particular: you MUST NEVER have parking calls inside
            `(profiled {:dynamic? false} ...)`.

            This can lead to concurrency exceptions.

            If you must profile code within a go block, and you really want to
            include un/parking times, use `(profiled {:dynamic? true} ...)`
            instead."

     [opts & body]
     (let [ns-str (str *ns*)]

       (when-not (map? opts)
         (truss/ex-info! "`tufte/profiled` requires a compile-time map as first arg."
           {:ns-str ns-str :line (:line (meta &form))
            :form (cons 'profiled (cons opts body))}))

       (let [level-form (get opts :level    5)
             dynamic?   (get opts :dynamic? false)
             test-form  (get opts :when     true)
             nmax       (get opts :nmax     default-nmax)

             _ (valid-compile-time-opts dynamic? nmax)
             nmax (long nmax)]

         (when (integer? level-form) (valid-call-level level-form))

         (if (-elide? level-form ns-str)
           `[(do ~@body)]
           (let [runtime-check
                 (if (= test-form true) ; Common case
                        `(may-profile? ~level-form ~ns-str)
                   `(and (may-profile? ~level-form ~ns-str) ~test-form))]

             (if dynamic?
               `(if ~runtime-check
                  (let [pd# (impl/new-pdata-dynamic ~nmax)]
                    (binding [impl/*pdata* pd#]
                      [(do ~@body) @pd#]))
                  [(do ~@body)])

               `(if ~runtime-check
                  (let [pd# (impl/new-pdata-local ~nmax)]
                    (binding [impl/*pdata* nil] ; Ensure no dynamic parent (=>nesting) steals local captures
                      (try
                        (impl/pdata-local-push pd#)
                        [(do ~@body) @pd#]
                        (finally (impl/pdata-local-pop)))))
                  [(do ~@body)]))))))))

(comment
  (enc/qb 1e6 ; [463.23 560.09]
    (profiled {})
    (profiled {} 2 (p :p1)))

  (profiled {} (p :p1))
  (profiled {} (p {:level 5 :id :p1}))
  (profiled {} (p (let [x :foo/id] x) "body"))
  (profiled {:level 2 :when (chance 0.5)} (p :p1 "body"))
  (profiled {} (p :foo (p :bar))))

(defrecord HandlerVal [ns-str level ?id ?data pstats pstats-str_ ?file ?line])

#?(:clj
   (defmacro profile
     "Always executes body, and always returns <body-result>.

     When [ns level] unelided and [ns level `when`] unfiltered, executes body
     with profiling active and dispatches stats to any registered handlers
     (see `add-handler!`).

     Handy if you'd like to consume/aggregate stats output later/elsewhere.
     Otherwise see `profiled`.

     Compile-time opts:
       `:dynamic?` - Use multi-threaded profiling? ; Default is `false`
       `:nmax` ----- ~Max captures per id before compaction ; Default is 8e5

     Runtime opts:
       `:level` ---- e/o #{0 1 2 3 4 5} ; Default is `5`
       `:when` ----- Optional arbitrary conditional form (e.g. boolean expr)
       `:id` ------- Optional profiling id provided to handlers (e.g. `::my-stats-1`)
       `:data` ----- Optional arbitrary data provided to handlers

     Laziness in body:
       Lazy seqs and other forms of laziness (e.g. delays) in body will only
       contribute to profiling results if/when EVALUATION ACTUALLY OCCURS.
       This is intentional and a useful property. Compare:

         (profiled {}  (delay (Thread/sleep 2000))) ; Doesn't count sleep
         (profiled {} @(delay (Thread/sleep 2000))) ; Does    count sleep

     Async code in body:
       Execution time of any code in body that runs asynchronously on a
       different thread will generally NOT be automatically captured by default.

       `:dynamic?` can be used to support capture in cases where Clojure's
       binding conveyance applies (e.g. futures, agents, pmap). Just make sure
       that all work you want to capture has COMPLETED before the `profiled`
       form ends- for example, by blocking on pending futures.

       In other advanced cases (notably core.async `go` blocks), please see
       `with-profiling` and `capture-time!`.

     `core.async` warning:
        `core.async` code can be difficult to profile correctly without a deep
        understanding of precisely what it's doing under-the-covers.

        Some general recommendations that can help keep things simple:

          - Try minimize the amount of code + logic in `go` blocks. Use `go`
            blocks for un/parking to get the data you need, then pass the data
            to external fns. Profile these fns (or in these fns), not in your
            `go` blocks.

          - In particular: you MUST NEVER have parking calls inside
            `(profiled {:dynamic? false} ...)`.

            This can lead to concurrency exceptions.

            If you must profile code within a go block, and you really want to
            include un/parking times, use `(profiled {:dynamic? true} ...)`
            instead."

     [opts & body]
     (let [ns-str (str *ns*)]

       (when-not (map? opts)
         (truss/ex-info! "`tufte/profile` requires a compile-time map as first arg."
           {:ns-str ns-str :line (:line (meta &form))
            :form (cons 'profile (cons opts body))}))

       (let [level-form (get opts :level 5)
             id-form    (get opts :id)
             data-form  (get opts :data)]

         (when (integer? level-form) (valid-call-level level-form))

         `(let [[result# pstats#] (profiled ~opts ~@body)]
            (when pstats#
              (impl/handle!
                (HandlerVal. ~ns-str ~level-form ~id-form ~data-form
                  pstats# (delay (format-pstats pstats#))
                  ~*file* ~(:line (meta &form)))))
            result#)))))

(comment (profile {:id ::my-id} (p :p1 "body")))

;;;; Output handlers
;; Handlers are used for `profile` output, let us nicely decouple stat
;; creation and consumption.

(def         handlers_ "{<handler-id> <handler-fn>}" impl/handlers_)
(defn remove-handler! [handler-id] (set (keys (swap! handlers_ dissoc handler-id))))
(defn    add-handler!
  "Use this to register interest in stats output produced by `profile` calls.
  Each registered `handler-fn` will be called as:

    (handler-fn {:ns-str _ :level _ :?id _ :?data _ :pstats _ :pstats-str_ _})

  Map args:
    `:ns-str` ------ Namespace string where `profile` call took place
    `:level` ------- Level e/o #{0 1 2 3 4 5}, given in `(profile {:level _} ...)`
    `:?id` --------- Optional profiling id,    given in `(profile {:id    _} ...)`
    `:?data` ------- Optional arb data,        given in `(profile {:data  _} ...)`
    `:pstats` ------ As in `(second (profiled ...))`. Derefable, mergeable.
    `:pstats-str_` - (delay (format-pstats pstats))

  Error handling (NB):
    Handler errors will be silently swallowed. Please `try`/`catch` and
    appropriately deal with (e.g. log) possible errors *within* `handler-fn`.

  Async/blocking:
    `handler-fn` should ideally be non-blocking, or reasonably cheap. Handler
     dispatch occurs through a 1-thread 1k-buffer dropping queue.

  Ns filtering:
    Provide an optional `ns-pattern` arg to only call handler for matching
    namespaces. See `*ns-filter*` for example patterns.

  Handler ideas:
    Save to a db, log, `put!` to an appropriate `core.async` channel, filter,
    aggregate, use for a realtime analytics dashboard, examine for outliers
    or unexpected output, ..."

  ([handler-id handler-fn] (add-handler! handler-id nil handler-fn))
  ([handler-id ns-pattern handler-fn]
   (let [f
         (if (or (nil? ns-pattern) (= ns-pattern "*"))
           handler-fn
           (let [nsf? (enc/name-filter ns-pattern)]
             (fn [m]
               (when (nsf? (get m :ns-str))
                 (handler-fn m)))))]

     (set (keys (swap! handlers_ assoc handler-id f))))))

(declare format-pstats)

(defn add-basic-println-handler!
  "Adds a simple handler that logs `profile` stats output with `println`."
  [{:keys [ns-pattern handler-id format-pstats-opts]
    :or   {ns-pattern "*"
           handler-id :basic-println}}]

  (add-handler! handler-id ns-pattern
    (fn [{:keys [?id ?data pstats]}]
      (println
        (str
          (when ?id   (str "\nid: "   ?id))
          (when ?data (str "\ndata: " ?data))
          "\n" (format-pstats pstats format-pstats-opts))))))

;;;; Public user utils

(enc/defaliases
  enc/chance
  impl/merge-pstats
  impl/format-pstats
  impl/format-grouped-pstats)

(comment
  (let [[_ ps1] (profiled {} (p :p1))
        [_ ps2] (profiled {} (p :p1))]
    (enc/qb 1e5 (merge-pstats ps1 ps2))) ; 74.38

  (println
    (str "\n"
      (format-pstats
        (second
          (profiled {}
            (p :foo (Thread/sleep 200))
            (p :bar (Thread/sleep 500))
            (do     (Thread/sleep 800))))
        {:columns [:clock :p50 :p95]}))))

(defn format-id-abbr-fn
  "Returns a cached (fn [id]) => abbreviated id with at most `n-full`
  unabbreviated namespace parts.

  Example:
    ((format-id-abbr 0)  :foo.bar/baz)   => :f.b/baz
    ((format-id-abbr 1)  'foo.bar/baz)   => 'f.bar/baz
    ((format-id-abbr 2) \"foo.bar/baz\") => \"foo.bar/baz\""

  ([      ] (format-id-abbr-fn 1))
  ([n-full] (enc/fmemoize (partial enc/abbreviate-ns n-full))))

(defn compile-ns-filter "Wraps `taoensso.encore/name-filter`."
  [ns-pattern] (enc/name-filter ns-pattern))

#?(:clj
   (defmacro refer-tufte
          "(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])"
     [] `(require '~'[taoensso.tufte :as tufte :refer [defnp p profiled profile]])))

(comment (refer-tufte))

;;;; fnp stuff

(defn- fn-sigs [def? ?meta-id ?fn-sym sigs location]
  (let [single-arity?   (vector? (first sigs))
        sigs   (if single-arity? (list  sigs) sigs)
        fn-sym (or ?fn-sym (gensym))

        base-id
        (if ?meta-id
          (enc/as-qname ?meta-id)
          (str *ns* "/"
            (if     ?fn-sym
              (name ?fn-sym)
              (str (if def? "defn_" "fn_") (name fn-sym)))))

        get-ids
        (if single-arity?
          (fn [fn-sym _params] [(keyword      base-id)])
          (fn [fn-sym  params] [(keyword      base-id)
                                (keyword (str base-id "_" (count params)))]))

        new-sigs
        (map
          (fn [[params & others]]
            (let [has-prepost-map?      (and (map? (first others)) (next others))
                  [?prepost-map & body] (if has-prepost-map? others (cons nil others))
                  [base-id ?arity-id]   (get-ids fn-sym params)]

              (if ?prepost-map
                (if-let [arity-id ?arity-id]
                  `(~params ~?prepost-map (p {:id ~base-id, :loc ~location} (p {:id ~arity-id, :loc ~location} ~@body)))
                  `(~params ~?prepost-map (p {:id ~base-id, :loc ~location}                                    ~@body)))

                (if-let [arity-id ?arity-id]
                  `(~params               (p {:id ~base-id, :loc ~location} (p {:id ~arity-id, :loc ~location} ~@body)))
                  `(~params               (p {:id ~base-id, :loc ~location}                                    ~@body))))))
          sigs)]

    new-sigs))

#?(:clj
   (defmacro fnp
     "Like `fn` but wraps fn bodies with `p` macro."
     {:arglists '([name?  [params*] prepost-map? body]
                  [name? ([params*] prepost-map? body)+])}
     [& sigs]
     (let [[?fn-sym sigs] (if (symbol? (first sigs)) [(first sigs) (next sigs)] [nil sigs])
           new-sigs       (fn-sigs (not :def) (:tufte/id (meta ?fn-sym)) ?fn-sym sigs
                            (dissoc (enc/get-source &form &env) :file))]
       (if ?fn-sym
         `(fn ~?fn-sym ~@new-sigs)
         `(fn          ~@new-sigs)))))

(comment
  (fn-sigs "foo"       '([x]            (* x x)))
  (macroexpand '(fnp     [x]            (* x x)))
  (macroexpand '(fn      [x]            (* x x)))
  (macroexpand '(fnp bob [x] {:pre [x]} (* x x)))
  (macroexpand '(fn      [x] {:pre [x]} (* x x)))
  (macroexpand '(fnp   ^{:tufte/id "foo/bar"} bob  [x]))
  (macroexpand '(defnp ^{:tufte/id "foo/bar"} bob ([x]) ([x y])))
  (macroexpand '(defnp                        bob ([x]) ([x y]))))

#?(:clj
   (defmacro defnp
     "Like `defn` but wraps fn bodies with `p` macro."
     {:arglists
      '([name doc-string? attr-map?  [params*] prepost-map? body]
        [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
     [& sigs]
     (let [[fn-sym sigs] (enc/name-with-attrs (first sigs) (next sigs))
           new-sigs      (fn-sigs :def (:tufte/id (meta fn-sym)) fn-sym sigs
                           (dissoc (enc/get-source &form &env) :file))]
       `(defn ~fn-sym ~@new-sigs))))

#?(:clj
   (defmacro defnp-
     "Like `defn-` but wraps fn bodies with `p` macro."
     {:arglists
      '([name doc-string? attr-map?  [params*] prepost-map? body]
        [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
     [& sigs]
     (let [[fn-sym sigs] (enc/name-with-attrs (first sigs) (next sigs) {:private true})
           new-sigs      (fn-sigs :def (get (meta fn-sym) :tufte/id) fn-sym sigs
                           (dissoc (enc/get-source &form &env) :file))]
       `(defn ~fn-sym ~@new-sigs))))

(comment
  (defnp foo "Docstring"                [x]   (* x x))
  (macroexpand '(defnp foo "Docstring"  [x]   (* x x)))
  (macroexpand '(defn  foo "Docstring"  [x]   (* x x)))
  (macroexpand '(defnp foo "Docstring" ([x]   (* x x))
                                       ([x y] (* x y))))
  (profiled {} (foo 5)))

;;;; StatsAccumulator

(deftype StatsAccumulator [acc]
  ;; `acc` - (latom {<profiling-id> <pstats>})
  #?(:clj clojure.lang.IDeref :cljs IDeref) (#?(:clj deref  :cljs -deref)  [_] (enc/reset-in! acc {})) ; Drain
  #?(:clj clojure.lang.IFn    :cljs IFn)    (#?(:clj invoke :cljs -invoke) [_] (enc/reset-in! acc {})) ; Drain
  (                                          #?(:clj invoke :cljs -invoke) [_ profiling-id ps]
   (when (and profiling-id ps)
     ;; Contention would be expensive so consumer should serialize calls
     (acc profiling-id #(impl/merge-pstats % ps))
     true)))

(defn stats-accumulator
  "Experimental, subject to change. Feedback welcome!
  Small util to help merge `pstats` from multiple runs and/or threads.

  Returns a stateful `StatsAccumulator` (`sacc`) with:
    - (sacc <profiling-id> <pstats>) ; Merges given pstats under given profile id
    - @sacc                          ; Drains accumulator and returns drained
                                     ; {<profiling-id> <merged-pstats>}

  Note that for performance reasons, you'll likely want some kind of
  async/buffer/serialization mechanism in front of merge calls.

  One common pattern using `handler:accumulating` is to create a
  system-wide accumulator that you deref every n minutes/etc. to get
  a view of system-wide performance over the period, e.g.:

    (defonce my-sacc (stats-accumulator) ; Create an accumulator
    (add-handler! :my-sacc (handler:accumulating my-sacc)) ; Register handler

    (defonce my-sacc-drainer
      ;; Drain and print formatted stats every minute
      (future
        (while true
          (when-let [m (not-empty @my-sacc)]
            (println (format-grouped-pstats m)))
          (Thread/sleep 60000))))

    (profile ...) ; Used elsewhere in your application, e.g.
                  ; wrapping relevant Ring routes in a web application.

  See example clj project for more details."
  [] (StatsAccumulator. (enc/latom {})))

(comment
  (enc/qb 1e6 (stats-accumulator)) ; 45.37
  (let [sacc  (stats-accumulator)]
    (sacc :profiled1 (second (profiled {} (p :p1 nil))))
    (Thread/sleep 100)
    (sacc :profiled2 (second (profiled {} (p :p2 nil))))
    [@sacc @sacc]))

(defn add-accumulating-handler!
  "Alpha, subject to change.

  Creates a new StatsAccumulator (and agent in clj), then
  registers a handler to accumulate `profile` output to the
  StatsAccumulator using the agent.

  Returns the StatsAccumulator. You can deref the result to
  drain the accumulator and return {<group-id> <merged-pstats>}.

  One common pattern is to deref the accumulator every n
  minutes/etc. to get a view of total-system performance over
  the period, e.g.:

  (defonce my-sacc (add-accumulating-handler! {:ns-pattern \"*\"}))
  (defonce my-sacc-drainer
    ;; Will drain and print formatted stats every minute
    (future
      (while true
        (when-let [m (not-empty @my-sacc)]
          (println (format-grouped-pstats m)))
        (Thread/sleep 60000))))

  (profile ...) ; Used elsewhere in your application, e.g.
                ; wrapping relevant Ring routes in a web application.

  See also `format-grouped-pstats`, example clj project."

  [{:keys [ns-pattern handler-id]
    :or   {ns-pattern "*"
           handler-id :accumulating}}]

  (let [sacc   (stats-accumulator)
        agent_ #?(:clj (delay (agent nil :error-mode :continue)) :cljs nil)]

    (add-handler! handler-id ns-pattern
      (fn [{:keys [?id ?data pstats]}]
        (let [id (or ?id :tufte/nil-id)]
          #?(:clj (send @agent_ (fn [_] (sacc id pstats)))
             :cljs                      (sacc id pstats)))))

    sacc))

(comment
  (def my-sacc (add-accumulating-handler! {:ns-pattern "*"}))

  (do
    (future (profile {}         (p :p1 (Thread/sleep 900))))
    (future (profile {:id :foo} (p :p1 (Thread/sleep 900))))
    (future (profile {:id :bar} (p :p1 (Thread/sleep 500)))))

  (println
    (format-grouped-pstats @my-sacc
      {:format-pstats-opts {:columns [:n]}})))

;;;; Deprecated

(enc/deprecated
  #?(:clj (defmacro ^:no-doc ^:deprecated with-min-level "Prefer `binding`." [level & body] `(binding [*min-level* ~level] ~@body)))
  (defn             ^:no-doc ^:deprecated set-min-level! "Prefer `alter-var-root`." [level]
    #?(:cljs (set!             *min-level*         level)
       :clj  (alter-var-root #'*min-level* (fn [_] level))))

  #?(:clj (defmacro ^:no-doc ^:deprecated with-ns-pattern  "Prefer `binding`." [ns-pattern & body] `(binding [*ns-filter* ~ns-pattern] ~@body)))
  (defn             ^:no-doc ^:deprecated  set-ns-pattern! "Prefer `alter-var-root`." [ns-pattern]
    #?(:cljs (set!             *ns-filter*         ns-pattern)
       :clj  (alter-var-root #'*ns-filter* (fn [_] ns-pattern)))))

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

  (profile {:level 2 :id ::sleepy :data "foo"}    (sleepy-threads))
  (profile {:level 2 :id ::sleepy :dynamic? true} (sleepy-threads))
  (p :hello "Hello, this is a result") ; Falls through (no pdata context)

  (defnp arithmetic []
    (let [nums (vec (range 1000))]
      (+ (p :fast-sleep (Thread/sleep 1) 10)
         (p :slow-sleep (Thread/sleep 2) 32)
         (p :add  (reduce + nums))
         (p :sub  (reduce - nums))
         (p :mult (reduce * nums))
         (p :div  (reduce / nums)))))

  (profiled {} "foo")
  (profile  {} (dotimes [n 100] (arithmetic)))
  (profile  {} (dotimes [n 1e5] (p :p1 nil)))
  (profile  {} (dotimes [n 1e6] (p :p1 nil)))
  (profiled {} (dotimes [n 1e6] (p :p1 nil)))
  (profiled {:level 2 :when (chance 0.5)} "body")

  @(second (profiled {:nmax 10000 :dynamic? true} (dotimes [n 200] (p :p1 nil))))

  (profile {})
  (profile {:nmax 10}                (dotimes [n 200] (p :p1 nil)))
  (profile {:nmax 10 :dynamic? true} (dotimes [n 200] (p :p1 nil)))
  (profile {}
    (p :foo
      (do       (Thread/sleep 100))
      (p :foo/a (Thread/sleep 120))
      (p :foo/b (Thread/sleep 220))))

  (println "\n" (format-pstats (second (profiled {} (p :p1 (p :p2 (p :p3 "foo")))))))
  (println "\n"
    (time
      (format-pstats
        @(let [[_ ps0] (profiled {} (dotimes [_ 1e6] (p :foo  "foo")))
               [_ ps1] (profiled {} (dotimes [_ 1e6] (p :foo  "foo")))
               [_ ps2] (profiled {} (dotimes [_ 500] (p ::bar "bar")))]
           (reduce (partial impl/merge-pstats 1e4) [ps0 ps1 ps2])))))

  (println
    (format-pstats
      (second
        (profiled {} (p :foo (Thread/sleep 100)))))))

;;;; Deprecated

(enc/deprecated
  #?(:clj (defmacro ^:no-doc pspy            {:deprecated "vX.Y.Z (YYYY-MM-DD)" :doc "Prefer `p`."} [& args] (truss/keep-callsite `(p ~@args))))
  (enc/def*         ^:no-doc format-id-abbr  {:deprecated "vX.Y.Z (YYYY-MM-DD)" :doc "Prefer `format-id-abbr-fn`."} format-id-abbr-fn))

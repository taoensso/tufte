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

    (add-handler! [handler-id handler-fn dispatch-opts])

  How/where to use this library:
    Tufte profiling is highly optimized: even without elision, you can usually
    leave profiling active in production (e.g. for sampled profiling, or to
    detect unusual performance behaviour). Tufte's pstats data is well suited
    to programmatic monitoring."

  {:author "Peter Taoussanis (@ptaoussanis)"}
  (:require
   [taoensso.encore             :as enc :refer [have have?]]
   [taoensso.encore.signals     :as sigs]
   [taoensso.encore.signals.api :as sigs-api]
   [taoensso.tufte.stats        :as stats]
   [taoensso.tufte.impl         :as impl
    #?@(:cljs [:refer [PStats ProfilingSignal]])])

  #?(:clj  (:import [taoensso.tufte.impl PStats ProfilingSignal]))
  #?(:cljs (:require-macros [taoensso.tufte :refer [profiled]])))

(enc/assert-min-encore-version [3 68 0])
(enc/require-telemere-if-present) ; For `telemere-handler`

(comment (remove-ns 'taoensso.tufte))

;;;; Shared signal API

(sigs-api/def-api 3 impl/*rt-sig-filter* impl/*sig-handlers* {:purpose "profiling"})

(comment
  [level-aliases]
  [handlers-help get-handlers add-handler! remove-handler!]
  [filtering-help
   set-kind-filter! set-ns-filter! set-id-filter! set-min-level!
   with-kind-filter with-ns-filter with-id-filter with-min-level])

;;;; Low-level primitives

(defn profiling? "Returns e/o #{nil :thread :dynamic}."
  [] (if impl/*pdata* :dynamic (when (impl/pdata-local-get) :thread)))

(comment (enc/qb 1e6 (profiling?))) ; 70.66

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
  ([{:keys [dynamic? nmax] :or {dynamic? true nmax default-nmax}}]
   (if dynamic?
     (impl/new-pdata-dynamic nmax)
     (impl/new-pdata-local   nmax))))

(comment (let [pd (new-pdata)] [(enc/qb 1e6 (pd :foo 100 nil)) @(pd)])) ; 155

#?(:clj
   (defmacro with-profiling
     "Low-level primitive for advanced users.
     Executes body with profiling active, and returns <body-result>.

     If `:dynamic?` is false (default), body's evaluation MUST begin
     and end without interruption on the same thread. In particular
     this means that body MUST NOT contain any parking `core.async`
     calls.

     See `new-pdata` for more info on low-level primitives."
     [pdata {:keys [dynamic? nmax] :or {nmax default-nmax}} & body]
     (if dynamic?
       `(binding [impl/*pdata* ~pdata] (do ~@body))
       `(try
          (impl/pdata-local-push ~pdata)
          (do ~@body)
          (finally (impl/pdata-local-pop))))))

(comment
  @((new-pdata))
  (let [pd (new-pdata)
        t0 (System/nanoTime)]
    ;; Dynamic pdata with non-dynamic `with-profiling` is fine:
    (with-profiling pd {}
      (pd :id1 100 nil)
      (pd :id2 250 nil))
    @(pd)))

#?(:clj
   (defmacro capture-time!
     "Low-level primitive for advanced users.
     Useful when tracking time across thread boundaries and/or for
     async jobs / callbacks / etc.

     See `new-pdata` for more info on low-level primitives.
     See also `capture-time!*`."
     ([pdata id nano-secs-elapsed] `(~pdata ~id ~nano-secs-elapsed ~(enc/get-source &form &env)))
     ([      id nano-secs-elapsed]
      `(when-let [~'pd (or impl/*pdata* (impl/pdata-local-get))]
         (~'pd ~id ~nano-secs-elapsed ~(enc/get-source &form &env))))))

(defn capture-time!*
  "Like `capture-time!` but: a function, and does not collect callsite location info."
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
       `:id`    - Form id for this body in pstats (e.g. `::my-fn-call`)
       `:level` - Integer (default 5)"

     {:arglists '([id & body] [{:keys [id level]} & body])}
     [s1 & body]
     (let [opts       (if (map? s1) s1 {:id s1})
           level-form (get opts :level 5)
           id-form    (get opts :id)
           loc    (or (get opts :loc) (enc/get-source &form &env))]

       ;; If level is present, it must be a valid compile-time level
       ;; since this macro doesn't offer runtime level checking
       (when level-form (sigs/valid-level level-form))

       (when (nil? id-form)
         (throw
           (ex-info "`tufte/p` requires an id."
             {:loc loc, :opts opts, :form `(~'p s1 ~@body)})))

       (if-let [elide? (when-let [sf impl/ct-sig-filter] (not (sf (str *ns*) level-form)))]
         `(do ~@body)
         ;; Note no rt-sig-filter check
         `(let [~'__pd-dynamic impl/*pdata*]
            (if-let [~'__pd (or ~'__pd-dynamic (impl/pdata-local-get))]
              (let [~'__t0     (enc/now-nano*)
                    ~'__result (do ~@body)
                    ~'__t1     (enc/now-nano*)]
                ;; Note that capture cost is excluded from p time
                (~'__pd ~id-form (- ~'__t1 ~'__t0) ~loc )
                ~'__result)
              (do ~@body)))))))

(comment
  (let [pd (new-pdata)]
    (with-profiling pd {}
      (p :foo (Thread/sleep 100))
      (p :bar (Thread/sleep 200)))
    @(pd)))

#?(:clj
   (let [default-opts {:dynamic? false, :nmax default-nmax, :level 5}]
     (defn- valid-profiling-opts [loc opts form]
       (let [caller (str "`" (first form) "`")
             _
             (when-not (map? opts)
               (throw
                 (ex-info (str caller " requires a compile-time map as first arg.")
                   {:loc loc, :opts opts, :form form})))

             opts (merge default-opts opts)
             {:keys [dynamic? nmax]} opts]

         (when-not (contains? #{false true} dynamic?) (throw (ex-info (str caller " `:dynamic?` opt must be compile-time boolean") {:value dynamic? :type (type dynamic?)})))
         (when-not (integer?                    nmax) (throw (ex-info (str caller     " `:nmax` opt must be compile-time integer") {:value nmax     :type (type nmax)})))
         opts))))

(comment (valid-profiling-opts {} {} `(tufte/profiled {} "body")))

#?(:clj
   (defmacro profiled
     "Use this to start profiling:
       - Always executes body and returns [<body-result> <?pstats>].
       - When profiling is unfiltered [*1], records execution time of all `p` forms.

     [*1] See `set-ns-filter!`, `set-id-filter!`, `set-min-level!`, etc.

     Handy if you'd like to consume pstats directly, otherwise see `profile`.

     `pstats` objects are derefable and mergeable:
       - @pstats                 => {:clock {:keys [t0 t1 total]}, :stats {<id> {:keys [n sum ...]}}}
       - @(merge-pstats ps1 ps2) => {:clock {:keys [t0 t1 total]}, :stats {<id> {:keys [n sum ...]}}}

     Full set of keys in above `:stats` maps:
       :n :min :max :mean :mad :sum :p25 :p50 :p75 :p90 :p95 :p99 :loc :last

       All values are numerical (longs or doubles), except for `:loc` which
       is a map of `p` callsite location information, or set of such maps, e.g.:
         #{{:ns \"my-ns\", :file \"/tmp/my-ns.clj\", :line 122, :column 21}}

     Options include:
       `:dynamic?`   - Use multi-threaded profiling? (default false).
       `:nmax`       - Max captures per `p` id before compaction (default 8e5).
       `:id`         - Profiling id provided to handlers (e.g. `::my-profiling-id`).

       `:level`      - Integer (default 5), must >= active minimum level to profile.
       `:sample`     - Sample rate ∈ℝ[0,1], profile only this proportion of calls.
       `:rate-limit` - {<limit-id> [<n-max-calls> <msecs-window>]} spec, profile
                       only calls that don't exceed the specified limits.
       `:filter`     - Profile only when filter form (e.g. boolean expr) is truthy.

     Laziness in body:
       Lazy seqs and other forms of laziness (e.g. delays) in body will only
       contribute to profiling results if/when EVALUATION ACTUALLY OCCURS.
       This is intentional and a useful property. Compare:

         (profiled {}  (delay (Thread/sleep 2000))) ; Doesn't count sleep
         (profiled {} @(delay (Thread/sleep 2000))) ; Does    count sleep

     Async code in body:
       Execution time of any code in body that runs asynchronously on a
       different thread will generally NOT be automatically captured by default.

       :dynamic? can be used to support capture in cases where Clojure's
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

     {:arglists '([{:keys [id level sample rate-limit filter]} & body])}
     [opts & body]
     (let [loc  (or (when (map? opts) (get opts :loc)) (enc/get-source &form &env))
           opts (valid-profiling-opts loc opts `(~'tufte/profiled ~opts ~@body))

           {:keys [dynamic? nmax]} opts
           nmax (long nmax)

           {:keys [elide? allow?]}
           (sigs/filterable-expansion
             {:loc      loc
              :opts-arg opts ; {:keys [id level ...]}
              :sf-arity 3
              :ct-sig-filter  impl/ct-sig-filter
              :rt-sig-filter `impl/*rt-sig-filter*})]

       (if elide?
         `[(do ~@body)]

         (if dynamic?
           `(enc/if-not ~allow?
              [(do ~@body)]
              (let [pd# (impl/new-pdata-dynamic ~nmax)]
                (binding [impl/*pdata* pd#] [(do ~@body) @pd#])))

           `(enc/if-not ~allow?
              [(do ~@body)]
              (let [pd# (impl/new-pdata-local ~nmax)]
                (binding [impl/*pdata* nil] ; Ensure no dynamic parent (=>nesting) steals local captures
                  (try
                    (impl/pdata-local-push pd#)
                    [(do ~@body) @pd#]
                    (finally (impl/pdata-local-pop)))))))))))

(comment
  (enc/qb 1e6   (profiled {:filter false}) (profiled {})) ; [78.09 695.94]
  (macroexpand '(profiled {:filter false})))

#?(:clj
   (defmacro profile
     "Use this to start profiling:
       - Always executes body and returns <body-result>.
       - When profiling is unfiltered [*1], records execution time of all `p` forms
         and dispatches map [*2] to any registered handlers (see `add-handler!`).

     [*1] See `set-ns-filter!`, `set-id-filter!`, `set-min-level!`, etc.
     [*2] {:keys [instant id level ns line data pstats pstats-str_]}

     Decouples creation and consumption of pstats, handy if you'd like to
     consume/aggregate pstats later/elsewhere. Otherwise see `profiled`.

     `pstats` objects are derefable and mergeable:
       - @pstats                 => {:clock {:keys [t0 t1 total]}, :stats {<id> {:keys [n sum ...]}}}
       - @(merge-pstats ps1 ps2) => {:clock {:keys [t0 t1 total]}, :stats {<id> {:keys [n sum ...]}}}

     Full set of keys in above `:stats` maps:
       :n :min :max :mean :mad :sum :p25 :p50 :p75 :p90 :p95 :p99 :loc :last

       All values are numerical (longs or doubles), except for `:loc` which
       is a map of `p` callsite location information, or set of such maps, e.g.:
         #{{:ns \"my-ns\", :file \"/tmp/my-ns.clj\", :line 122, :column 21}}

     Options include:
       `:dynamic?`   - Use multi-threaded profiling? (default false).
       `:nmax`       - Max captures per `p` id before compaction (default 8e5).
       `:id`         - Profiling id provided to handlers (e.g. `::my-profiling-id`).

       `:level`      - Integer (default 5), must >= active minimum level to profile.
       `:sample`     - Sample rate ∈ℝ[0,1], profile only this proportion of calls.
       `:rate-limit` - {<limit-id> [<n-max-calls> <msecs-window>]} spec, profile
                       only calls that don't exceed the specified limits.
       `:filter`     - Profile only when filter form (e.g. boolean expr) is truthy.

     Laziness in body:
       Lazy seqs and other forms of laziness (e.g. delays) in body will only
       contribute to profiling results if/when EVALUATION ACTUALLY OCCURS.
       This is intentional and a useful property. Compare:

         (profiled {}  (delay (Thread/sleep 2000))) ; Doesn't count sleep
         (profiled {} @(delay (Thread/sleep 2000))) ; Does    count sleep

     Async code in body:
       Execution time of any code in body that runs asynchronously on a
       different thread will generally NOT be automatically captured by default.

       :dynamic? can be used to support capture in cases where Clojure's
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

     {:arglists '([{:keys [id level sample rate-limit filter]} & body])}
     [opts & body]
     (let [loc  (or (when (map? opts) (get opts :loc)) (enc/get-source &form &env))
           opts (valid-profiling-opts loc opts `(~'tufte/profiled ~opts ~@body))

           {:keys [ns line column file]} loc
           {:keys [id data level]} opts]

       `(let [[body-result# pstats#] (profiled ~opts ~@body)]
          (when pstats#
            (when-let [handlers# impl/*sig-handlers*]
              (let [inst# (enc/now-inst)]
                (sigs/call-handlers! handlers#
                  (ProfilingSignal. inst# ~id ~level ~loc ~ns ~line ~data
                    (do                   pstats#)
                    (delay (format-pstats pstats#)))))))
          body-result#))))

(comment
  (profiled {} "body")
  (profiled {:filter (chance 0.5)} "body")
  (profile  {:id ::my-id} "body")

  (do      (profiled {} (p {:level 5 :id :p1})))
  @(second (profiled {} (p (let [x :foo/id] x) "body")))
  @(second (profiled {} (p :foo (p :bar))))

  (enc/qb 1e5  (profiled {} 2 (p :p1))) ; 70.7
  (enc/time-ms (profiled {} 2 (dotimes [_ 3e6] (p :p1)))) ; 997
  )

;;;; Utils

(enc/defaliases
  {:src impl/merge-pstats}
  {:src impl/format-pstats}
  {:src impl/format-grouped-pstats})

(comment
  (let [[_ ps1] (profiled {} (p :p1))
        [_ ps2] (profiled {} (p :p1))]
    (enc/qb 1e5 (merge-pstats ps1 ps2))) ; 211.67

  (println
    (str "\n"
      (format-pstats
        (second
          (profiled {}
            (p :foo (Thread/sleep 200))
            (p :bar (Thread/sleep 500))
            (do     (Thread/sleep 800))))
        {:columns [:clock :p50 :p95]}))))

(defn chance "Returns true with probability p∈ℝ[0,1]."
  [p] (< (Math/random) (double p)))

(defn format-id-abbr
  "Returns a cached (fn [id]) => abbreviated id with at most `n-full`
  unabbreviated namespace parts.

  Example:
    ((format-id-abbr 0)  :foo.bar/baz)   => :f.b/baz
    ((format-id-abbr 1)  'foo.bar/baz)   => 'f.bar/baz
    ((format-id-abbr 2) \"foo.bar/baz\") => \"foo.bar/baz\""

  ([      ] (format-id-abbr 1))
  ([n-full] (enc/fmemoize (partial enc/abbreviate-ns n-full))))

#?(:clj
   (defmacro refer-tufte
          "(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])"
     [] `(require '~'[taoensso.tufte :as tufte :refer [defnp p profiled profile]])))

(comment (refer-tufte))

;;;; fnp stuff

(defn- fn-sigs [def? ?meta-id ?fn-sym sigs loc]
  (let [single-arity?   (vector? (first sigs))
        sigs   (if single-arity? (list  sigs) sigs)
        fn-sym (or ?fn-sym (gensym))

        base-id
        (if ?meta-id
          (enc/as-qname ?meta-id)
          (str *ns* "/" (if def? "defn_" "fn_") (name fn-sym)))

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
                  `(~params ~?prepost-map (p {:id ~base-id, :loc ~loc} (p {:id ~arity-id, :loc ~loc} ~@body)))
                  `(~params ~?prepost-map (p {:id ~base-id, :loc ~loc}                               ~@body)))

                (if-let [arity-id ?arity-id]
                  `(~params               (p {:id ~base-id, :loc ~loc} (p {:id ~arity-id, :loc ~loc} ~@body)))
                  `(~params               (p {:id ~base-id, :loc ~loc}                               ~@body))))))

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
                            (enc/get-source &form &env))]
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
                           (enc/get-source &form &env))]
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
                           (enc/get-source &form &env))]
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
  ;; `acc` - (latom {<profile-id> <pstats>})
  #?(:clj clojure.lang.IDeref :cljs IDeref) (#?(:clj deref  :cljs -deref)  [_] (enc/reset-in! acc {})) ; Drain
  #?(:clj clojure.lang.IFn    :cljs IFn)    (#?(:clj invoke :cljs -invoke) [_] (enc/reset-in! acc {})) ; Drain
  (                                          #?(:clj invoke :cljs -invoke) [_ profile-id ps]
   (when (and profile-id ps)
     ;; Contention would be expensive so consumer should serialize calls
     (acc profile-id #(impl/merge-pstats % ps))
     true)))

(defn stats-accumulator
  "Experimental, subject to change!
  Small util to help merge pstats from multiple runs and/or threads.

  Returns a stateful `StatsAccumulator` (`sacc`) with:
    - (sacc <profile-id> <pstats>) ; Merges given pstats under given profile id
    - @sacc                        ; Drains accumulator and returns drained
                                   ; {<profile-id> <merged-pstats>}

  Note that for performance reasons, you'll likely want some kind of
  async/buffer/serialization mechanism in front of merge calls.

  One common pattern using `accumulating-handler` is to create a
  system-wide accumulator that you deref every n minutes/etc. to get
  a view of system-wide performance over the period, e.g.:

    (defonce my-sacc (stats-accumulator) ; Create an accumulator
    (add-handler! :my-sacc (accumulating-handler my-sacc)) ; Register handler

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
    (sacc :profiled1 (second (profiled {} (p :p1))))
    (Thread/sleep 100)
    (sacc :profiled2 (second (profiled {} (p :p2))))
    [@sacc @sacc]))

;;;; Handlers

(defn print-handler
  "Returns a simple handler fn for use with `add-handler!` that:
    1. Formats `profile` pstats with `format-pstats`, and
    2. Prints the resulting string table with `print`.

  Options:
    `:format-pstats-opts` - Opts map provided to `format-pstats`"
  {:added "vX.Y.Z (YYYY-MM-DD)"}
  ([] (print-handler nil))
  ([{:keys [format-pstats-opts]}]
   (fn print-handler [^ProfilingSignal ps]
     (enc/print1
       (str
         (when-let [id   (.-id     ps)] (str   "id: "   id enc/newline))
         (when-let [data (.-data   ps)] (str "data: " data enc/newline))
         (format-pstats  (.-pstats ps) format-pstats-opts) enc/newline)))))

(defn telemere-handler
  "Returns nil if Telemere isn't present, otherwise-
  Returns a simple handler fn for use with `add-handler!` that:
    1. Formats `profile` pstats with `format-pstats`, and
    2. Generates an appropriate signal with Telemere.

  Options:
    `:format-pstats-opts` - Opts map provided to `format-pstats`
    `:signal-level`       - Signal level, or ifn to map profiling->signal level"

  ;; Shares impl details with `taoensso.tufte.timbre/timbre-handler`

  {:added "vX.Y.Z (YYYY-MM-DD)"}
  ([] (telemere-handler nil))
  ([{:keys [signal-level format-pstats-opts]}]
   (when enc/have-telemere?
     (fn telemere-handler [^ProfilingSignal ps]
       (enc/signal!
         {:kind  :profiling
          :loc   (.-loc ps)
          :id    (.-id  ps)
          :data         ps
          :level (impl/signal-level ps signal-level)
          :msg   (impl/signal-msg   ps format-pstats-opts)})))))

(defn accumulating-handler
  "Takes a `StatsAccumulator` and returns a simple handler fn for use with
  `add-handler!` that merges `profile` pstats into the given accumulator.

  See `stats-accumulator` for more info."
  {:added "vX.Y.Z (YYYY-MM-DD)"}
  [^StatsAccumulator sacc]
  (enc/have #(instance? StatsAccumulator %) sacc)
  (fn accumulating-handler [^ProfilingSignal ps]
    (sacc (.-id ps) (.-pstats ps))))

(comment
  (def my-sacc (stats-accumulator))
  (add-handler! :my-sacc (accumulating-handler my-sacc))

  (do
    (future (profile {}         (p :p1 (Thread/sleep 900))))
    (future (profile {:id :foo} (p :p1 (Thread/sleep 900))))
    (future (profile {:id :bar} (p :p1 (Thread/sleep 500)))))

  (println
    (format-grouped-pstats @my-sacc
      {:format-pstats-opts {:columns [:n]}})))

;;;; Deprecated

(enc/deprecated
  #?(:clj (defmacro ^:no-doc ^:deprecated with-ns-pattern  "Prefer `with-ns-filter`." [ns-pattern & body] `(with-ns-filter ~ns-pattern (do ~@body))))
  (defn             ^:no-doc ^:deprecated  set-ns-pattern! "Prefer `set-ns-filter!`." [ns-pattern]         (set-ns-filter!  ns-pattern))

  #?(:clj
     (defmacro ^:no-doc pspy "Prefer `p`."
       {:deprecated "vX.Y.Z (YYYY-MM-DD)"}
       [& args] (enc/keep-callsite `(p ~@args))))

  (defn ^:no-doc add-legacy-handler!
    "Prefer `add-handler!`.
    Note that some handler arg key names have changed:
      :ns-str -> :ns
      :?id    -> :id
      :?data  -> :data"
    {:deprecated "vX.Y.Z (YYYY-MM-DD)"}
    ([handler-id            handler-fn] (add-handler! handler-id nil handler-fn))
    ([handler-id ns-pattern handler-fn]
     (let [dispatch-opts
           (when (and ns-pattern (not= ns-pattern "*"))
             {:ns-filter ns-pattern})]

       (add-handler! handler-id
         (fn [^ProfilingSignal ps]
           (assoc ps
             :ns-str (.-ns   ps)
             :?id    (.-id   ps)
             :?data  (.-data ps)))))))

  (defn ^:no-doc add-basic-println-handler!
    "Prefer (add-handler! <handler-id> (print-handler {<handler-opts>}) <dispatch-opts>)."
    {:deprecated "vX.Y.Z (YYYY-MM-DD)"}
    [{:keys [ns-pattern handler-id format-pstats-opts]
      :or   {ns-pattern "*"
             handler-id :basic-println}}]

    (let [handler-fn (print-handler {:format-pstats-opts format-pstats-opts})]
      (add-legacy-handler! handler-id ns-pattern handler-fn)))

  (defn ^:no-doc add-accumulating-handler!
    "Prefer
      (def my-sacc (stats-accumulator))
      (add-handler! <handler-id> (accumulating-handler my-sacc) <dispatch-opts>)."
    {:deprecated "vX.Y.Z (YYYY-MM-DD)"}
    [{:keys [ns-pattern handler-id runner-opts]
      :or   {ns-pattern "*"
             handler-id :accumulating}}]

    (let [sacc       (stats-accumulator)
          handler-fn (accumulating-handler sacc)]
      (add-legacy-handler! handler-id ns-pattern handler-fn)
      sacc)))

;;;;

(comment
  (add-handler! :print-handler (print-handler))
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

  (defnp arithmetic
    []
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

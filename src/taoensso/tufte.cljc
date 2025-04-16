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
  (:refer-clojure :exclude [newline])
  (:require
   [taoensso.truss          :as truss]
   [taoensso.encore         :as enc]
   [taoensso.encore.stats   :as stats]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte.impl     :as impl
    #?@(:cljs [:refer [PStats HandlerVal Signal WrappedSignal]])])

  #?(:clj  (:import [taoensso.tufte.impl PStats HandlerVal Signal WrappedSignal]))
  #?(:cljs (:require-macros [taoensso.tufte :refer [profiled]])))

(comment
  (remove-ns (symbol (str *ns*)))
  (:api (enc/interns-overview)))

(enc/assert-min-encore-version [3 143 1])

;;;; Shared signal API

(declare ; Needed to avoid `clj-kondo` "Unresolved var" warnings
  level-aliases
  help:filters help:handlers help:handler-dispatch-options
  get-filters get-min-levels get-handlers get-handlers-stats

  #?(:clj without-filters)
  set-kind-filter! #?(:clj with-kind-filter)
  set-ns-filter!   #?(:clj with-ns-filter)
  set-id-filter!   #?(:clj with-id-filter)
  set-min-level!   #?(:clj with-min-level)

  #?(:clj with-handler) #?(:clj with-handler+)
  add-handler! remove-handler! stop-handlers!

  ^:dynamic *ctx* set-ctx! #?(:clj with-ctx) #?(:clj with-ctx+)
  ^:dynamic *xfn* set-xfn! #?(:clj with-xfn) #?(:clj with-xfn+))

(def default-handler-dispatch-opts
  "See `help:handler-dispatch-opts` for details."
  (dissoc sigs/default-handler-dispatch-opts
    :convey-bindings? ; We use `enc/bound-delay`
    ))

(sigs/def-api
  {:sf-arity 3
   :ct-call-filter    impl/ct-call-filter
   :*rt-call-filter*  impl/*rt-call-filter*
   :*sig-handlers*    impl/*sig-handlers*
   :lib-dispatch-opts default-handler-dispatch-opts})

;;;; Aliases

(enc/defaliases
  ;; Encore
  #?(:clj enc/set-var-root!)
  #?(:clj enc/update-var-root!)
  #?(:clj enc/get-env)
  #?(:clj enc/call-on-shutdown!)
  enc/chance
  enc/rate-limiter
  enc/newline
  sigs/comp-xfn
  #?(:clj truss/keep-callsite)

  ;; Impl
  impl/merge-pstats
  impl/format-pstats
  impl/format-grouped-pstats)

;;;; Help

(do
  (impl/defhelp help:profiling-options    :profiling-options)
  (impl/defhelp help:pstats-content       :pstats-content)
  (impl/defhelp help:signal-content       :signal-content)
  (impl/defhelp help:environmental-config :environmental-config))

;;;; Vestigial

(def ^:dynamic *min-level* "Vestigial, currently ignored." 2)
(def ^:dynamic *ns-filter* "Vestigial, currently ignored." "*")

;;;; Low-level primitives

(defn profiling? "Returns e/o #{nil :thread :dynamic}."
  [] (if impl/*pdata* :dynamic (when (impl/pdata-local-get) :thread)))

(comment (enc/qb 1e6 (profiling?))) ; 43.91

(def ^:const ^:no-doc default-nmax (long 8e5))
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
     [pdata {:as opts, :keys [dynamic?]} & body]
     (impl/valid-opts! &form &env 'tufte/with-profiling opts body)
     (if dynamic?
       `(binding [impl/*pdata* ~pdata] (do ~@body))
       `(binding [impl/*pdata*    nil] ; Ensure no dynamic parent (=>nesting) steals local captures
          (try
            (impl/pdata-local-push ~pdata)
            (do ~@body)
            (finally (impl/pdata-local-pop)))))))

#?(:clj
   (defmacro ^:no-doc profiled*
     "Unconditionally returns [<body-result> <pstats>].
     Implementation detail."
     [caller dynamic? nmax run-form]
     (if     dynamic?
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
     "Profiling spy, wraps forms that should be timed during profiling."
     {:doc (impl/docstring :p)
      :arglists '([id & body] [{:keys [id level]} & body])}
     [s1 & body]
     (let [opts       (if (map? s1) s1 {:id s1})
           level-form (get opts :level 5)
           id-form    (get opts :id)
           location
           (enc/assoc-some nil
             (or (get opts :loc) (dissoc (enc/get-source &form &env) :file)))]

       ;; If level is present, it must be a valid compile-time level
       ;; since this macro doesn't offer runtime level checking
       (when level-form (sigs/valid-level level-form))

       (when (or (nil? id-form) (empty? body))
         (truss/ex-info!
           (str "`tufte/p` form needs an id at " (impl/location-str location) ": "
             `(~'p s1 ~@body))))

       (if-let [elide? (when-let [sf impl/ct-call-filter] (not (sf (:ns location) (enc/const-form id-form) (enc/const-form level-form))))]
         `(do ~@body)
         ;; Note no `impl/*rt-call-filter*` check
         `(if-let [pd#     (or impl/*pdata* (impl/pdata-local-get))]
            (let  [t0#     (enc/now-nano*)
                   result# (do ~@body)
                   t1#     (enc/now-nano*)]
              ;; Note that capture cost is excluded from p time
              (pd# ~id-form (- t1# t0#) ~location)
              result#)
            (do ~@body))))))

(comment
  (macroexpand '(p :foo "hello"))
  (let [pd (new-pdata)]
    (with-profiling pd {}
      (p :foo (Thread/sleep 100))
      (p :bar (Thread/sleep 200)))
    @(pd)))

#?(:clj (defn- auto-> [form auto-form] (if (= form :auto) auto-form form)))
#?(:clj
   (defmacro profiled
     "Conditionally profiles body, returns [<body-result> <?pstats>]."
     {:doc (impl/docstring :profiled)
      :arglists
      '([{:keys [elidable? #_elide? #_allow? #_callsite-id,
                 sample ns id level when limit limit-by]}
         & body])}

     [opts & body]
     (impl/valid-opts! &form &env 'tufte/profiled opts body)
     (let [opts     (merge {:level 5} opts)
           ns-form* (get opts :ns :auto)
           ns-form  (auto-> ns-form* (str *ns*))

           {:keys [elide? allow?]}
           (sigs/filter-call
             {:cljs? (boolean (:ns &env))
              :sf-arity 3
              :ct-call-filter     impl/ct-call-filter
              :*rt-call-filter* `impl/*rt-call-filter*}
             (assoc opts :ns ns-form))

           {:keys [dynamic? nmax]} opts]

       (if elide?
         `[(do ~@body)]
         `((fn [] ; iife for better IoC compatibility
             (let [body-fn# (fn [] ~@body)]
               (enc/if-not ~allow?
                 [(body-fn#)]
                 (profiled* 'tufte/profiled ~dynamic? ~nmax (body-fn#))))))))))

(comment
  (enc/qb 1e6   (profiled {:allow? false}) (profiled {})) ; [36.74 259.73]
  (macroexpand '(profiled {:allow? false}))

  (profiled {} (p :p1 nil))
  (profiled {} (p {:level 5 :id :p1} nil))
  (profiled {} (p (let [x :foo/id] x) "body"))
  (profiled {:level 2 :when (chance 0.5)} (p :p1 "body"))
  (profiled {} (p :foo (p :bar nil))))

#?(:clj
   (defmacro profile
     "Conditionally profiles body, returns <body-result> and ?dispatches
     signal to registered handlers."
     {:doc (impl/docstring :profile)
      :arglists
      '([{:keys [elidable? #_elide? #_allow? #_callsite-id,
                 sample ns id level when limit limit-by]}
         & body])}

     [opts & body]
     (impl/valid-opts! &form &env 'tufte/profile opts body)
     (let [opts       (merge {:level 5} opts)
           ns-form*   (get opts :ns :auto)
           ns-form    (auto-> ns-form* (str *ns*))
           level-form (get opts :level)
           id-form    (get opts :id)
           data-form  (get opts :data)]

       `(let [[result# pstats#] (profiled ~opts ~@body)]
          (when        pstats#
            (impl/handle!
              (HandlerVal. ~ns-form ~level-form ~id-form ~data-form
                pstats# (delay (format-pstats pstats#))
                ~*file* ~(:line (meta &form)))))
          result#))))

(comment (profile {:id ::my-id} (p :p1 "body")))

;;;; Handlers
;; Handlers used by `profile`, decouple `pstats` creation and consumption

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

;;;; Public utils

(defn format-id-abbr-fn
  "Returns a cached (fn [id]) => abbreviated id with at most `n-full`
  unabbreviated namespace parts.

  Example:
    ((format-id-abbr 0)  :foo.bar/baz)   => :f.b/baz
    ((format-id-abbr 1)  'foo.bar/baz)   => 'f.bar/baz
    ((format-id-abbr 2) \"foo.bar/baz\") => \"foo.bar/baz\""

  ([      ] (format-id-abbr-fn 1))
  ([n-full] (enc/fmemoize (partial enc/abbreviate-ns n-full))))

#?(:clj
   (defmacro refer-tufte
          "(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])"
     [] `(require '~'[taoensso.tufte :as tufte :refer [defnp p profiled profile]])))

(comment (refer-tufte))

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
  #?(:clj (defmacro ^:no-doc ^:deprecated pspy           "Prefer `p`." [& args] (truss/keep-callsite `(p ~@args))))
  (enc/def*         ^:no-doc ^:deprecated format-id-abbr "Prefer `format-id-abbr-fn`." format-id-abbr-fn)

  #?(:clj (defmacro ^:no-doc ^:deprecated with-min-level  "Prefer `binding`." [level & body] `(binding [*min-level* ~level] ~@body)))
  (defn             ^:no-doc ^:deprecated  set-min-level! "Prefer `alter-var-root`." [level]
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

  (let [[_ ps1] (profiled {} (p :p1 nil))
        [_ ps2] (profiled {} (p :p1 nil))]
    (enc/qb 1e5 (merge-pstats ps1 ps2))) ; 83.5

  (println
    (str "\n"
      (format-pstats
        (second
          (profiled {}
            (p :foo (Thread/sleep 200))
            (p :bar (Thread/sleep 500))
            (do     (Thread/sleep 800))))
        {:columns [:clock :p50 :p95]})))

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

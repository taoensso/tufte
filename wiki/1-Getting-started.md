# Setup

Add the [relevant dependency](../#latest-releases) to your project:

```clojure
Leiningen: [com.taoensso/tufte               "x-y-z"] ; or
deps.edn:   com.taoensso/tufte {:mvn/version "x-y-z"}
```

And setup your namespace imports:

```clojure
(ns my-app
  (:require [taoensso.tufte :as tufte]]))
```

# Usage

## Step 1: identify forms to profile

Wrap the forms you'd like to [sometimes] profile with [`p`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-p) and give them an id (namespaced keyword):

```clojure
(defn get-customer-info []
  (let [raw-customer-map (p ::get-raw-customer (fetch-from-db))]
    (tufte/p ::enrich-raw-customer
      (do-some-work raw-customer-map))))
```

Tufte will record the execution times of these `p` forms whenever profiling is active. 

**NB**: whether or not profiling is active, a `p` form **always returns its normal body result**. You never need to worry about Tufte messing with your return values.

## Step 2: activate profiling

Activate profiling of `p` forms with [`profiled`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-profiled) or [`profile`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-profile):

API | Return value | Effect
--- | --- | ---
`profiled` | `[<body-result> <?pstats>]` | None                                        |
`profile` | `<body-result>` | Sends `<?pstats>` to registered handlers[1]

> **[1]** Register handlers with `[tufte/add-handler!]`(https://taoensso.github.io/tufte/taoensso.tufte.html#var-add-handler.21)

Since `?pstats` is structured Clojure data (a map), it's trivial to work with. Save to a db, log, `put!` to a `core.async` channel, filter, aggregate, use for a realtime analytics dashboard, examine for outliers or unexpected behaviour, feed into your other performance/analytics systems, etc.

- Use `profiled` to handle `?pstats` yourself **directly at the call site**
- Use `profile` to queue `?pstats` for handling **later/elsewhere**

Between the two, you have great flexibility for a wide range of use cases in production and during development/debugging.

# Conditional profiling

Tufte offers **3 methods** to control **if/when** profiling happens.

These methods may apply at **compile time**, **runtime**, or both. Note that profiling excluded at compile time will be completely elided for **zero runtime cost**.

## Method 1/3: namespace filtering

`p`, `profiled`, and `profile` forms can be elided or filtered based on the **namespace in which they occur**.

Namespace filter | Control with
--- | ---
Runtime | [`tufte/*ns-filter*`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-*ns-filter*) (reset with `alter-var-root!`, or rebind with `binding`)
Compile-time | `taoensso.tufte.ns-pattern` JVM property or `TAOENSSO_TUFTE_NS_PATTERN` environment variable, both read **as edn**

`tufte/*ns-filter*` can be an arbitrary `(fn may-profile? [ns])`, or a namespace pattern like:

```clojure
"foo.bar.baz"
"foo.bar.*"
#{"foo.bar.*" "some.lib.*"}
{:allow #{"foo.bar.*"} :deny #{"noisy.lib.*"}}
```

See [`tufte/*ns-filter*`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-*ns-filter*) for more info.

### Method 2/3: profiling levels

`p`, `profiled`, and `profile` forms can take an optional **profiling level** ∈ `#{0 1 2 3 4 5}`, e.g.:

```clojure
(tufte/profiled {:level 3} ...) ; Only activates profiling when (>= 3 *min-level*)
```

These form levels will be checked against [`tufte/*min-level*`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-*min-level*) which can be an constant integer, or a `[[<ns-pattern> <min-level-int>] ... ["*" <default-min-level-int>]]` for namespace-specific levels.

See [`tufte/*min-level*`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-*min-level*) for more info.

Min level    | Control with
--- | ---
Runtime | [`tufte/*min-level*`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-*min-level*) (reset with `alter-var-root!`, or rebind with `binding`)
Compile-time | `taoensso.tufte.min-level` JVM property or `TAOENSSO_TUFTE_MIN_LEVEL` environment variable, both read **as edn**

### Method 3/3: arbitrary runtime conditions

Finally, `profiled` and `profile` both support an optional arbitrary test expression:

```clojure
(tufte/profiled {:when my-cond?} ...) ; Only activates profiling when `my-cond?` is truthy
```

This can be used for a wide range of sophisticated behaviour including smart, **application-aware rate limiting**.

As one simpler example, we can get **sampled profiling** like this:

```clojure
(tufte/profiled {:when (tufte/chance 0.5)} ...) ; Only activates profiling with 50% probability
```

# Formatting `pstats`

[`tufte/format-pstats`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-format-pstats) is provided as one easy way to generate a string table from Tufte's `pstats` data.

It takes an optional map as a second parameter to control formatting:

```clojure
{;; Vector of ordered columns to include in output (all columns by default):
 :columns [:n :min :p50 :p90 :p95 :p99 :max :mean :mad :clock :sum]

 ;; Function called on each form id (pid), allowing format customization:
 :format-id-fn #_str (tufte/format-id-abbr) ; For abbreviated ids, see docstring for details

 ;; Allows for custom sorting of results:
 :sort-fn (fn [m] (get m :sum))}
```

If you're using [`tufte/add-basic-println-handler!`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-add-basic-println-handler.21), you can control formatting
through the `:format-pstats-opts` option:

```clojure
;; See the individual docstrings for options, etc.:
(tufte/add-handler! :my-console-handler
  (tufte/handler:console
    {:output-fn
     (tufte/format-signal-fn
       {:format-pstats-opts {:columns [:n :p50 :mean :clock :sum]}})}))

(defnp get-x [] (Thread/sleep 500)             "x val")
(defnp get-y [] (Thread/sleep (rand-int 1000)) "y val")

(tufte/profile {:level :info, :id ::my-profiling-id}
  (dotimes [_ 5]
    (get-x)
    (get-y)))

;; The following will be printed to *out*:
;;
;; pId         nCalls      50% ≤       Mean      Clock  Total
;; defn_get-y       5      572ms      567ms      2.84s    53%
;; defn_get-x       5      500ms      500ms      2.50s    47%
;;
;; Accounted                                     5.34s   100%
;; Clock                                         5.34s   100%
```

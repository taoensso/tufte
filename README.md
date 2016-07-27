<a href="https://www.taoensso.com" title="More stuff by @ptaoussanis at www.taoensso.com">
<img src="https://www.taoensso.com/taoensso-open-source.png" alt="Taoensso open-source" width="400"/></a>

**[CHANGELOG]** | [API] | current [Break Version]:

```clojure
[com.taoensso/tufte "1.0.1"]
```

> Please consider helping to [support my continued open-source Clojure/Script work]? 
> 
> Even small contributions can add up + make a big difference to help sustain my time writing, maintaining, and supporting Tufte and other Clojure/Script libraries. **Thank you!**
>
> \- Peter Taoussanis

# Tufte

### Simple profiling and performance monitoring for Clojure/Script

![Hero]

> Charles Joseph Minard's _Carte Figurative_, one of [Edward Tufte][]'s favourite examples of good data visualization.

## 10-second example

```clojure
(require '[taoensso.tufte :as tufte :refer (defnp p profiled profile)])

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

;;; Let's define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

;; How do these fns perform? Let's check:

(profile ; Profile any `p` forms called during body execution
  {} ; Profiling options; we'll use the defaults for now
  (dotimes [_ 5]
    (p :get-x (get-x))
    (p :get-y (get-y))))

;; The following will be printed to *out*:
;;
;;            pId      nCalls       Min        Max       MAD      Mean   Time% Time
;;         :get-y           5  171.68ms   940.84ms  264.82ms  541.28ms      52 2.71s
;;         :get-x           5   502.3ms   505.07ms    1.17ms   503.4ms      48 2.52s
;;     Clock Time                                                          100 5.22s
;; Accounted Time                                                          100 5.22s
```

## Features

 * Small, **fast**, cross-platform Clojure/Script codebase (<1k loc)
 * **Tiny**, flexible API: `p`, `profiled`, `profile`
 * Great **compile-time elision** and **runtime filtering** support
 * Arbitrary Clojure/Script **form-level** profiling
 * Full support for **thread-local** and **multi-threaded** profiling
 * **Stats are just Clojure maps**: aggregate, **analyse**, log, serialize to db, ...

## Quickstart

Add the necessary dependency to your project:

```clojure
[com.taoensso/tufte "1.0.1"]
```

And setup your namespace imports:

```clojure
(ns my-clj-ns ; Clojure namespace
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(ns my-cljs-ns ; ClojureScript namespace
  (:require [taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]))
```

### Step 1: identify forms you'd like to [sometimes] profile

Just wrap them with `p` and give them an id (namespaced keyword):

```clojure
(defn get-customer-info []
  (let [raw-customer-map (p ::get-raw-customer (fetch-from-db))]
    (p ::enrich-raw-customer
      (do-some-work raw-customer-map))))
```

Tufte will record the execution times of these `p` forms whenever profiling is active. 

**NB**: whether or not profiling is active, a `p` form **always returns its normal body result**.

> This last point is important: you never need to worry about Tufte messing with your return values.

### Step 2: activate profiling of `p` forms

Activate profiling with `profiled` or `profile`:

API        | Return value               | Effect                                     |
---------- | -------------------------- | ------------------------------------------ |
`profiled` | `[<body-result> <?stats>]` | None                                       |
`profile`  | `<body-result>`            | Sends `<?stats>` to registered handlers[1] |

**[1]** Register handlers using `(tufte/add-handler! <handler-id> <ns-pattern> <handler-fn>)`

> Handler ideas: save to a db, log, `put!` to a `core.async` channel, filter, aggregate, use for a realtime analytics dashboard, examine for outliers or unexpected behaviour, feed into your other performance/analytics systems, ...

 * Use `profiled` to handle stats yourself **directly at the callsite**.
 * Use `profile` to queue stats for handling **later/elsewhere**.

Between the two, you have great flexibility for a wide range of use cases in production and during development/debugging.

## Conditional profiling

Tufte offers extensive facilities to control if and when profiling happens.

Both **compile-time elision** and **runtime filtering** are supported.

### Method 1/3: profiling levels

Every `p`, `profiled`, and `profile` form can take an optional **profiling level** ∈ `#{0 1 2 3 4 5}`.

This level must be >= `tufte/*min-level*` for profiling to occur.

For example:

```clojure
(profiled {:level 3} ...) ; Only activates profiling when (>= 3 *min-level*)
```

Min level    | Set with                                       |
------------ | ---------------------------------------------- |
Runtime      | `tufte/set-min-level!`, `tufte/with-min-level` |
Compile-time | `TUFTE_MIN_LEVEL` environment variable         |

> Note that runtime filtering stacks with any compile-time elision

### Method 2/3: namespace filtering

Likewise- `p`, `profiled`, and `profile` forms can be elided or filtered by the namespace in which they occur.

Namespace filter | Set with                                         |
---------------- | ------------------------------------------------ |
Runtime          | `tufte/set-ns-pattern!`, `tufte-with-ns-pattern` |
Compile-time     | `TUFTE_NS_PATTERN` environment variable          |

> Note that runtime filtering stacks with any compile-time elision

Some example namespace patterns:

```clojure
"foo.bar.baz"
"foo.bar.*"
#{"foo.bar.*" "some.lib.*"}
{:whitelist #{"foo.bar.*"} :blacklist #{"noisy.lib.*"}}
```

### Method 3/3: arbitrary runtime conditions

Finally, `profiled` and `profile` both support an optional arbitrary test expression:

```clojure
(profiled {:when my-cond?} ...) ; Only activates profiling when `my-cond?` is truthy
```

This can be used for a wide range of sophisticated behaviour including smart, **application-aware rate limiting**.

As one simpler example, we can get **sampled profiling** like this:

```clojure
(profiled {:when (tufte/chance 0.5)} ...) ; Only activates profiling with 50% probability
```

## FAQ

### Why not just use [YourKit], [JProfiler], or [VisualVM]?

The traditional recommendation for Clojure profiling has usually been to use a standard JVM profiling tool like one of the above.

And they can certainly do the job, but they also tend to be a little hairy: requiring special effort to use, and often producing gobs of information that can be difficult or time-consuming to meaningfully interpret.

In contrast, Tufte offers some interesting benefits:

 * Arbitrary **application-aware, form-level** profiling; measure [just] what you care about
 * Simple **thread-local or multi-threaded semantics**
 * During dev/debug: check performance **right from within your REPL**
 * During production: **ongoing, application-aware** conditional profiling, logging, and analysis (stats are just **Clojure maps**)
 * A **cross-platform API** that works seamlessly between your server (Clj) and client (Cljs) applications

Note that JVM profiling tools can still be very handy. Tufte doesn't offer memory profiling for example, and it's not well suited to forensic or very low-level profiling.

If you want to know `clojure.lang.Numbers$LongOps.lt(Number, Number)` or memory stats, you'll want a JVM tool. If you want to know `my-fn` stats, or you want ongoing stats in production - Tufte could be a good fit.

### How does Tufte compare to the profiling in [@ptaoussanis/Timbre]?

Actually, I developed Tufte one weekend while refactoring Timbre's profiling. It's basically a refinement of the ideas from there.

Decided that I could make some worthwhile improvements with some breaking API changes and a new set of dedicated docs. Tufte's implementation is cross-platform, considerably faster, and its API more flexible.

With the release of Tufte, I'll be **deprecating Timbre's profiling features**.

Note that Tufte's a feature **superset** of Timbre's profiling, so porting should be straightforward:

API        | Timbre              | Tufte                              |
---------- | ------------------- | ---------------------------------- |
`p`        | `[id & body]`       | `[id & body]`, `[opts & body]`     |
`profile`  | `[level id & body]` | `[opts & body]`                    |
`profiled` | `[level id & body]` | `[opts & body]`                    |
`profile`  | Output -> log       | Output -> arbitrary handler-fn [1] |

**[1]** See `tufte.timbre/add-timbre-logging-handler!` for directing Tufte's `profile` output to Timbre.

### How does Tufte compare to [@hugoduncan/Criterium]?

Basically, they serve different use cases: **benchmarking** for Criterium, and **profiling** for Tufte.

| Library   | Measures                 | Use for                       | During    | Emphasis           |
| --------- | ------------------------ | ----------------------------- | --------- | ------------------ |
| Criterium | 1 Clojure form           | Benchmarking                  | Dev       | Accuracy           |
| Tufte     | >=1 Clojure/Script forms | Profiling, basic benchmarking | Dev, prod | Flexibility, speed |

So Criterium produces very accurate stats for a _single_ Clojure expression.

Tufte produces _combined stats_ for an _arbitrary_ number of Clojure/Script expressions with less emphasis on decimal-digit-level accuracy.

For example:

 * Use **Criterium** to measure and compare the performance of two libraries.
 * Use **Tufte** to measure or monitor the performance of various parts of your system and how they relate.

### How's the performance in production?

For thread-local profiling: _very good_. For dynamic profiling: _good_.

Tufte's designed specifically to support ongoing use in production, and is **highly optimized**: so it's about as fast as it gets in both cases.

I'd think of it this way: if something's _at all_ worth profiling, the overhead that Tufte introduces will be _vanishingly_ insignificant. 

Also, keep in mind that Tufte's **conditional profiling** gives you complete control over if and when you do pay (however little) for profiling.

### What's the difference between thread-local and dynamic (multi-threaded) profiling?

If you don't already know the difference, you probably want **thread-local profiling** (the default). It's faster and conceptually simpler: it literally just profiles what happens sequentially on the current thread.

Work being done concurrently in futures and agents will be ignored.

In contrast, **dynamic profiling** works across thread boundaries using Clojure's standard `^:dynamic` binding conveyance as in:

```clojure
(def ^:dynamic *my-dynamic-var* nil)
(binding [*my-dynamic-var* "foobar!"] ; This val will be available across Clojure threads
  (future (println [:thread1 *my-dynamic-var*]))
  (future (println [:thread2 *my-dynamic-var*]))
  (println [:thread3 *my-dynamic-var*])
  (Thread/sleep 100))
  
;; %> "foobar!", "foobar!", "foobar!"
```

### How do I get dynamic (multi-threaded) profiling?

`profiled` and `profile` have a `:dynamic?` option:

```clojure
(profiled {:dynamic? true} ...) ; Activates dynamic (multi-threaded) profiling
```

This works through Clojure's standard `^:dynamic` binding conveyance.

If you really want to get fancy, you can also do _manual_ multi-threaded profiling using `tufte/stats-accumulator`.

### What's the difference between Clock Time and Accounted Time?

> This question refers to the values reported by the `format-stats` util

**Clock time** is just the total real-world time that elapsed between the start and end of a `profiled` or `profile` call. This is the amount of time that you'd have seen pass on a stopwatch in your hand.

**Accounted time** is the total execution time tracked by all `p` forms during the same period. It can be:

Outcome                    | Meaning                                                 |
-------------------------- | ------------------------------------------------------- |
`(< accounted clock-time)` | Some work was done that wasn't tracked by any `p` forms |
`(> accounted clock-time)` | Nested `p` forms, and/or multi-threaded profiling[1]    |

**[1]** For example: if you're doing concurrent work on 6 threads, then you can do 6ms of work for each 1ms of clock time.

## Contacting me / contributions

Please use the project's [GitHub issues page] for all questions, ideas, etc. **Pull requests welcome**. See the project's [GitHub contributors page] for a list of contributors.

Otherwise, you can reach me at [Taoensso.com]. Happy hacking!

\- [Peter Taoussanis]

## License

Distributed under the [EPL v1.0] \(same as Clojure).  
Copyright &copy; 2016 [Peter Taoussanis].

<!--- Standard links -->
[Taoensso.com]: https://www.taoensso.com
[Peter Taoussanis]: https://www.taoensso.com
[@ptaoussanis]: https://www.taoensso.com
[More by @ptaoussanis]: https://www.taoensso.com
[Break Version]: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md
[support my continued open-source Clojure/Script work]: http://taoensso.com/clojure/backers

<!--- Standard links (repo specific) -->
[CHANGELOG]: https://github.com/ptaoussanis/tufte/releases
[API]: http://ptaoussanis.github.io/tufte/
[GitHub issues page]: https://github.com/ptaoussanis/tufte/issues
[GitHub contributors page]: https://github.com/ptaoussanis/tufte/graphs/contributors
[EPL v1.0]: https://raw.githubusercontent.com/ptaoussanis/tufte/master/LICENSE
[Hero]: https://raw.githubusercontent.com/ptaoussanis/tufte/master/hero.png "Title"

<!--- Unique links -->
[Edward Tufte]: https://en.wikipedia.org/wiki/Edward_Tufte
[YourKit]: http://www.yourkit.com/)
[JProfiler]: http://www.ej-technologies.com/products/jprofiler/overview.html
[VisualVM]: http://docs.oracle.com/javase/6/docs/technotes/guides/visualvm/index.html
[@ptaoussanis/Timbre]: https://github.com/ptaoussanis/timbre
[@hugoduncan/Criterium]: https://github.com/hugoduncan/criterium

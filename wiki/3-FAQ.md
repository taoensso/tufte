# How does Tufte compare to YourKit, etc.

The traditional recommendation for Clojure profiling has usually been to use a standard JVM profiling tool like [YourKit](https://www.yourkit.com/), [JProfiler](https://www.ej-technologies.com/products/jprofiler/overview.html), or [VisualVM](https://visualvm.github.io/).

And they can certainly do the job, but they also tend to be a little hairy: requiring special effort to use, and often producing gobs of information that can be difficult or time-consuming to meaningfully interpret.

In contrast, Tufte offers some interesting benefits:

- A **cross-platform API** that works seamlessly between your server (Clj) and client (Cljs) applications
- Arbitrary **application-aware, form-level** profiling; measure _just_ what you care about at the application level
- Simple **thread-local or multi-threaded semantics**
- During dev/debug: check performance **right from within your REPL**
- During production: **ongoing, application-aware** conditional profiling, logging, and analysis (stats are just **Clojure maps**)

Note that JVM profiling tools can still be very handy. Tufte doesn't offer memory profiling for example, and it's not well suited to forensic or very low-level profiling.

If you want to know `clojure.lang.Numbers$LongOps.lt(Number, Number)` or memory stats, you'll want a JVM tool. If you want to know `my-fn` stats, or you want ongoing stats in production - Tufte could be a good fit.

# How does Tufte compare to the profiling in Timbre?

I developed Tufte one weekend while refactoring the profiling tools in [Timbre](https://www.taoensso.com/timbre). It's basically a refinement of the ideas from there.

I decided that I could make worthwhile improvements with some breaking API changes and a new set of dedicated docs. Tufte's implementation is cross-platform, considerably faster, and its API more flexible.

With the release of Tufte, **Timbre's profiling tools are deprecated**.

Note that Tufte's a feature **superset** of Timbre's profiling, so porting should be straightforward:

API | Timbre | Tufte
--- | --- | ---
`p` | `[id & body]` | `[id & body]`, `[opts & body]`
`profile` | `[level id & body]` | `[opts & body]`
`profiled` | `[level id & body]` | `[opts & body]`
`profile` | Output -> log | Output -> arbitrary handler-fn [1]

> **[1]** See [`tufte.timbre/add-timbre-logging-handler!`](https://taoensso.github.io/tufte/taoensso.tufte.timbre.html#var-add-timbre-logging-handler.21) for directing Tufte's `profile` output to Timbre.

# How does Tufte compare to Criterium?

They serve different (though related) use cases:

- [Criterium](https://github.com/hugoduncan/criterium) is focused on **benchmarking**
- Tufte is focused on **profiling** (especially ongoing monitoring)

Profiling is:

- Usually more **invasive** (often uses some kind of instrumentation)
- Usually more **informative** (helps to explain the *cause* of observed measurements)
- Is often done as part of **ongoing monitoring**

Both Criterium and Tufte can be used for performance measurement + comparison, and both can be used for performance optimization.

The main tradeoff is: profiling generally provides deeper information at the cost of increased setup cost (instrumentation).

Library | Measures | Use for | During | Emphasis
--- | --- | --- | --- | ---
Criterium | 1 Clojure form | Benchmarking | Dev | Accuracy
Tufte | >=1 Clojure/Script forms | Profiling, basic benchmarking | Dev, prod | Flexibility, speed

So Criterium produces very accurate stats for a _single_ Clojure expression while Tufte produces _combined stats_ for an _arbitrary_ number of Clojure/Script expressions, possibly over time.

For example:

- Use **Criterium** for a one-off measurement or comparison of the performance of two libraries.
- Use **Tufte** to measure or monitor the performance of various parts of your system and how they relate.

# What's the difference between thread-local and dynamic (multi-threaded) profiling?

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

# How to do dynamic (multi-threaded) profiling?

`profiled` and `profile` have a `:dynamic?` option:

```clojure
(profiled {:dynamic? true} ...) ; Activates dynamic (multi-threaded) profiling
```

This works through Clojure's standard `^:dynamic` binding conveyance.

If you really want to get fancy, you can also do _manual_ multi-threaded profiling using [`tufte/stats-accumulator`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-stats-accumulator).

# What's the difference between Clock Time and Accounted Time?

> This question refers to the values reported by the [`format-pstats`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-format-pstats) util

**Clock time** is just the total real-world time that elapsed between the start and end of a `profiled` or `profile` call. This is the amount of time that you'd have seen pass on a stopwatch in your hand.

**Accounted time** is the total execution time tracked by all `p` forms during the same period. It can be:

Outcome | Meaning
--- | ---
`(< accounted clock-time)` | Some work was done that wasn't tracked by any `p` forms
`(> accounted clock-time)` | Nested `p` forms, and/or multi-threaded profiling[1]

> **[1]** For example: if you're doing concurrent work on 6 threads, then you can do 6ms of work for each 1ms of clock time.

# How to time across a promise / async handler / etc.?

A low-level util ([`tufte/capture-time!`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-capture-time.21)) is provided for this and similar use cases.
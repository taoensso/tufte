> This project uses [Break Versioning](https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md)

## v2.2.0 - 2020 Sep 14

```clojure
[com.taoensso/tufte "2.2.0"]
```

> This is a major feature release. Should be **non-breaking**.  
> See [here](https://github.com/ptaoussanis/encore#recommended-steps-after-any-significant-dependency-update) for recommended steps when updating any Clojure/Script dependencies.

**Changes** since `v2.1.0`:

* [DEPRECATED] `set-min-level!`, `set-ns-pattern!`, `with-min-level`, `with-ns-pattern`: prefer just using the relevant dynamic vars directly.

**New** since `v2.1.0`:

* `*min-level*` can now be an int, or a `[[<ns-pattern> <int>] ...]` for ns-specific levels.
* `*min-level*` init val can now be set via `taoensso.tufte.min-level.edn` JVM property, or `TAOENSSO_TUFTE_MIN_LEVEL_EDN` env var.
* `*ns-filter*` can now be a pred fn, or an ns-pattern (which will be auto compiled to a pred fn).
* `*ns-filter*` init val can now be set via `taoensso.tufte.ns-pattern.edn` JVM property, or `TAOENSSO_TUFTE_NS_PATTERN_EDN` env var.
* Docstring improvements.


## v2.2.0-RC1 - 2020 Aug 29

```clojure
[com.taoensso/tufte "2.2.0-RC1"]
```

> This is a major feature release. Should be non-breaking.
> See [here](https://github.com/ptaoussanis/encore#recommended-steps-after-any-significant-dependency-update) for a tip re: general recommended steps when updating any Clojure/Script dependencies.

**New** since `v2.1.0`:

* `*min-level*` can now be an int, or a `[[<ns-pattern> <int>] ...]` for ns-specific levels.
* `*min-level*` init val can now be set via `taoensso.tufte.min-level` JVM property, or `TAOENSSO_TUFTE_MIN_LEVEL` env var.
* `*ns-filter*` can now be a pred fn, or an ns-pattern (which will be auto compiled to a pred fn).
* `*ns-filter*` init val can now be set via `taoensso.tufte.ns-pattern` JVM property, or `TAOENSSO_TUFTE_NS_PATTERN` env var.

**Changes** since `v2.1.0`:

* [DEPRECATED] `set-min-level!`, `set-ns-pattern!`, `with-min-level`, `with-ns-pattern`: prefer just using the relevant dynamic vars directly.
* Some docstring improvements.

## v2.1.0 - 2019 Sep 7

```clojure
[com.taoensso/tufte "2.1.0"]
```

> Identical to "2.1.0-RC5" (besides some docstring tweaks).
>
> This is an API-non-breaking feature release (though `format-pstats` output has changed). Big thanks to **@ivarref** for much of the work+input on this release!
>
> Feedback and bug reports welcome, especially for the Cljs implementation which I'm not currently using myself.
>
> Thank you!

* [#54] **New**: `merge-pstats` clock total should now mostly be lossless (@ivarref)
* [#37] **New**: Improve+expand accumulator utils to make one common pattern of app-wide profiling trivial
* [#32 #31 #30] **New**: `format-pstats`: add configurable columns filter, sort, id format (@ivarref)
* [#41] **New**: Add abbreviated `format-id-fn` (@ivarref)
* [#19] **New**: Add `defnp-` macro
* **New**: `defnp`: add support for explicit `^{:tufte/id _}` to override automatic ids
* **New**: New optional stats columns: `:p25`, `:p75`
* [#33] **Fix**: cljs implementation was broken
* [#54] **Fix**: `merge-pstats` compaction wasn't always working, could lead to OOMs (@ivarref)
* [#40] **Change**: Left-align pid in `format-pstats` output (@meeseekz)

## v2.0.1 - 2018 Apr 27

```clojure
[com.taoensso/tufte "2.0.1"]
```

> This is a trivial, non-breaking hotfix release

* [#27] Hotfix: fix broken Cljs build (@atroche)

## v2.0.0 - 2018 Apr 22

```clojure
[com.taoensso/tufte "2.0.0"]
```

> This is a major, **breaking** feature release. Please report any issues, thank you!

Tufte v2 introduces API flexibility improvements to expand on Tufte's strengths as an **ongoing application performance monitoring tool**.

Main objectives of the v2 API:

  - **Defer more profiling costs** off-thread, and on demand.
  - **Lossless merging** of profiling stats from different runs/threads/over-time.
  - Inclusion of **percentiles** in profiling stats.

These objectives are related: percentiles are expensive to calculate, so infeasible to do on the same thread as profiling capture. Also, stats like percentiles are inherently inaccurate to merge.

By deferring the conversion of captured times to stats, the Tufte v2 API makes it easy to support both lossless merging (by merging times, not stats) - and the inclusion of more expensive stats like percentiles (since the cost of stats computation can be incurred on demand, and off the capturing thread).

The net result: the v2 API is more flexible since it **decouples time capture and stats computation**.

The decoupling is particularly handy for the **ongoing performance monitoring** of production applications: the application thread/s can inexpensively capture timing info for deferred/async digestion by a background analytics/monitoring task.

**tl;dr** The v2 API returns `PStats` objects that can be **~losslessly merged**, or **derefed to actually compute statistics**.


### Detailed changes

* **BREAKING (1)**: `profiled` return value has changed: `[<result> <?stats-map>]` -> `[<result> <?pstats-obj>]`.

* **BREAKING (2)**: `profile` handler keys have changed: `:stats`, `:stats-str_` -> `:pstats`, `:pstats-str_`.

* **BREAKING (3)**: Utils were renamed: `merge-stats` -> `merge-pstats`, `format-stats` -> `format-pstats`.

* **BREAKING (4)**: Stats maps keys have changed: `#{:count :min :max :mean :mad :time}` -> `#{:n :min :max :mean :mad :sum :p50 :p90 :p95 :p99}`.

* **New**: Various readability tweaks+improvements to the output of `format-pstats`.

* **New**: `profiled` and `profile` calls now accept an `:nmax` option to control size of captured time buffers (used to prevent OOMs).

* **New**: Stats will now automatically include `:tufte/compaction` info when buffer sizes were exceeded.

* **New**: added several new fully-documented low-level primitives for advanced users: `new-pdata`, `with-profiling`, `capture-time!`.

* **Impl**: Significant performance improvements to both dynamic and thread-local profiling.


### Migration guide

- If you use only `profile` with an out-the-box (println or timbre) handler, there are **no breaking changes for you**.

- If you use `profile` with any custom handlers, you'll need to update your handler fns:

  - Stats map keys (as returned by @pstats) have changed (see **BREAKING (1)** above).
  - Map keys given to handler fns have changed (see **BREAKING (2)** above).

- If you use `profiled`, see **BREAKING (1, 3, 4)** above.


## v1.4.0 - 2018 Mar 21

```clojure
[com.taoensso/tufte "1.4.0"]
```

> This is a minor, non-breaking feature release

* **Impl**: tweak `format-stats` output spacing (@kassapo)

## v1.3.0 - 2018 Mar 11

```clojure
[com.taoensso/tufte "1.3.0"]
```

> This is a minor, non-breaking feature release

* [#21] **New**: support minutes in `format-stats` table (@austinhaas)

## v1.2.0 - 2018 Mar 8

```clojure
[com.taoensso/tufte "1.2.0"]
```

> This is a minor, non-breaking feature release

* **New**: Add low-level `capture-time!` util fn
* **Impl**: Bump dependencies, incl. ClojureScript

## v1.1.3 - 2018 Feb 2

```clojure
[com.taoensso/tufte "1.1.3"]
```

> This is a non-breaking hotfix release.

**Hotfix**: Fix broken micro-optimization for conditional profiling (@balajirrao, @kassapo)

## v1.1.2 - 2017 Sep 21

```clojure
[com.taoensso/tufte "1.1.2"]
```

> This is a non-breaking hotfix release.

**Hotfix**: Fix broken handler namespace filters (@knotschi)

## v1.1.1 - 2016 Dec 17

```clojure
[com.taoensso/tufte "1.1.1"]
```

> This is a non-breaking hotfix release.

**Hotfix**: [#16] Fix `merge-stats` typo (@atdixon)
**Impl**: [#14] Add short mention of laziness to relevant doc-strings

## v1.1.0 - 2016 Oct 18

```clojure
[com.taoensso/tufte "1.1.0"]
```

> **NB** This release now **requires Clojure >= 1.7**. Please continue to use `v1.0.x` if you're stuck on Clojure < 1.7.

**Impl**: Bump minimum Clojure dependency, migrate from .cljx to .cljc

## v1.0.2 - 2016 Aug 2

```clojure
[com.taoensso/tufte "1.0.2"]
```

**Hotfix**: [#6] Fix non-dynamic p-nesting support

## v1.0.1 - 2016 Jul 27

```clojure
[com.taoensso/tufte "1.0.1"]
```

**Hotfix**: [#4] Fix broken reference (@federkasten)

## v1.0.0 - 2016 Jul 24

```clojure
[com.taoensso/tufte "1.0.0"]
```

> No changes from `1.0.0-RC2`

## v1.0.0-RC2 - 2016 Jul 14

```clojure
[com.taoensso/tufte "1.0.0-RC2"]
```

**Hotfix**: [#2] Timbre handler: fix broken timbre level support (@gtrak)

## v1.0.0-RC1 - 2016 Jul 11

```clojure
[com.taoensso/tufte "1.0.0-RC1"]
```

> Initial release

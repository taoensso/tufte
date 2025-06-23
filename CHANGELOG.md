This project uses [**Break Versioning**](https://www.taoensso.com/break-versioning).

---

# `v3.0.0` (2025-06-23)

- **Dependency**: [on Clojars](https://clojars.org/com.taoensso/tufte/versions/3.0.0)
- **Versioning**: [Break Versioning](https://www.taoensso.com/break-versioning)

This is a **major upgrade** with many improvements. It includes **breaking changes** for a minority of users. Please see below for changes, and linked commit messages for relevant migration info and motivations.

Sincere apologies for the hassle! Most users (esp. those using defaults) should be able to upgrade without any modifications, but as always please **report any unexpected problems** on [GitHub](https://github.com/taoensso/telemere/issues) or the [Slack channel](https://www.taoensso.com/tufte/slack), and feel free to ping if I can provide any assistance! 🙏

\- [Peter Taoussanis](https://www.taoensso.com)

## Breaking since `v2.7.0` (2024-04-15)

- ➤ **\[mod]** \[BREAKING] Changed arg given to custom `profile` handlers \0f73778], see [`help:signal-content`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:signal-content) for new arg
- ➤ **\[mod]** \[BREAKING] Change default `min-level` (2 -> `:info`) \[025ff70]
- ➤ **\[mod]** \[BREAKING] New API for draining [stats accumulator](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#stats-accumulator) \[b2c866b]
- ➤ **\[mod]** \[BREAKING] Omit `defn_` / `fn_` id prefix when using [`defnp`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#defnp), [`defnp-`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#defnp-), [`fnp`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#fnp) \[5f5d2ef]
- ➤ **\[mod]** \[BREAKING] Remove `:file` key from [pstats](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:pstats-content) `:loc` maps \[79ccac5]

## New since `v2.7.0` (2024-04-15)

- \[new] Added [Telemere](https://www.taoensso.com/telemere) and [Trove](https://www.taoensso.com/trove) handlers.
- \[new] Many new options added to [`profiled`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profiled), [`profile`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profile) (see linked docstrings)
- \[new] Many new options added to [`add-handler!`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#add-handler!) (see linked docstring, [`help:handler-dispatch-options`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:handler-dispatch-options))
- \[new] Automatic handler stats (see [`get-handlers-stats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#get-handlers-stats))
- \[new] Major documentation improvements \[7d14517], \[1d7abfd], etc.
- \[new] Major performance improvements \[312bb4c], \[bbc8dfc], \[e51df6a], \[2b6e276], \[0f73778], etc.

## Since `v3.0.0-RC1` (2025-04-30)

- \[mod] Some changes to the options and output to the Timbre and Telemere handlers \[48e82ad]. Please recheck if you're using custom options.

---

# `v3.0.0-RC1` (2025-04-30)

- **Dependency**: [on Clojars](https://clojars.org/com.taoensso/tufte/versions/3.0.0-RC1)
- **Versioning**: [Break Versioning](https://www.taoensso.com/break-versioning)

This is a **major upgrade** of Tufte with *many* new features and improvements, including expanded documentation and a rich new API for filtering and handling that it has in common with [Telemere](https://www.taoensso.com/telemere).

v3 includes **breaking changes** (indicated by ➤ below) that may affect some (but not all) users. Please see the **relevant linked commit messages** below for details and migration info. Sincere apologies for any hassle while upgrading! My hope is that most users will be unaffected, and that those affected will be able to migrate easily.

As always please **report any unexpected problems** on [GitHub](https://github.com/taoensso/telemere/issues) or the [Slack channel](https://www.taoensso.com/tufte/slack), and feel free to ping if I can provide any assistance! 🙏

\- [Peter Taoussanis](https://www.taoensso.com)

## Changes since `v2.7.0` (2024-04-15)

Please read **linked commit messages** carefully in case you may be affected:

- ➤ **\[mod]** \[BREAKING] New API for draining [stats accumulator](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#stats-accumulator) \[b2c866b]
- ➤ **\[mod]** \[BREAKING] Omit `defn_` / `fn_` id prefix when using [`defnp`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#defnp), [`defnp-`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#defnp-), [`fnp`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#fnp) \[5f5d2ef]
- ➤ **\[mod]** \[BREAKING] Remove `:file` key from [pstats](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:pstats-content) `:loc` maps \[79ccac5]
- ➤ **\[mod]** \[BREAKING] Change default `min-level` (2 -> `:info`) \[025ff70]
- ➤ **\[mod]** \[BREAKING] Changed arg given to `profile` handlers \[a778ac9], see [`help:signal-content`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:signal-content) for new arg

## New since `v2.7.0` (2024-04-15)

- \[new] *Many* new options available to [`profiled`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profiled), [`profile`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profile) (see linked docstrings)
- \[new] *Many* new options available to [`add-handler!`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#add-handler!) (see linked docstring, [`help:handler-dispatch-options`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:handler-dispatch-options))
- \[new] Significantly expanded filtering and handler capabilities, see [`help:filters`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:filters) and [`help:handlers`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:handlers)
- \[new] Automatic handler stats (see [`get-handlers-stats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#get-handlers-stats))
- \[new] Add [Telemere](https://www.taoensso.com/telemere) handler \[d3ab990]
- \[new] Major documentation improvements \[7d14517], \[1d7abfd], etc.
- \[new] Major code refactor for improved performance and maintenance \[312bb4c], \[bbc8dfc], \[e51df6a], \[2b6e276], \[0f73778], etc.

---

# `v2.7.0` (2025-04-15)

- **Dependency**: [on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.7.0)
- **Versioning**: [Break Versioning](https://www.taoensso.com/break-versioning)

This is a **maintenance release** focused on updating dependencies. It **drops support for Clojure v1.9** but should otherwise be a safe update for users of `v2.6.x`.

This is expected to be the last v2.x release before the upcoming Tufte v3.

## Since `v2.6.3` (2023-09-27)

- \[new] Use [Truss exceptions](https://cljdoc.org/d/com.taoensso/truss/CURRENT/api/taoensso.truss#ex-info) on errors \[8f89aac]
- \[nop] Update dependencies

---

# `v2.6.3` (2023-09-27)

> 📦 [Available on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.6.3), this project uses [Break Versioning](https://www.taoensso.com/break-versioning).

This is a non-breaking **maintenance** release, mostly intended to synchronize dependencies with my other recent library releases.

This should be a safe update for users of `v2.6.x`.

---

# `v2.6.1` (2023-08-18)

> 📦 [Available on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.6.1), this project uses [Break Versioning](https://www.taoensso.com/break-versioning).

This is a non-breaking **hotfix** release. Please upgrade if you're using `v2.6.0`.

## Fixes since `v2.6.0`

* c64ba59 [fix] Hotfix: don't add non-numerical val to dereferenced sstats
* 27d0090 [fix] Hotfix: bump Encore version

---

# `v2.6.0` (2023-08-14)

> 📦 [Available on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.6.0), this project uses [Break Versioning](https://www.taoensso.com/break-versioning).

This is a non-breaking **maintenance** release, but includes changes (improvements) to the formatted string output by [`format-pstats`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-format-pstats). This shouldn't affect most users, but may affect you if you do any manual parsing of formatted pstats strings.

## Changes since `v2.5.1`

* a4aee9f [mod] [Stats ns] Refactor pstats formatting code

## Fixes since `v2.5.1`

* 4f6e05f [fix] [Stats ns] [#67] Don't interpolate percentiles
* dce5e40 [fix] [Stats ns] Broken `SortedDoubles` printing

---

# `v2.5.1` (2023-07-18)

> 📦 [Available on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.5.1), this project uses [Break Versioning](https://www.taoensso.com/break-versioning).

Identical to `v2.5.0`, but synchronizes Encore dependency with my recent library releases (Timbre, Tufte, Sente, Carmine, etc.) to prevent confusion caused by dependency conflicts.

This is a safe update for users of `v2.5.0`.

---

# `v2.5.0` (2023-07-13)

> 📦 [Available on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.5.0), this project uses [Break Versioning](https://www.taoensso.com/break-versioning).

This is intended as a **non-breaking feature release**, but it touches a fair bit of code so **please keep an eye out** for (and let me know about) any unexpected problems - thank you! 🙏

See **referenced commit messages** for details.

## Changes since `v2.4.5`

* ebed6d0 [mod] Change `capture-time!` from a fn to a macro
* 7442835 [mod] Rename format columns: `:n-calls` -> `:n`, `:total` -> `:sum`

## New since `v2.4.5`

* 2a38e1a [new] [#66] Track callsite locations of `p` ids
* 0a2484f [new] `defnp` and `fnp` now track cross-arity stats, add tests
* GraalVM compatibility is now tested during build

---

# `v2.4.5` (2022-10-27)

```clojure
[com.taoensso/tufte "2.4.5"]
```

> This is a very minor maintenance release. It should be **non-breaking**.
> See [here](https://github.com/taoensso/encore#recommended-steps-after-any-significant-dependency-update) for recommended steps when updating any Clojure/Script dependencies.

## Changes since `v2.3.0`

* Update dependencies
* Some minor refactoring and performance improvements

---

# `v2.3.0` (2022-09-05)

> Identical to `v2.3.0-RC1` (2022-07-18)

```clojure
[com.taoensso/tufte "2.3.0"]
```

> This is a major feature and fix release. Should be **non-breaking** for vast majority of folks.  
> See [here](https://github.com/taoensso/encore#recommended-steps-after-any-significant-dependency-update) for recommended steps when updating any Clojure/Script dependencies.

## Behavioural changes since `v2.2.0`

* Stats format: refactor, use default JVM locale thousands separator for call counts (Clj only)

## Fixes since `v2.2.0`

* `add-accumulating-handler!`: fix broken default val and examples
* [#64] Broken concurrent `(local (local ...))` nested profiling (Clj only) (@awkay)
* Broken `(dynamic (local ...))` nested profiling
* Broken nested profiling tests
* Correctly document which `profile/d` options are compile-time and runtime

## New since `v2.2.0`

* Throw on invalid compile-time `profile/d` options
* [#61] Improve docstrings re: async code

## Other changes since `v2.2.0`

* Refactor: rename `pdata-proxy` -> `pdata-local`
* Refactor `pdata-local`
* Update dependencies

---

# Earlier releases

See [here](https://github.com/taoensso/tufte/releases) for earlier releases.

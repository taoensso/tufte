This project uses [**Break Versioning**](https://www.taoensso.com/break-versioning).

---

# `v2.6.1` (2023-08-18)

> 📦 [Available on Clojars](https://clojars.org/com.taoensso/tufte/versions/2.6.1), this project uses [Break Versioning](https://www.taoensso.com/break-versioning).

This is a non-breaking **hotfix** release. Please upgrade if you're using `v2.6.0`.

## Fixes since `v2.6.0`

* 386699b [fix] Hotfix: don't add non-numerical val to derefenced sstats
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

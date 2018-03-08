> This project uses [Break Versioning](https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md)

## v1.2.0 - 2018 Mar 8

```clojure
[com.taoensso/tufte "1.2.0"]
```

> This is a non-breaking feature release.

**New**: Add low-level `capture-time!` util fn
**Impl**: Bump dependencies, incl. ClojureScript

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

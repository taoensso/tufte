<a href="https://www.taoensso.com/clojure" title="More stuff by @ptaoussanis at www.taoensso.com"><img src="https://www.taoensso.com/open-source.png" alt="Taoensso open source" width="340"/></a>  
[**API**][cljdoc] | [**Wiki**][GitHub wiki] | [Latest releases](#latest-releases) | [Slack channel][]

# Tufte

### Simple performance monitoring library for Clojure/Script

**Tufte** allows you to **easily monitor the ongoing performance** of your Clojure and ClojureScript applications in production and other environments.

It provides **sensible application-level metrics**, and gives them to you as **Clojure data** that can be easily analyzed programatically.

Works great with [Telemere](https://www.taoensso.com/telemere), enabling unique **next-gen observability** of Clojure/Script applications.

<img width="600" src="../../raw/master/hero.png" alt="Carte Figurative"/>

> [Carte Figurative](https://en.wikipedia.org/wiki/Charles_Joseph_Minard#The_map_of_Napoleon's_Russian_campaign), one of [Edward Tufte](https://en.wikipedia.org/wiki/Edward_Tufte)'s favourite data visualizations.

## Latest release/s

- `2025-04-15` `2.7.0`: [release info](../../releases/tag/v2.7.0)

[![Main tests][Main tests SVG]][Main tests URL]
[![Graal tests][Graal tests SVG]][Graal tests URL]

See [here][GitHub releases] for earlier releases.

## Why Tufte?

- Small, **fast**, cross-platform Clojure/Script codebase
- Sensible **application-level metrics** without the obscure JVM-level noise
- **Metrics as Clojure maps**: easily aggregate, **analyse**, log, serialize to db, etc.
- **Tiny**, flexible API: [`p`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-p), [`profiled`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-profiled), [`profile`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-profile)
- Great **compile-time elision** and **runtime filtering** support
- Arbitrary Clojure/Script **form-level** profiling
- Full support for **thread-local** and **multi-threaded** profiling

## Quick example

```clojure
(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])

;; Request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

;;; Define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

;; Let's check how these fns perform:

(profile ; Profile any `p` forms called during body execution
  {} ; Profiling options; we'll use the defaults for now
  (dotimes [_ 5]
    (p :get-x (get-x))
    (p :get-y (get-y))))

;; The following will be printed to *out*:
;;
;; pId      nCalls      Min    50% ≤    90% ≤    95% ≤    99% ≤      Max     Mean   MAD    Clock  Total
;; :get-x        5    501ms    503ms    505ms    505ms    505ms    505ms    503ms   ±0%    2.52s    53%
;; :get-y        5     78ms    396ms    815ms    815ms    815ms    815ms    452ms  ±48%    2.25s    47%
;;
;; Accounted                                                                               4.78s   100%
;; Clock                                                                                   4.78s   100%
```

## Documentation

- [Wiki][GitHub wiki] (getting started, usage, etc.)
- API reference via [cljdoc][cljdoc]
- Support via [Slack channel][] or [GitHub issues][]

## Funding

You can [help support][sponsor] continued work on this project, thank you!! 🙏

## License

Copyright &copy; 2016-2025 [Peter Taoussanis][].  
Licensed under [EPL 1.0](LICENSE.txt) (same as Clojure).

<!-- Common -->

[GitHub releases]: ../../releases
[GitHub issues]:   ../../issues
[GitHub wiki]:     ../../wiki
[Slack channel]: https://www.taoensso.com/tufte/slack

[Peter Taoussanis]: https://www.taoensso.com
[sponsor]:          https://www.taoensso.com/sponsor

<!-- Project -->

[cljdoc]: https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte

[Clojars SVG]: https://img.shields.io/clojars/v/com.taoensso/tufte.svg
[Clojars URL]: https://clojars.org/com.taoensso/tufte

[Main tests SVG]:  https://github.com/taoensso/tufte/actions/workflows/main-tests.yml/badge.svg
[Main tests URL]:  https://github.com/taoensso/tufte/actions/workflows/main-tests.yml
[Graal tests SVG]: https://github.com/taoensso/tufte/actions/workflows/graal-tests.yml/badge.svg
[Graal tests URL]: https://github.com/taoensso/tufte/actions/workflows/graal-tests.yml

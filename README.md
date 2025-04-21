<a href="https://www.taoensso.com/clojure" title="More stuff by @ptaoussanis at www.taoensso.com"><img src="https://www.taoensso.com/open-source.png" alt="Taoensso open source" width="340"/></a>  
[**API**][cljdoc] | [**Wiki**][GitHub wiki] | [Latest releases](#latest-releases) | [Slack channel][]

# Tufte

### Simple performance monitoring library for Clojure/Script

**Tufte** allows you to **easily monitor the ongoing performance** of your Clojure and ClojureScript applications in production and other environments.

It provides **sensible application-level metrics**, and gives them to you as **Clojure data** that can be easily analyzed programatically.

It works great with [Telemere](https://www.taoensso.com/telemere) and [Truss](https://www.taoensso.com/truss) to help enable unique **next-gen observability** for Clojure and ClojureScript applications.

<img width="600" src="../../raw/master/hero.png" alt="Carte Figurative"/>

> [Carte Figurative](https://en.wikipedia.org/wiki/Charles_Joseph_Minard#The_map_of_Napoleon's_Russian_campaign), one of [Edward Tufte](https://en.wikipedia.org/wiki/Edward_Tufte)'s favourite data visualizations.

## Latest release/s

- `2025-04-21` `v3.0.0-beta1`: (forthcoming) [release info](../../releases/tag/v3.0.0-beta1) (shares filter + handler API with [Telemere](https://www.taoensso.com/telemere))
- `2025-04-15` `v2.7.0`: (stable) [release info](../../releases/tag/v2.7.0)

[![Main tests][Main tests SVG]][Main tests URL]
[![Graal tests][Graal tests SVG]][Graal tests URL]

See [here][GitHub releases] for earlier releases.

## Why Tufte?

- Small, **fast**, cross-platform Clojure/Script codebase.
- Sensible **form-level** profiling without the low-level JVM noise.
- **Metrics as Clojure maps**: easily aggregate, **analyse**, log, serialize to db, etc.
- **Tiny**, flexible API: [`p`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#p), [`profiled`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profiled), [`profile`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profile).
- Full support for **thread-local** and dynamic (**multi-threaded**) profiling.
- Rich [filtering](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:filters): **conditional profiling** by namespace, id pattern, level, level by namespace pattern, etc.
- Rich a/sync [handling](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:handler-dispatch-options): with sampling, rate limiting, back-pressure monitoring, etc.
- Includes handlers for [Telemere](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte.telemere#handler:telemere), [Timbre](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte.timbre#handler:timbre), and [consoles](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#handler:console) (`*out*`, etc.).

## Quick example

```clojure
(require '[taoensso.tufte :as tufte)

;; Send `profile` signals to console
(tufte/add-handler! :my-console-handler (tufte/handler:console))

;;; Define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

;; Let's check how these fns perform:
(tufte/profile   ; Profile any `p` forms called during body execution
  {:level :info} ; Rich set of filtering options available
  (dotimes [_ 5]
    (tufte/p :get-x (get-x))
    (tufte/p :get-y (get-y))))

;; The following will be printed to *out*:
;; 2025-04-18T11:23:08.820786Z INFO MyHost readme-examples[15,1]
;; <<< pstats <<<
;; pId           nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total
;;
;; :get-y             5      238ms      501ms      981ms      981ms      981ms      981ms      618ms  ±42%      3.09s    55%
;; :get-x             5      501ms      502ms      505ms      505ms      505ms      505ms      503ms   ±0%      2.51s    45%
;;
;; Accounted                                                                                                    5.60s   100%
;; Clock                                                                                                        5.60s   100%
;; >>> pstats >>>
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

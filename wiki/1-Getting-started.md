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

Wrap the forms you'd like to (sometimes) profile with [`p`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#p) and give them an id (unique keyword):

```clojure
(defn get-customer-info []
  (let [raw-customer-map (tufte/p ::get-raw-customer (fetch-from-db))]
    (tufte/p ::enrich-raw-customer
      (do-some-work raw-customer-map))))
```

Tufte will record the execution times of these `p` forms whenever profiling is active. 

Whether or not profiling is active, `p` forms **always return their normal body result**. So you never need to worry about Tufte messing with your return values.

## Step 2: activate profiling

Activate profiling of `p` forms with:

| API                                                                                       | Effect                                                                                                                                                                                                                                                      |
| ----------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`profiled`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profiled) | Returns `[<body-result> <?pstats>]`                                                                                                                                                                                                                         |
| [`profile`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profile)   | Returns `<body-result>` and sends [profiling signal](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:signal-content) map to [registered handlers](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:handlers) |

Since [`pstats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:pstats-content) and [profiling signals](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:signal-content) are structured Clojure data, they're trivial to work with. Save to a db, log, `put!` to a `core.async` channel, filter, aggregate, use for a realtime analytics dashboard, examine for outliers or unexpected behaviour, feed into your other performance/analytics systems, etc.

- Use `profiled` to consume profiling results **directly at the call site**
- Use `profile` to consume profiling results **later/elsewhere**

Between the two, you have great flexibility for a wide range of use cases in production and during development/debugging.

## Conditional profiling

Tufte offers the same rich filtering capabilities available to [Telemere](https://www.taoensso.com/telemere), using the exact same API: 

```clojure
(tufte/profile
  ;; See `profile` docstring for all options:
  {:dynamic? true ; Use dynamic (multi-threaded) profiling
   :level :debug  ; Must be allowed by `set-min-level!`, etc.
   :id    :id1    ; Must be allowed by `set-id-filter!`, etc.

   :sample 0.75     ; Profile 75% of calls
   :when  (my-pred) ; Must be truthy at runtime to profile
   :limit [[1 2000]
           [2 60000]]   ; Rate limit (1 per 2 secs, 2 per minute)
   :limit-by my-user-id ; Rate limit per unique user-id
   }

  ;; Code that has forms wrapped with `tufte/p`:
  (my-code))
```

See [help:filtering](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:filtering) and the [`profiled`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profiled) / [`profile`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profile) docstrings for details.

## Formatting `pstats`

Once `profiled` or `profile` complete, you'll have access to a [`pstats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:pstats-content) object.

These can be:

- Merged with one another using [`merge-pstats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#merge-pstats)
- Derefed to get detailed summary statistics of wrapped `p` runtimes
- Passed to [`format-pstats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#format-pstats) to get a string table of summary statistics

An example:

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
;; 2025-04-18T11:23:08.820786Z INFO MyHost readme-examples[15,1]
;; <<< pstats <<<
;; pId         nCalls      50% ≤       Mean      Clock  Total
;; defn_get-y       5      572ms      567ms      2.84s    53%
;; defn_get-x       5      500ms      500ms      2.50s    47%
;;
;; Accounted                                     5.34s   100%
;; Clock                                         5.34s   100%
;; >>> pstats >>>
```
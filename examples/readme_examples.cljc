(ns readme-examples
  "Basic examples that appear in the Truss README."
  (:require [taoensso.tufte]))

(comment

(taoensso.tufte/refer-tufte) ; Setup Tufte's ns imports (works with clj only)
(taoensso.tufte/add-basic-println-handler! {}) ; Send `profile` stats to `println`

;;; Let's define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

;; How do these fns perform? Let's check:

(profile ; Activate profiling w/in body
  {} ; Profiling options; we'll use the default for now
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
)

(comment

(ns my-clj-ns ; Clojure namespace
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(ns my-cljs-ns ; ClojureScript namespace
  (:require [taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]))

)

(comment

(defn get-customer-info []
  (let [raw-customer-map (p ::get-raw-customer (fetch-from-db))]
    (p ::enrich-raw-customer
      (do-some-work raw-customer-map))))

)

(comment

"foo.bar.baz"
"foo.bar.*"
#{"foo.bar.*" "some.lib.*"}
{:whitelist #{"foo.bar.*"} :blacklist #{"noisy.lib.*"}}

)

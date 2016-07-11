(ns taoensso.tufte.examples
  "This is mostly stuff for the README atm."
  {:author "Peter Taoussanis (@ptaoussanis)"}
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
;;            pId      nCalls       Min        Max       MAD      Mean   Time% Time
;;         :get-y           5  171.68ms   940.84ms  264.82ms  541.28ms      52 2.71s
;;         :get-x           5   502.3ms   505.07ms    1.17ms   503.4ms      48 2.52s
;;     Clock Time                                                          100 5.22s
;; Accounted Time                                                          100 5.22s
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

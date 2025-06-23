(ns readme-examples
  "Basic examples that appear in the Truss README."
  (:require [taoensso.tufte :as tufte]))

(comment

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
;; 2025-04-18T11:23:08.820786Z INFO MyHost readme-examples[15,1] Tufte pstats
;; pId           nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total
;;
;; :get-y             5      238ms      501ms      981ms      981ms      981ms      981ms      618ms  ±42%      3.09s    55%
;; :get-x             5      501ms      502ms      505ms      505ms      505ms      505ms      503ms   ±0%      2.51s    45%
;;
;; Accounted                                                                                                    5.60s   100%
;; Clock                                                                                                        5.60s   100%

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

)

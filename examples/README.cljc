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
(tufte/profile ; Profile any `p` forms called during body execution
  {} ; Profiling options; we'll use the defaults for now
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

)

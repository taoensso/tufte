(ns taoensso.stats-tests
  (:require
   [clojure.test         :as test  :refer [deftest testing is]]
   [taoensso.encore      :as enc]
   [taoensso.tufte.stats :as stats]))

(comment
  (remove-ns      'taoensso.stats-tests)
  (test/run-tests 'taoensso.stats-tests))

;;;;

(deftest sorted-nums
  [(is (= (vec (stats/sorted-doubles nil))               []))
   (is (= (vec (stats/sorted-doubles []))                []))
   (is (= (vec (stats/sorted-doubles [1]))               [1.0]))
   (is (= (vec (stats/sorted-doubles '(3 2 1)))          [1.0 2.0 3.0]))
   (is (= (vec (stats/sorted-doubles [1 2 3 4 5 4 3 2])) [1.0 2.0 2.0 3.0 3.0 4.0 4.0 5.0]))])

(deftest multi-reduce
  [(is (= (stats/multi-reduce + 0 - 0 (range 1e4)) [49995000 -49995000]))])

(deftest weighted-nth
  [(is (= (#'stats/weighted-nth [1  3] 0.5)  2.0))
   (is (= (#'stats/weighted-nth [1 10] 0.5)  5.5))
   (is (= (#'stats/weighted-nth [1 10] 0.75) 7.75))])

(deftest percentiles
  [(is (= (stats/percentiles                       [1 5 2 4 3])  [1.0 2.0 3.0 4.0 5.0 5.0 5.0 5.0]))
   (is (= (stats/percentiles (stats/sorted-doubles [1 5 2 4 3])) [1.0 2.0 3.0 4.0 5.0 5.0 5.0 5.0]))])

;;;;

(defn- sstats-approx== [signf ss1 ss2]
  (reduce-kv
    (fn [acc k v]
      (if (enc/approx== signf v (get ss2 k))
        true
        (reduced false)))
    true
    ss1))

(comment (sstats-approx== 0.001 {:a 100 :b 100} {:a 100 :b 100.0001}))

(do
  (defn rand-longs   [n] (into [] (repeatedly n #(- ^long   (rand-int 2000) 1000))))
  (defn rand-doubles [n] (into [] (repeatedly n #(- ^double (rand     2000) 1000.0)))))

(defn- ss-merging-error [n-samples n-sample-size n-tests]
  (enc/reduce-n
    (fn [acc _]
      (let [samples (repeatedly n-samples #(rand-longs n-sample-size))
            ssstats (mapv    stats/summary-stats samples)
            approx  @(reduce stats/summary-stats-merge nil ssstats)
            exact   @(stats/summary-stats (reduce into [] samples))
            pass?
            (and
              (= (:n    exact) (:n    approx))
              (= (:mean exact) (:mean approx)))]

        (if pass? nil (reduced [exact approx]))))
    nil
    n-tests))

(comment (ss-merging-error 100 1000 10))

(deftest summary-stats
  [(is (= (stats/summary-stats nil)           nil))
   (is (= (stats/summary-stats [])            nil))
   (is (= (stats/summary-stats-merge nil nil) nil))

   (is (= (:sum @(stats/summary-stats [0  ])) 0))
   (is (= (:sum @(stats/summary-stats [1  ])) 1))
   (is (= (:sum @(stats/summary-stats [1.0])) 1.0))

   (let [ss (stats/summary-stats [1 2 3])]
     [(is (= @ss @(stats/summary-stats  ss)) "(summary-stats <ss>)")
      (is (= @ss @(stats/summary-stats @ss)) "(summary-stats <map>)")])

   (is
     (sstats-approx== 0.1 @(stats/summary-stats (range 1000))

       {:min 0, :mean 499.5, :p75 749.25, :mad-sum 250000.0, :p99 989.01,
        :n 1000, :p25 249.75, :p90 899.1, :var 83333.25, :max 999, :mad 250.0,
        :last 999, :p50 499.5, :sum 499500, :p95 949.05, :var-sum 8.333325E7}))

   (is
     (sstats-approx== 0.1
       @(stats/summary-stats-merge
          (stats/summary-stats (range 0   900))
          (stats/summary-stats (range 200 500)))

       {:min 0, :mean 424.5, :p75 611.75, :mad-sum 225000.0, :p99 791.51,
        :n 1200, :p25 237.25, :p90 724.1, :var 52587.562604340565, :max 899, :mad 187.5,
        :last 499, :p50 424.5, :sum 509400, :p95 761.55, :var-sum 6.29999E7}))

   (is (nil? (ss-merging-error 10 100 10)))

   (let [ssb (stats/summary-stats-buffered {:buffer-size 10})]
     (dotimes [n 1e5] (ssb n))
     [(is (enc/submap? @@ssb {:n 100000 :min 0 :max 99999}))
      (is (= (str ssb) "SummaryStatsBuffered[n=0, pending=0, merged=9091]"))])])

;;;;

#?(:cljs (test/run-tests))

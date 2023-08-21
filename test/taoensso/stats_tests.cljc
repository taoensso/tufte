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
  [(testing "Sorted longs"
     [(is (= (vec (stats/sorted-longs nil))               []))
      (is (= (vec (stats/sorted-longs []))                []))
      (is (= (vec (stats/sorted-longs [1]))               [1]))
      (is (= (vec (stats/sorted-longs '(3 2 1)))          [1 2 3]))
      (is (= (vec (stats/sorted-longs [1 2 3 4 5 4 3 2])) [1 2 2 3 3 4 4 5]))])

   (testing "Sorted doubles"
     [(is (= (vec (stats/sorted-doubles nil))               []))
      (is (= (vec (stats/sorted-doubles []))                []))
      (is (= (vec (stats/sorted-doubles [1]))               [1.0]))
      (is (= (vec (stats/sorted-doubles '(3 2 1)))          [1.0 2.0 3.0]))
      (is (= (vec (stats/sorted-doubles [1 2 3 4 5 4 3 2])) [1.0 2.0 2.0 3.0 3.0 4.0 4.0 5.0]))])

   (testing "Sorted nums"
     [(is (= (vec (stats/sorted-nums [1   3   2]))   [1   2   3]))
      (is (= (vec (stats/sorted-nums [1.0 3.0 2.0])) [1.0 2.0 3.0]))
      (is (= (vec (stats/sorted-nums [1.0 3   2]))   [1.0 2.0 3.0]))
      (is (= (vec (stats/sorted-nums [1   3.0 2.0])) [1   2   3]))])])

(deftest weighted-nth
  [(is (= (#'stats/weighted-nth 0.5  [1  3])  2.0))
   (is (= (#'stats/weighted-nth 0.5  [1 10])  5.5))
   (is (= (#'stats/weighted-nth 0.75 [1 10]) 7.75))])

(deftest percentiles
  [(is (= (stats/percentiles                       [1   5   2   4   3])   [1   2   3   4   5   5   5   5]))
   (is (= (stats/percentiles                       [1.0 5.0 2.0 4.0 3.0]) [1.0 2.0 3.0 4.0 5.0 5.0 5.0 5.0]))
   (is (= (stats/percentiles (stats/sorted-longs   [1.0 5.0 2.0 4.0 3.0]))[1   2   3   4   5   5   5   5]))
   (is (= (stats/percentiles (stats/sorted-doubles [1   5   2   4   3]))  [1.0 2.0 3.0 4.0 5.0 5.0 5.0 5.0]))])

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
  (defn- rand-longs   [n] (into [] (repeatedly n #(- ^long   (rand-int 2000) 1000))))
  (defn- rand-doubles [n] (into [] (repeatedly n #(- ^double (rand     2000) 1000.0)))))

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

(let [key-idx
      (into {}
        (map-indexed (fn [n k] [k n])
          [:n :sum :min :max :p25 :p50 :p75 :p90 :p95 :p99
           :mean :var :mad :var-sum :mad-sum :meta]))

      comparator (fn [k1 k2] (< (long (get key-idx k1 -1)) (long (get key-idx k2 -1))))]

  (defn- sorted-sstats [m]
    (assoc
      (apply sorted-map-by comparator (interleave (keys m) (vals m)))
      :meta (meta m))))

(deftest summary-stats
  [(is (= (stats/summary-stats nil)           nil))
   (is (= (stats/summary-stats [])            nil))
   (is (= (stats/summary-stats-merge nil nil) nil))

   (is (enc/submap? @(stats/summary-stats [0  ]) {:sum 0,   :min 0,   :max 0,   :p99 0}))
   (is (enc/submap? @(stats/summary-stats [1  ]) {:sum 1,   :min 1,   :max 1,   :p99 1}))
   (is (enc/submap? @(stats/summary-stats [1.0]) {:sum 1.0, :min 1.0, :max 1.0, :p99 1.0}))

   (let [ss (stats/summary-stats [1 2 3])]
     [(is (= @ss @(stats/summary-stats  ss)) "(summary-stats <ss>)")
      (is (= @ss @(stats/summary-stats @ss)) "(summary-stats <map>)")])

   (is
     (= (sorted-sstats @(stats/summary-stats (range 1 1001)))
       {:n 1000, :sum 500500, :min 1, :max 1000, :p25 251, :p50 501, :p75 750, :p90 900, :p95 950, :p99 990,
        :mean 500.5, :var 83333.25, :mad 250.0, :var-sum 8.333325E7, :mad-sum 250000.0, :meta {:floats? false}}))

   (is
     (= (sorted-sstats @(stats/summary-stats (range 0.5 1000)))
       {:n 1000, :sum 500000.0, :min 0.5, :max 999.5, :p25 250.5, :p50 500.5, :p75 749.5, :p90 899.5, :p95 949.5, :p99 989.5,
        :mean 500.0, :var 83333.25, :mad 250.0, :var-sum 8.333325E7, :mad-sum 250000.0, :meta {:floats? true}}))

   (is
     (= (sorted-sstats
          @(stats/summary-stats-merge
             (stats/summary-stats (range 0   900))
             (stats/summary-stats (range 200 500))))

       {:n 1200, :sum 509400, :min 0, :max 899, :p25 238, :p50 425, :p75 612, :p90 724, :p95 762, :p99 792,
        :mean 424.5, :var 52499.916666666664, :mad 187.5, :var-sum 6.29999E7, :mad-sum 225000.0, :meta {:floats? false}}))

   (is
     (= (sorted-sstats
          @(stats/summary-stats-merge
             (stats/summary-stats (range 0.5   900))
             (stats/summary-stats (range 200.5 500))))

       {:n 1200, :sum 510000.0, :min 0.5, :max 899.5, :p25 238.0, :p50 425.5, :p75 612.0, :p90 724.5, :p95 762.0, :p99 792.0,
        :mean 425.0, :var 52499.916666666664, :mad 187.5, :var-sum 6.29999E7, :mad-sum 225000.0, :meta {:floats? true}}))

   (is
     (= (sorted-sstats
          @(stats/summary-stats-merge
             ;; Mixed long/double vals
             (stats/summary-stats (range 0     900))
             (stats/summary-stats (range 200.5 500))))

       {:n 1200, :sum 509550.0, :min 0.0, :max 899.0, :p25 237.625, :p50 425.125, :p75 611.625, :p90 724.125, :p95 761.625, :p99 791.625,
        :mean 424.625, :var 52499.916666666664, :mad 187.5, :var-sum 6.29999E7, :mad-sum 225000.0, :meta {:floats? true}}))

   (is (nil? (ss-merging-error 10 100 10)))

   (let [ssb (stats/summary-stats-buffered {:buffer-size 10})]
     (dotimes [n 1e5] (ssb n))
     [(is (enc/submap? @@ssb {:n 100000 :min 0 :max 99999}))
      (is (= (str ssb) "SummaryStatsBuffered[n=0, pending=0, merged=9091]"))])])

;;;;

#?(:cljs (test/run-tests))

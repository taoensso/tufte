(ns taoensso.tufte-tests
  (:require
   [clojure.test    :as test  :refer [deftest testing is]]
   [clojure.string  :as str]
   [taoensso.encore :as enc]
   [taoensso.tufte  :as tufte :refer [profiled profile p]]

   #?(:clj  [taoensso.tufte.impl  :as impl]
      :cljs [taoensso.tufte.impl  :as impl :refer [PStats PData TimeSpan]]))

  #?(:clj  (:import [taoensso.tufte.impl PStats PData TimeSpan]))
  #?(:cljs (:require-macros
            [taoensso.tufte-tests
             :refer [looped nested-profiled]])))

(comment
  (remove-ns      'taoensso.tufte-tests)
  (test/run-tests 'taoensso.tufte-tests))

;;;;

#?(:clj
   (defmacro looped
     "Like `dotimes` but returns final body result."
     [n & body]
     `(let [n# (long ~n)]
        (when (> n# 0)
          (loop [i# 1]
            (let [result# (do ~@body)]
              (if (< i# n#)
                (recur (inc i#))
                result#)))))))

(comment (looped 10 (println "x") "x"))

(defn ps? [x] (instance? PStats x))

;;;;

(deftest profiled-basics
  (testing "Profiled/basics"
    [(let [[r ps] (profiled {})]                     [(is (= r  nil))  (is (ps? ps)) (is (map? (:clock @ps))) (is (nil? (:stats @ps)))])
     (let [[r ps] (profiled {:dynamic? true})]       [(is (= r  nil))  (is (ps? ps)) (is (map? (:clock @ps))) (is (nil? (:stats @ps)))])
     (let [[r ps] (profiled {}               "foo")] [(is (= r "foo")) (is (ps? ps)) (is (map? (:clock @ps))) (is (nil? (:stats @ps)))])
     (let [[r ps] (profiled {:dynamic? true} "foo")] [(is (= r "foo")) (is (ps? ps)) (is (map? (:clock @ps))) (is (nil? (:stats @ps)))])

     (let [[r ps] (profiled {:when false}                "foo")] [(is (= r "foo")) (is (nil? ps))])
     (let [[r ps] (profiled {:when false :dynamic? true} "foo")] [(is (= r "foo")) (is (nil? ps))])]))

(deftest capture-basics
  (testing "Capture/basics"

    [(is (= (p :foo "foo")          "foo"))
     (is (= (p :foo (p :bar "bar")) "bar"))

     (let [[r ps] (profiled {:when false}                (p :foo "foo"))] [(is (= r "foo")) (is (nil? ps))])
     (let [[r ps] (profiled {:when false :dynamic? true} (p :foo "foo"))] [(is (= r "foo")) (is (nil? ps))])

     (let [[r ps] (profiled {}               (p :foo "foo"))] [(is (= r "foo")) (is (ps? ps)) (is (= (get-in @ps [:stats :foo :n]) 1))])
     (let [[r ps] (profiled {:dynamic? true} (p :foo "foo"))] [(is (= r "foo")) (is (ps? ps)) (is (= (get-in @ps [:stats :foo :n]) 1))])

     (let [[r ps]
           (profiled {}
             (looped 100 (p :foo "foo"))
             (looped 50  (p :bar "bar"))
             (looped 10  (p :foo "foo")))]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (enc/submap? @ps {:stats {:foo {:n 110} :bar {:n 50}}}))])

     (let [[r ps]
           (profiled {:dynamic? true}
             (looped 100 (p :foo "foo"))
             (looped  50 (p :bar "bar"))
             (looped  10 (p :foo "foo")))]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (enc/submap? @ps {:stats {:foo {:n 110} :bar {:n 50}}}))])

     (let [[r ps] (profiled {} (p :foo) (p ::foo) (p 'foo) (p `foo))]
       [(is (enc/submap? @ps {:stats {:foo {} ::foo {} 'foo {} `foo {}}})
          "Support un/qualified keyword and symbol pids")])]))

(deftest capture-nested
  (testing "Capture/nested"

    [(let [[r ps] (profiled {:when false}                (p :foo (p :bar (p :baz "baz"))))] [(is (= r "baz") (is (nil? ps)))])
     (let [[r ps] (profiled {:when false :dynamic? true} (p :foo (p :bar (p :baz "baz"))))] [(is (= r "baz") (is (nil? ps)))])

     (let [[r ps]
           (profiled {}
             (looped 100 (p :foo (p :bar "bar")))
             (looped  10 (p :bar (p :bar "bar")))
             (looped   5 (p :foo (p :bar (p :foo "foo")))))]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (enc/submap? @ps {:stats {:foo {:n 110} :bar {:n 125}}}))])

     (let [[r ps]
           (profiled {:dynamic? true}
             (looped 100 (p :foo (p :bar "bar")))
             (looped  10 (p :bar (p :bar "bar")))
             (looped   5 (p :foo (p :bar "bar" (p :foo "foo")))))]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (enc/submap? @ps {:stats {:foo {:n 110} :bar {:n 125}}}))])]))

#?(:clj
   (deftest capture-threaded
     (testing "Capture/threaded"

       [(let [[r ps]
              (profiled {}
                (future (p :foo))
                (future (p :bar))
                (Thread/sleep 100)
                (p :foo "foo"))]

          [(is (= r "foo"))
           (is (ps? ps))
           (is (enc/submap? @ps {:stats {:foo {:n 1} :bar :submap/nx}}))])

        (let [[r ps]
              (profiled {:dynamic? true}
                (future (p :foo))
                (future (p :bar))
                (Thread/sleep 100)
                (p :foo "foo"))]

          [(is (= r "foo"))
           (is (ps? ps))
           (is (enc/submap? @ps {:stats {:foo {:n 2} :bar {:n 1}}}))])])))

(deftest merging-basics
  [(testing "Merging/basics"

     [;; Note mixed dynamic/non-dynamic
      (let [[_ ps0] (profiled {:dynamic? true} (looped 100 (p :foo) (p :bar)))
            [_ ps1] (profiled {}               (looped  20 (p :foo)))
            [_ ps2] (profiled {:dynamic? true} (looped  30 (p :baz)))
            ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]
        [(is (ps? ps3))
         (is (enc/submap? @ps3 {:stats {:foo {:n 120} :bar {:n 100} :baz {:n 30}}}))])

      ;; Invert dynamic/non-dynamic
      (let [[_ ps0] (profiled {}               (looped 100 (p :foo) (p :bar)))
            [_ ps1] (profiled {:dynamic? true} (looped  20 (p :foo)))
            [_ ps2] (profiled {}               (looped  30 (p :baz)))
            ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]
        [(is (ps? ps3))
         (is (enc/submap? @ps3 {:stats {:foo {:n 120} :bar {:n 100} :baz {:n 30}}}))])])

   #?(:clj
      (testing "ps1 starting before ps0 should sum clock total correctly, Ref. #48"
        (let [f2 (future                    (profiled {} (p :p1 (Thread/sleep 1500)))) ; Starts first, ends after
              f1 (future (Thread/sleep 800) (profiled {} (p :p2 (Thread/sleep  500))))
              m  @(tufte/merge-pstats (second @f1) (second @f2))]
          (is (>= (-> (long (get-in m [:clock :total])) (/ 1e8) (Math/round) (* 100)) 1400)))))])

(deftest compaction-capture
  (testing "Compaction/capture"

    [(let [[r ps]
           (profiled {:nmax 10}
             (looped 100 (p :foo))
             (looped  50 (p :foo (p :bar (p :baz))))
             (looped   2 (p :qux "qux")))]

       [(is (= r "qux"))
        (is (ps? ps))
        (is (enc/submap? @ps
              {:stats
               {:foo              {:n 150}
                :bar              {:n  50}
                :baz              {:n  50}
                :tufte/compaction {:n (/ 250 10)}}}))

        (is (= (count (.-acc ^PStats ps)) 2))])

     (let [[r ps]
           (profiled {:nmax 10 :dynamic? true}
             (looped 100 (p :foo))
             (looped  50 (p :foo (p :bar (p :baz))))
             (looped   2 (p :qux "qux")))]

       [(is (= r "qux"))
        (is (ps? ps))

        (is (enc/submap? @ps
              {:stats
               {:foo              {:n 150}
                :bar              {:n  50}
                :baz              {:n  50}
                :tufte/compaction {:n (/ 250 10)}}}))

        (is (= (count @(.-acc ^PStats ps)) 2))])]))

(deftest compaction-merge
  (testing "Compaction/merge"

    [;; Note mixed dynamic/non-dynamic
     (let [[_ ps0] (profiled {:dynamic? true :nmax 10} (looped 100 (p :foo)) (looped 50 (p :foo (p :bar))))
           [_ ps1] (profiled {}                        (looped  20 (p :foo)))
           [_ ps2] (profiled {:dynamic? true :nmax 5}  (looped  10 (p :bar)))
           ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]

       [(is (= (get-in @ps0 [:stats :tufte/compaction :n]) 19))
        (is (= (get-in @ps1 [:stats :tufte/compaction :n]) nil))
        (is (= (get-in @ps2 [:stats :tufte/compaction :n]) 1))

        (is (= (enc/submap? @ps3
                 {:stats
                  {:foo              {:n 170}
                   :bar              {:n  60}
                   :tufte/compaction {:n  20}}}))
          "Merging does uncounted compaction")])

     ;; Invert dynamic/non-dynamic
     (let [[_ ps0] (profiled {:nmax 10}       (looped 100 (p :foo)) (looped 50 (p :foo (p :bar))))
           [_ ps1] (profiled {:dynamic? true} (looped  20 (p :foo)))
           [_ ps2] (profiled {:nmax 5}        (looped  10 (p :bar)))
           ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]

       [(is (= (get-in @ps0 [:stats :tufte/compaction :n]) 19))
        (is (= (get-in @ps1 [:stats :tufte/compaction :n]) nil))
        (is (= (get-in @ps2 [:stats :tufte/compaction :n]) 1))

        (is (enc/submap? @ps3
              {:stats
               {:foo              {:n 170}
                :bar              {:n  60}
                :tufte/compaction {:n  20}}})
          "Merging does uncounted compaction")])

     ;; [#54] merging PStats with times still (only) in accumulator
     (let [[_ ^PStats ps0] (profiled {:nmax 10} (p :foo))
           ps (enc/reduce-n (fn [ps _] (tufte/merge-pstats ps ps0)) nil 1000)]

       [(is (<= (count (:foo (.-id-times  ps0))) 10))
        (is (<= (count (:foo (.-id-sstats ps0))) 10))])]))

(defn- pstats-tspan [t0 t1]
  (let [pd (PData. false 8e5 t0 nil nil nil false)
        tspans [(TimeSpan. t0 t1)]]
    (PStats. 8e5 t0 t1 nil nil nil tspans
      (delay (#'impl/deref-pstats 8e5 t0 t1 nil nil nil tspans)))))

(deftest merge-time-spans
  [(testing "Merge discrete time-spans"
     [(is (= 13 (get-in @(tufte/merge-pstats (pstats-tspan 0 6) (pstats-tspan 10 17)) [:clock :total])))
      (is (=  5 (get-in @(tufte/merge-pstats (pstats-tspan 1 3) (pstats-tspan  3  6)) [:clock :total])))])

  (testing "Merge overlapping time-spans"
    [(is (=  9 (get-in @(tufte/merge-pstats (pstats-tspan 1 10) (pstats-tspan 3  6)) [:clock :total])))
     (is (= 11 (get-in @(tufte/merge-pstats (pstats-tspan 0 10) (pstats-tspan 7 11)) [:clock :total])))
     (is (= 16
           (get-in
             @(reduce tufte/merge-pstats
                [(pstats-tspan 10 14)
                 (pstats-tspan  4 18)
                 (pstats-tspan 19 20)
                 (pstats-tspan 19 20)
                 (pstats-tspan 13 20)])
             [:clock :total])))])

   #?(:clj
      (testing "Accurate clock time even when merging non-increasing t0s" ; #52
        (let [sacc (tufte/stats-accumulator)
              _    (do                        (sacc :g1 (second (profiled {:id :foo} (p :p1))))) ; 1st-t0
              f1   (future (Thread/sleep 800) (sacc :g1 (second (profiled {:id :foo} (p :p1))))) ; 3rd-t0
              f2   (future                    (sacc :g1 (second (profiled {:id :foo} (p :p1 (Thread/sleep 1000)))))) ; 2nd-t0
              ]

          [@f1
           @f2
           (is (>= (/ (double (get-in @(:g1 @sacc) [:clock :total])) 1e6) 1000))])))])

(defn add-test-handler! []
  #?(:clj
     (let [p_ (promise)]
       (tufte/add-handler! :testing (fn [x] (p_ x)) {})
       (fn [] (deref p_ 1000 nil)))

     :cljs
     (let [a_ (atom ::nil)]
       (tufte/add-handler! :testing (fn [x] (compare-and-set! a_ ::nil x)) {})
       (fn [] (deref a_)))))

(deftest profile-basics
  (testing "Profile/basics"

    [(let [th (add-test-handler!)
           r  (profile {} "foo")
           m  (th)
           ps (:pstats m)]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (enc/submap? @ps {:clock :submap/ex :stats nil}))])

     (let [th (add-test-handler!)
           r  (profile {:dynamic? true} "foo")
           m  (th)
           ps (:pstats m)]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (enc/submap? @ps {:clock :submap/ex :stats nil}))])

     #?(:clj
        (let [th (add-test-handler!)
              r  (profile {} (future (p :foo)) (Thread/sleep 100) (p :bar "bar"))
              m  (th)
              ps (:pstats m)]

          [(is (= r "bar"))
           (is (ps? ps))
           (is (enc/submap? @ps
                 {:clock       :submap/ex
                  :stats {:foo :submap/nx
                          :bar {:n 1}}}))
           (is (string? @(:pstats-str_ m)))]))

     #?(:clj
        (let [th (add-test-handler!)
              r  (profile {:dynamic? true} (future (p :foo)) (Thread/sleep 100) (p :bar "bar"))
              m  (th)
              ps (:pstats m)]

          [(is (= r "bar"))
           (is (ps? ps))
           (is (enc/submap? @ps
                 {:clock :submap/ex
                  :stats {:foo {:n 1}
                          :bar {:n 1}}}))
           (is (string? @(:pstats-str_ m)))]))

     (tufte/remove-handler! :testing)]))

(let [get-ns (fn [ps] (enc/map-vals #(get % :n) (:stats @ps)))]
  (defn- nested-profiled-output [[r ps]] [r (get-ns ps)]))

#?(:clj
   (defmacro nested-profiled [outer-dynamic? inner-dynamic?]
     `(let [inner_# (atom nil)
            outer#
            (profiled {:dynamic? ~outer-dynamic?}
              (p :foo)
              (p :bar
                (reset! inner_#
                  (profiled {:dynamic? ~inner-dynamic?}
                    (p :foo)
                    (p :baz)
                    "inner")))

              (p :qux) ; Captured *after* inner pdata released (needs stack)
              "outer")]

        [(nested-profiled-output   outer#)
         (nested-profiled-output @inner_#)])))

(comment (nested-profiled true false))

(def ^:private nested-reference [["outer" {:qux 1, :bar 1, :foo 1}] ["inner" {:baz 1, :foo 1}]])

(deftest profiled-nesting
  [(testing "Profiled/nesting (single-threaded)"
     [(is (= (nested-profiled false false) nested-reference) "(local   (local   ...))")
      (is (= (nested-profiled true  true)  nested-reference) "(dynamic (dynamic ...))")
      (is (= (nested-profiled false true)  nested-reference) "(local   (dynamic ...))")
      (is (= (nested-profiled true  false) nested-reference) "(dynamic (local   ...))")])

   #?(:clj
      (testing "Profile/nesting (multi-threaded)"
        [(is (every? #(= nested-reference @%) (doall (repeatedly 100 (fn [] (future (Thread/sleep 10) (nested-profiled false false)))))) "(local   (local   ...))")
         (is (every? #(= nested-reference @%) (doall (repeatedly 100 (fn [] (future (Thread/sleep 10) (nested-profiled true  true))))))  "(dynamic (dynamic ...))")
         (is (every? #(= nested-reference @%) (doall (repeatedly 100 (fn [] (future (Thread/sleep 10) (nested-profiled false true))))))  "(local   (dynamic ...))")
         (is (every? #(= nested-reference @%) (doall (repeatedly 100 (fn [] (future (Thread/sleep 10) (nested-profiled true  false)))))) "(dynamic (local   ...))")]))])

(deftest advanced
  (testing "Advanced"

    [;; Capture in `profiled`
     (let [[_ ps] (profiled {} (tufte/capture-time! :foo 100))]
       (is (enc/submap? @ps {:stats {:foo {:n 1}}})))

     ;; Capture to local pdata
     (let [n  (long 2e6)
           pd (tufte/new-pdata)
           _  (looped n (tufte/capture-time! pd :foo 100))]
       (is (enc/submap? @@pd {:stats {:foo {:n n}}})))

     #?(:clj
        [;; Capture to dynamic pdata from separate threads using a super low
         ;; nmax to aggrevate possible contention. This test will show if any
         ;; times were lost during compaction.
         (let [n  (long 500)
               pd (tufte/new-pdata {:dynamic? true :nmax 10}) ; Low nmax
               f1 (future (looped n (tufte/capture-time! pd :foo 100)))
               f2 (future (looped n (tufte/capture-time! pd :foo 100)))
               f3 (future (looped n (tufte/capture-time! pd :foo 100)))
               f4 (future (looped n (tufte/capture-time! pd :foo 100)))]

           [@f1 @f2 @f3 @f4] ; Block for futures to complete
           (is (= (get-in @@pd [:stats :foo :n]) (* 4 n))))

         ;; local `p`s against local pdata
         (let [pd (tufte/new-pdata         {:dynamic? false})
               _  (tufte/with-profiling pd {:dynamic? false}
                    (p :foo (Thread/sleep 100))
                    (p :bar (Thread/sleep 200))
                    (tufte/capture-time! :baz 100))]

           [(is (enc/submap? @@pd
                  {:stats
                   {:foo {:n 1}
                    :bar {:n 1}
                    :baz {:n 1}}}))])

         ;; local `p`s against dynamic pdata
         (let [pd (tufte/new-pdata         {:dynamic? true})
               _  (tufte/with-profiling pd {:dynamic? false}
                    (p :foo (Thread/sleep 100))
                    (p :bar (Thread/sleep 200))
                    (tufte/capture-time! :baz 100))]

           [(is (enc/submap? @@pd
                  {:stats
                   {:foo {:n 1}
                    :bar {:n 1}
                    :baz {:n 1}}}))])

         ;; dynamic `p`s against dynamic pdata
         (let [pd (tufte/new-pdata         {:dynamic? true})
               _  (tufte/with-profiling pd {:dynamic? true}
                    (future (p :foo (Thread/sleep 100)))
                    (do     (p :bar (Thread/sleep 200)))
                    (tufte/capture-time! :baz 100))]

           (Thread/sleep 100)
           [(is (enc/submap? @@pd
                  {:stats
                   {:foo {:n 1}
                    :bar {:n 1}
                    :baz {:n 1}}}))])])]))

(deftest format-pstats
  [(testing "Basic format-pstats"
     (let [data {:clock {:total 15}
                 :stats {:foo {:n 10000
                               :min 1
                               :p50 2
                               :p90 3
                               :p95 4
                               :p99 5
                               :max 6
                               :mean 7
                               :mad 5.062294599999648
                               :sum 15}}}]
       [(is
          (= ["pId           nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total"
              ":foo          10,000        1ns        2ns        3ns        4ns        5ns        6ns        7ns  ±72%       15ns   100%"
              "Accounted                                                                                                     15ns   100%"
              "Clock                                                                                                         15ns   100%"]
            (->>
              (tufte/format-pstats data)
              (str/split-lines)
              (remove empty?))))
        (is
          (= ["pId           nCalls       Mean      Clock  Total"
              ":foo          10,000        7ns       15ns   100%"
              "Accounted                             15ns   100%"
              "Clock                                 15ns   100%"]
            (->>
              (tufte/format-pstats data {:columns [:n :mean :clock :sum]})
              (str/split-lines)
              (remove empty?))))]))

   (testing "format-pstats with namespaced symbols"
     (let [data {:clock {:total 15}
                 :stats {:foo/bar {:n 10000
                                   :min 1
                                   :p50 2
                                   :p90 3
                                   :p95 4
                                   :p99 5
                                   :max 6
                                   :mean 7
                                   :mad 5.062294599999648
                                   :sum 15}}}]
       (is
         (= ["pId           nCalls       Mean      Clock  Total"
             ":foo/bar      10,000        7ns       15ns   100%"
             "Accounted                             15ns   100%"
             "Clock                                 15ns   100%"]
           (->>
             (tufte/format-pstats data {:columns [:n :mean :clock :sum]})
             (str/split-lines)
             (remove empty?))))))

   (testing "Format seconds"
     (let [data {:clock {:total 2e9}
                 :stats {:foo {:n 1 :mean 2e9 :sum 2e9}
                         :bar {:n 1 :mean 15  :sum 15}}}]
       (is
         (= ["pId           nCalls       Mean"
             ":foo               1      2.00s"
             ":bar               1       15ns"]
           (->>
             (tufte/format-pstats data {:columns [:n :mean]})
             (str/split-lines)
             (remove empty?)
             (take 3))))))

   (testing "Format seconds only"
     (let [data {:clock {:total 2e9}
                 :stats {:foo {:n 1 :mean 2e9 :sum 2e9}
                         :bar {:n 1 :mean 1e9 :sum 1e9}}}]
       (is
         (= ["pId           nCalls       Mean"
             ":foo               1      2.00s"
             ":bar               1      1.00s"]
           (->>
             (tufte/format-pstats data {:columns [:n :mean]})
             (str/split-lines)
             (remove empty?)
             (take 3))))))

   (testing "Format id fn"
     (let [data {:clock {:total 2e9}
                 :stats {:example.hello/foo {:n 1 :mean 2e9 :sum 2e9}}}]
       (is
         (= ["pId           nCalls       Mean"
             "foo                1      2.00s"]
           (->>
             (tufte/format-pstats data {:columns [:n :mean]
                                        :format-id-fn name})
             (str/split-lines)
             (remove empty?)
             (take 2))))))])

;;;; Util macros

(do
  (tufte/defnp                      fn1  [x] x) ; Line 549
  (tufte/defnp                      fn2  [x] x)
  (tufte/defnp ^{:tufte/id :__fn3}  fn3  [x] x)
  (tufte/defnp ^{:tufte/id "__fn4"} fn4 ([x] x) ([x y] [x y]))
  (tufte/defnp-                     fn5  [x] x)

  (defn run-test-fns []
    (let [fn6 (tufte/fnp                     fn6 [x] x)
          fn7 (tufte/fnp ^{:tufte/id :__fn7} fn7 [x] x)]
      [(fn1 "fn1")
       (fn2 "fn2")
       (fn3 "fn3")
       (fn4 "fn4_1")
       (fn4 "fn4_2x" "fn4_2y")
       (fn5 "fn5")
       (fn6 "fn6")
       (fn7 "fn7")
       (fn1 (fn2 "fn1+2"))])))

(deftest util-macros
  [(let [[r ps] (profiled {} (run-test-fns))]
     [(is (= r ["fn1" "fn2" "fn3" "fn4_1" ["fn4_2x" "fn4_2y"] "fn5" "fn6" "fn7" "fn1+2"]))
      (is (enc/submap? @ps
            {:stats {::defn_fn1 {:n 2},
                     ::defn_fn2 {:n 2},
                     :__fn3     {:n 1},
                     :__fn4     {:n 2}
                     :__fn4_1   {:n 1}
                     :__fn4_2   {:n 1}
                     ::defn_fn5 {:n 1}
                     ::fn_fn6   {:n 1}
                     :__fn7     {:n 1}}}))])])

;;;; Location info

(deftest location-info
  ;; Note that these tests are sensitive to line numbers and will
  ;; need to be updated when line numbers change.
  [(let [[r ps]
         (profiled {}
           (p :foo) (p :bar) ; Line 589
           (p :baz
             (p :qux)))]

     [(is (enc/submap? @ps
            (let [nref 589]
              {:stats {:foo {:loc {:line nref}}
                       :bar {:loc {:line nref}}
                       :baz {:loc {:line (+ nref 1)}}
                       :qux {:loc {:line (+ nref 2)}}}})))])

   (let [[r ps]
         (profiled {}
           (p :foo) ; Line 602
           (p :foo)
           (p :foo)
           (p :foo))

         loc (get-in @ps [:stats :foo :loc])]

     [(is (set? loc) "id with >1 locations")
      (is (= (into #{} (map :line) loc) (let [nref 602] #{nref (+ nref 1) (+ nref 2) (+ nref 3)})))
      (is (enc/submap? @ps {:stats {:foo {:n 4}}}) "id's stats include all locations")])

   (let [[r ps] (profiled {} (run-test-fns))]
     [(is
        (enc/submap? @ps
          (let [nref 549]
           {:stats {::defn_fn1 {:loc {:line    nref}}
                    ::defn_fn2 {:loc {:line (+ nref 1)}}
                    :__fn3     {:loc {:line (+ nref 2)}}
                    :__fn4     {:loc {:line (+ nref 3)}}
                    :__fn4_1   {:loc {:line (+ nref 3)}}
                    :__fn4_2   {:loc {:line (+ nref 3)}}
                    ::defn_fn5 {:loc {:line (+ nref 4)}}
                    ::fn_fn6   {:loc {:line (+ nref 7)}}
                    :__fn7     {:loc {:line (+ nref 8)}}}})))])])

(comment (let [f1 (tufte/fnp foo [x] x #_(p :x x))]))

;;;;

#?(:cljs
   (defmethod test/report [:cljs.test/default :end-run-tests] [m]
     (when-not (test/successful? m)
       ;; Trigger non-zero `lein test-cljs` exit code for CI
       (throw (ex-info "ClojureScript tests failed" {})))))

#?(:cljs (test/run-tests))

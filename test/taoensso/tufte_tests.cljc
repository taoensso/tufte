(ns taoensso.tufte-tests
  (:require
   [clojure.test    :as test  :refer [deftest testing is]]
   [clojure.string  :as str]
   [taoensso.truss  :as truss]
   [taoensso.encore :as enc]
   [taoensso.tufte  :as tufte :refer [profiled profile p]]
   [taoensso.tufte.telemere :as tufte-telemere]
   [taoensso.tufte.timbre   :as tufte-timbre]
   [taoensso.tufte.trove    :as tufte-trove]

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
        (is (truss/submap? @ps {:stats {:foo {:n 110} :bar {:n 50}}}))])

     (let [[r ps]
           (profiled {:dynamic? true}
             (looped 100 (p :foo "foo"))
             (looped  50 (p :bar "bar"))
             (looped  10 (p :foo "foo")))]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (truss/submap? @ps {:stats {:foo {:n 110} :bar {:n 50}}}))])

     (let [[r ps] (profiled {} (p :foo nil) (p ::foo nil) (p 'foo nil) (p `foo nil))]
       [(is (truss/submap? @ps {:stats {:foo {} ::foo {} 'foo {} `foo {}}})
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
        (is (truss/submap? @ps {:stats {:foo {:n 110} :bar {:n 125}}}))])

     (let [[r ps]
           (profiled {:dynamic? true}
             (looped 100 (p :foo (p :bar "bar")))
             (looped  10 (p :bar (p :bar "bar")))
             (looped   5 (p :foo (p :bar "bar" (p :foo "foo")))))]

       [(is (= r "foo"))
        (is (ps? ps))
        (is (truss/submap? @ps {:stats {:foo {:n 110} :bar {:n 125}}}))])]))

#?(:clj
   (deftest capture-threaded
     (testing "Capture/threaded"
       [(let [[r ps]
              (profiled {}
                (future (p :foo nil))
                (future (p :bar nil))
                (Thread/sleep 100)
                (p :foo "foo"))]

          [(is (= r "foo"))
           (is (ps? ps))
           (is (truss/submap? @ps {:stats {:foo {:n 1} :bar :submap/nx}}))])

        (let [[r ps]
              (profiled {:dynamic? true}
                (future (p :foo nil))
                (future (p :bar nil))
                (Thread/sleep 100)
                (p :foo "foo"))]

          [(is (= r "foo"))
           (is (ps? ps))
           (is (truss/submap? @ps {:stats {:foo {:n 2} :bar {:n 1}}}))])])))

(deftest merging-basics
  [(testing "Merging/basics"
     [;; Note mixed dynamic/non-dynamic
      (let [[_ ps0] (profiled {:dynamic? true} (looped 100 (p :foo nil) (p :bar nil)))
            [_ ps1] (profiled {}               (looped  20 (p :foo nil)))
            [_ ps2] (profiled {:dynamic? true} (looped  30 (p :baz nil)))
            ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]
        [(is (ps? ps3))
         (is (truss/submap? @ps3 {:stats {:foo {:n 120} :bar {:n 100} :baz {:n 30}}}))])

      ;; Invert dynamic/non-dynamic
      (let [[_ ps0] (profiled {}               (looped 100 (p :foo nil) (p :bar nil)))
            [_ ps1] (profiled {:dynamic? true} (looped  20 (p :foo nil)))
            [_ ps2] (profiled {}               (looped  30 (p :baz nil)))
            ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]
        [(is (ps? ps3))
         (is (truss/submap? @ps3 {:stats {:foo {:n 120} :bar {:n 100} :baz {:n 30}}}))])])

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
             (looped 100 (p :foo nil))
             (looped  50 (p :foo (p :bar (p :baz nil))))
             (looped   2 (p :qux "qux")))]

       [(is (= r "qux"))
        (is (ps? ps))
        (is (truss/submap? @ps
              {:stats
               {:foo              {:n 150}
                :bar              {:n  50}
                :baz              {:n  50}
                :tufte/compaction {:n (/ 250 10)}}}))

        (is (= (count (.-acc ^PStats ps)) 2))])

     (let [[r ps]
           (profiled {:nmax 10 :dynamic? true}
             (looped 100 (p :foo nil))
             (looped  50 (p :foo (p :bar (p :baz nil))))
             (looped   2 (p :qux "qux")))]

       [(is (= r "qux"))
        (is (ps? ps))

        (is (truss/submap? @ps
              {:stats
               {:foo              {:n 150}
                :bar              {:n  50}
                :baz              {:n  50}
                :tufte/compaction {:n (/ 250 10)}}}))

        (is (= (count @(.-acc ^PStats ps)) 2))])]))

(deftest compaction-merge
  (testing "Compaction/merge"
    [;; Note mixed dynamic/non-dynamic
     (let [[_ ps0] (profiled {:dynamic? true :nmax 10} (looped 100 (p :foo nil)) (looped 50 (p :foo (p :bar nil))))
           [_ ps1] (profiled {}                        (looped  20 (p :foo nil)))
           [_ ps2] (profiled {:dynamic? true :nmax 5}  (looped  10 (p :bar nil)))
           ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]

       [(is (= (get-in @ps0 [:stats :tufte/compaction :n]) 19))
        (is (= (get-in @ps1 [:stats :tufte/compaction :n]) nil))
        (is (= (get-in @ps2 [:stats :tufte/compaction :n]) 1))

        (is (= (truss/submap? @ps3
                 {:stats
                  {:foo              {:n 170}
                   :bar              {:n  60}
                   :tufte/compaction {:n  20}}}))
          "Merging does uncounted compaction")])

     ;; Invert dynamic/non-dynamic
     (let [[_ ps0] (profiled {:nmax 10}       (looped 100 (p :foo nil)) (looped 50 (p :foo (p :bar nil))))
           [_ ps1] (profiled {:dynamic? true} (looped  20 (p :foo nil)))
           [_ ps2] (profiled {:nmax 5}        (looped  10 (p :bar nil)))
           ps3 (reduce tufte/merge-pstats [nil ps0 nil nil ps1 ps2 nil])]

       [(is (= (get-in @ps0 [:stats :tufte/compaction :n]) 19))
        (is (= (get-in @ps1 [:stats :tufte/compaction :n]) nil))
        (is (= (get-in @ps2 [:stats :tufte/compaction :n]) 1))

        (is (truss/submap? @ps3
              {:stats
               {:foo              {:n 170}
                :bar              {:n  60}
                :tufte/compaction {:n  20}}})
          "Merging does uncounted compaction")])

     ;; [#54] merging PStats with times still (only) in accumulator
     (let [[_ ^PStats ps0] (profiled {:nmax 10} (p :foo nil))
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
              _    (do                        (sacc :g1 (second (profiled {:id :foo} (p :p1 nil))))) ; 1st-t0
              f1   (future (Thread/sleep 800) (sacc :g1 (second (profiled {:id :foo} (p :p1 nil))))) ; 3rd-t0
              f2   (future                    (sacc :g1 (second (profiled {:id :foo} (p :p1 (Thread/sleep 1000)))))) ; 2nd-t0
              ]

          [@f1
           @f2
           (is (>= (/ (double (get-in @(:g1 @sacc) [:clock :total])) 1e6) 1000))])))])

(defn add-test-handler! []
  (let [sig_ (volatile! nil)]
    (tufte/add-handler! :testing (fn [sig] (vreset! sig_ sig)) {:async nil})
    (fn [] @sig_)))

(deftest profile-basics
  (testing "Profile/basics"

    [(let [th     (add-test-handler!)
           res    (profile {} "foo")
           sig    (th)
           pstats (:pstats sig)]

       [(is (= res "foo"))
        (is (ps? pstats))
        (is (truss/submap? @pstats {:clock {:total enc/nat-int?} :stats nil}))])

     (let [th     (add-test-handler!)
           res    (profile {:dynamic? true} "foo")
           sig    (th)
           pstats (:pstats sig)]

       [(is (= res "foo"))
        (is (ps? pstats))
        (is (truss/submap? @pstats {:clock {:total enc/nat-int?} :stats nil}))])

     (let [th     (add-test-handler!)
           res    (profile {} (p :foo "foo") (p :bar "bar"))
           sig    (th)
           pstats (:pstats sig)]

          [(is (= res "bar"))
           (is (ps?           pstats))
           (is (truss/submap? @pstats
                 {:clock {:total enc/nat-int?}
                  :stats {:foo {:n 1}
                          :bar {:n 1}}}))
           (is (string? ((:format-pstats-fn sig) pstats)))])

     #?(:clj
        (let [th     (add-test-handler!)
              res    (profile {} (future (p :foo nil)) (Thread/sleep 100) (p :bar "bar"))
              sig    (th)
              pstats (:pstats sig)]

          [(is (= res "bar"))
           (is (ps?            pstats))
           (is (truss/submap? @pstats
                 {:clock {:total enc/nat-int?}
                  :stats {:foo :submap/nx
                          :bar {:n 1}}}))
           (is (string? ((:format-pstats-fn sig) pstats)))]))

     #?(:clj
        (let [th     (add-test-handler!)
              res    (profile {:dynamic? true} (future (p :foo nil)) (Thread/sleep 100) (p :bar "bar"))
              sig    (th)
              pstats (:pstats sig)]

          [(is (= res "bar"))
           (is (ps?            pstats))
           (is (truss/submap? @pstats
                 {:clock {:total          enc/nat-int?}
                  :stats {:foo {:n 1 :sum enc/nat-int?}
                          :bar {:n 1 :sum enc/nat-int?}}}))
           (is (string? ((:format-pstats-fn sig) pstats)))]))

     (do (tufte/remove-handler! :testing) :remove-handler)]))

(let [get-ns (fn [ps] (enc/map-vals #(get % :n) (:stats @ps)))]
  (defn- nested-profiled-output [[r ps]] [r (get-ns ps)]))

#?(:clj
   (defmacro nested-profiled [outer-dynamic? inner-dynamic?]
     `(let [inner_# (atom nil)
            outer#
            (profiled {:dynamic? ~outer-dynamic?}
              (p :foo nil)
              (p :bar
                (reset! inner_#
                  (profiled {:dynamic? ~inner-dynamic?}
                    (p :foo nil)
                    (p :baz nil)
                    "inner")))

              (p :qux nil) ; Captured *after* inner pdata released (needs stack)
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

(deftest printing
  [#?(:clj (is (impl/signal? (read-string (binding [*print-dup* true] (pr-str        (impl/map->Signal {})))))))
   #?(:clj (is (impl/signal? (read-string (binding [*print-dup* true] (pr-str (assoc (impl/map->Signal {}) :k :v)))))))
   (is (enc/str-starts-with?              (binding [*print-dup* true]    (str (assoc (impl/map->Signal {}) :k :v))) "taoensso.tufte.Signal{"))
   (is (enc/str-starts-with?                                          (pr-str (assoc (impl/map->Signal {}) :k :v)) "#taoensso.tufte.Signal{"))
   (is (enc/str-starts-with?                                             (str (assoc (impl/map->Signal {}) :k :v))  "taoensso.tufte.Signal{"))])

(deftest advanced
  (testing "Advanced"
    [(testing "Capture in `profiled`"
       (let [[_ ps] (profiled {} (tufte/capture-time! :foo 100))]
         (is (truss/submap? @ps {:stats {:foo {:n 1}}}))))

     (testing "Capture to local pdata"
       (let [n  (long 2e6)
             pd (tufte/new-pdata)
             _  (looped n (tufte/capture-time! pd :foo 100))]
         (is (truss/submap? @@pd {:stats {:foo {:n n}}}))))

     #?(:clj
        (testing "Capture to dynamic pdata from separate threads"
          ;; Use super low nmax to aggrevate possible contention.
          ;; This test will show if any times were lost during compaction.
          (let [n  (long 500)
                pd (tufte/new-pdata {:dynamic? true :nmax 100}) ; Low nmax
                f1 (future (looped n (tufte/capture-time! pd :foo 100)))
                f2 (future (looped n (tufte/capture-time! pd :foo 100)))
                f3 (future (looped n (tufte/capture-time! pd :foo 100)))
                f4 (future (looped n (tufte/capture-time! pd :foo 100)))]

            [@f1 @f2 @f3 @f4] ; Block for futures to complete
            [(is (= (get-in @@pd [:stats :foo :n]) (* 4 n)))])))

     #?(:clj
        (testing "Local `p`s against local pdata"
          (let [pd (tufte/new-pdata         {:dynamic? false})
                _  (tufte/with-profiling pd {:dynamic? false}
                     (p :foo (Thread/sleep 100))
                     (p :bar (Thread/sleep 200))
                     (tufte/capture-time! :baz 100))]

            [(is (truss/submap? @@pd
                   {:stats
                    {:foo {:n 1}
                     :bar {:n 1}
                     :baz {:n 1}}}))])))

     #?(:clj
        (testing "Local `p`s against dynamic pdata"
          (let [pd (tufte/new-pdata         {:dynamic? true})
                _  (tufte/with-profiling pd {:dynamic? false}
                     (p :foo (Thread/sleep 100))
                     (p :bar (Thread/sleep 200))
                     (tufte/capture-time! :baz 100))]

            [(is (truss/submap? @@pd
                   {:stats
                    {:foo {:n 1}
                     :bar {:n 1}
                     :baz {:n 1}}}))])))

     #?(:clj
        (testing "Dynamic `p`s against dynamic pdata"
          (let [pd (tufte/new-pdata         {:dynamic? true})
                _  (tufte/with-profiling pd {:dynamic? true}
                     (future (p :foo (Thread/sleep 100)))
                     (do     (p :bar (Thread/sleep 200)))
                     (tufte/capture-time! :baz 100))]

            (Thread/sleep 100)
            [(is (truss/submap? @@pd
                   {:stats
                    {:foo {:n 1}
                     :bar {:n 1}
                     :baz {:n 1}}}))])))]))

(deftest format-pstats
  [(testing "Basics"
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

   (testing "With namespaced symbols"
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
  (tufte/defnp                       fn1  [x] x) ; Line 563
  (tufte/defnp                       fn2  [x] x)
  (tufte/defnp ^{:tufte/id :my-fn3}  fn3  [x] x)
  (tufte/defnp ^{:tufte/id "my-fn4"} fn4 ([x] x) ([x y] [x y]))
  (tufte/defnp-                      fn5  [x] x)

  (defn run-test-fns []
    (let [fn6 (tufte/fnp                      fn6 [x] x)
          fn7 (tufte/fnp ^{:tufte/id :my-fn7} fn7 [x] x)]
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
      (is (truss/submap? @ps
            {:stats {::fn1     {:n 2},
                     ::fn2     {:n 2},
                     :my-fn3   {:n 1},
                     :my-fn4   {:n 2}
                     :my-fn4_1 {:n 1}
                     :my-fn4_2 {:n 1}
                     ::fn5     {:n 1}
                     ::fn6     {:n 1}
                     :my-fn7   {:n 1}}}))])])

;;;; Location info

(deftest location-info
  ;; Note that these tests are sensitive to line numbers and will
  ;; need to be updated when line numbers change.
  [(let [[r ps]
         (profiled {}
           (p :foo nil) (p :bar nil) ; Line 603
           (p :baz
             (p :qux nil)))]

     [(is (truss/submap? @ps
            (let [nref 603]
              {:stats {:foo {:loc {:line nref}}
                       :bar {:loc {:line nref}}
                       :baz {:loc {:line (+ nref 1)}}
                       :qux {:loc {:line (+ nref 2)}}}})))])

   (let [[r ps]
         (profiled {}
           (p :foo nil) ; Line 616
           (p :foo nil)
           (p :foo nil)
           (p :foo nil))

         loc (get-in @ps [:stats :foo :loc])]

     [(is (set? loc) "id with >1 locations")
      (is (= (into #{} (map :line) loc) (let [nref 616] #{nref (+ nref 1) (+ nref 2) (+ nref 3)})))
      (is (truss/submap? @ps {:stats {:foo {:n 4}}}) "id's stats include all locations")])

   (let [[r ps] (profiled {} (run-test-fns))]
     [(is
        (truss/submap? @ps
          (let [nref 563]
            {:stats {::fn1     {:loc {:line    nref}}
                     ::fn2     {:loc {:line (+ nref 1)}}
                     :my-fn3   {:loc {:line (+ nref 2)}}
                     :my-fn4   {:loc {:line (+ nref 3)}}
                     :my-fn4_1 {:loc {:line (+ nref 3)}}
                     :my-fn4_2 {:loc {:line (+ nref 3)}}
                     ::fn5     {:loc {:line (+ nref 4)}}
                     ::fn6     {:loc {:line (+ nref 7)}}
                     :my-fn7   {:loc {:line (+ nref 8)}}}})))])])

(comment (let [f1 (tufte/fnp foo [x] x #_(p :x x))]))

;;;; NB keep below any line-sensitive tests (e.g. `location-info`)

(deftest profile-sample-evaluation
  (testing "`profile` evaluates a dynamic sample rate once"
    (let [calls_  (atom 0)
          signal_ (atom nil)]
      (binding [impl/*sig-handlers* nil]
        (profile {:sample (do (swap! calls_ inc) 1.0)} nil))
      (is (zero? @calls_) "Does not evaluate without handlers")
      (tufte/add-handler! ::sample-test
        (fn
          ([])
          ([signal] (reset! signal_ signal)))
        {:async nil})
      (try
        (profile {:sample (do (swap! calls_ inc) 1.0)} nil)
        (is (= @calls_ 1))
        (is (= (:sample @signal_) 1.0))
        (finally
          (tufte/remove-handler! ::sample-test))))))

(deftest invalid-nmax
  (testing "`:nmax` must remain positive"
    (let [[_ ps] (profiled {} nil)]
      [(is (thrown? #?(:clj Exception :cljs js/Error) (tufte/new-pdata {:nmax 0})))
       (is (thrown? #?(:clj Exception :cljs js/Error) (profiled {:nmax -1} nil)))
       (is (thrown? #?(:clj Exception :cljs js/Error) (profiled {:dynamic? true :nmax 0.5} nil)))
       (is (thrown? #?(:clj Exception :cljs js/Error) (tufte/merge-pstats 0 ps ps)))])))

(deftest merge-negative-time-spans
  (testing "Merged clock time supports negative monotonic timestamps"
    [(is (= 12
           (get-in @(tufte/merge-pstats
                      (pstats-tspan -20 -15)
                      (pstats-tspan -10  -3))
             [:clock :total])))
     (is (= 17
           (get-in @(tufte/merge-pstats
                      (pstats-tspan -20 -5)
                      (pstats-tspan -10 -3))
             [:clock :total])))]))

(tufte/defnp ^{:tufte/id :leading-id} trailing-attrs-fn
  ([x] x)
  ([x y] [x y])
  {:private true, :test-meta :defnp, :tufte/id :trailing-id})

(tufte/defnp- trailing-attrs-private-fn
  ([x] x)
  {:test-meta :defnp-})

(deftest defnp-trailing-attrs
  (testing "`defnp` and `defnp-` preserve trailing attr maps"
    (let [[r ps]
          (profiled {}
            [(trailing-attrs-fn :x)
             (trailing-attrs-fn :x :y)
             (trailing-attrs-private-fn :z)])]
      [(is (= r [:x [:x :y] :z]))
       (is (truss/submap? @ps
             {:stats
              {:trailing-id   {:n 2}
               :trailing-id_1 {:n 1}
               :trailing-id_2 {:n 1}}}))
       (is (= (:test-meta (meta #'trailing-attrs-fn)) :defnp))
       (is (= (:test-meta (meta #'trailing-attrs-private-fn)) :defnp-))
       (is (true? (:private (meta #'trailing-attrs-fn))))
       (is (true? (:private (meta #'trailing-attrs-private-fn))))])))

(deftest handler-lifecycle
  (testing "Provided handlers support the stop arity"
    [(is (nil? ((tufte/handler:accumulating (tufte/stats-accumulator)))))
     (is (nil? ((tufte-telemere/handler:telemere))))
     (is (nil? ((tufte-timbre/handler:timbre))))
     (is (nil? ((tufte-trove/handler:trove))))]))

(deftest trove-handler
  (testing "Trove handler preserves profiling id"
    (let [call_  (atom nil)
          signal (impl/map->Signal
                   {:ns "example.ns", :coords [1 2], :id :profile-id, :level :info})]
      (with-redefs [taoensso.trove/*log-fn*
                    (fn [ns coords level id lazy_]
                      (reset! call_ {:ns ns, :coords coords, :level level, :id id}))]
        ((tufte-trove/handler:trove) signal))
      (is (= @call_
            {:ns "example.ns", :coords [1 2], :level :info, :id :profile-id})))))

#?(:clj
   (deftest profile-thread-info
     (testing "`profile` captures thread info before async dispatch"
       (let [signal_      (promise)
             thread-ids_ (atom [])
             caller-id   (.getId (Thread/currentThread))]
         (tufte/add-handler! ::thread-test
           (fn
             ([])
             ([signal] (deliver signal_ signal))))
         (try
           (profile {:thread (swap! thread-ids_ conj (.getId (Thread/currentThread)))} nil)
           (let [signal (deref signal_ 5000 ::timeout)]
             (is (not= signal ::timeout))
             (is (= (:thread signal) [caller-id]))
             (is (= @thread-ids_ [caller-id]) "Evaluates custom thread form once"))
           (finally
             (tufte/remove-handler! ::thread-test)))))))

#?(:clj
   (deftest legacy-handler
     (when-let [add-legacy! (ns-resolve 'taoensso.tufte 'add-legacy-handler!)]
       (testing "Legacy handler argument adaptation"
         (let [signal_ (atom nil)]
           (add-legacy! ::legacy-test #(reset! signal_ %))
           (try
             (profile {:id :profile-id :data :profile-data} (p :p-id nil))
             (finally
               (tufte/remove-handler! ::legacy-test)))
           (is (truss/submap? @signal_
                 {:ns-str "taoensso.tufte-tests"
                  :?id :profile-id
                  :?data :profile-data}))
           (is (= 1 (get-in @(:pstats @signal_) [:stats :p-id :n])))
           (is (delay? (:pstats-str_ @signal_)))))

       (testing "Legacy namespace filtering"
         (let [calls_ (atom 0)]
           (add-legacy! ::legacy-filter-test "no.such.namespace" #(swap! calls_ inc))
           (try
             (profile {} nil)
             (finally
               (tufte/remove-handler! ::legacy-filter-test)))
           (is (zero? @calls_))))

       (testing "Legacy console formatting options"
         (let [add-console! (ns-resolve 'taoensso.tufte 'add-basic-println-handler!)
               [_ ps]       (profiled {} (p :p-id nil))]
           (add-console!
             {:handler-id ::legacy-console-test
              :format-pstats-opts {:columns [:n]}})
           (try
             (let [handler-fn (get-in (tufte/get-handlers)
                                [::legacy-console-test :handler-fn])
                   output (with-out-str
                            (handler-fn
                              (impl/map->Signal
                                {:pstats ps, :format-pstats-fn tufte/format-pstats})))]
               (is (str/includes? output "nCalls"))
               (is (not (str/includes? output "Mean"))))
             (finally
               (tufte/remove-handler! ::legacy-console-test))))))))

(deftest accumulating-handler ; [#75]
  (testing "`handler:accumulating` receives one signal after each completed `profile`"
    (let [sacc    (tufte/stats-accumulator)
          during_ (volatile! nil)]
      ;; `{:async nil}` => synchronous dispatch, so reads here are deterministic
      (tufte/add-handler! ::acc-test (tufte/handler:accumulating sacc) {:async nil})
      (try
        (profile {:id :foo}
          (p :p1 nil)
          (vreset! during_ @sacc))

        (is (empty? @during_) "No partial signal while `profile` is running")
        (is (= 1 (get-in @(:foo @sacc) [:stats :p1 :n]))
          "Exactly one signal after the first `profile` returns")

        (profile {:id :foo} (p :p1 nil))
        (is (= 2 (get-in @(:foo @sacc) [:stats :p1 :n]))
          "Accumulates across completed `profile` calls")
        (is (ps? (:foo (sacc))))       ; `(sacc)` returns current + drains
        (is (nil? (not-empty @sacc)))  ; Now drained

        (finally
          (tufte/remove-handler! ::acc-test))))))

;;;;

#?(:cljs
   (defmethod test/report [:cljs.test/default :end-run-tests] [m]
     (when-not (test/successful? m)
       ;; Trigger non-zero `lein test-cljs` exit code for CI
       (throw (ex-info "ClojureScript tests failed" {})))))

#?(:cljs (test/run-tests))

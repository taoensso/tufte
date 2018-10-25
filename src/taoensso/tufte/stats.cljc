(ns taoensso.tufte.stats
  "Basic stats utils. Private, subject to change."
  (:require [taoensso.encore :as enc]
   #?(:cljs [goog.array])))

(comment
  (do
    (defn rand-vs [n & [max]] (take n (repeatedly (partial rand-int (or max Integer/MAX_VALUE)))))
    (def v1 (rand-vs 1e5))
    (def v1-sorted (sort-longs v1))))

#?(:clj (def ^:const longs-class (Class/forName "[J")))
#?(:clj (defn- longs? [x] (instance? longs-class x)))
(comment (longs? (long-array 10)))

(deftype SortedLongs [^longs a]
  #?@(:clj  [clojure.lang.Counted  (count [_] (alength a))]
      :cljs [            ICounted (-count [_] (alength a))]))

(defn sorted-longs? [x] (instance? SortedLongs x))
(defn sort-longs ^SortedLongs [longs]
  (if (sorted-longs? longs)
    longs
    #?(:cljs
       (let [a (if (array? longs) longs (to-array longs))]
         (goog.array/sort a)
         (SortedLongs. a))

       :clj
       (let [^longs a (if (longs? longs) longs (long-array longs))]
         (java.util.Arrays/sort a) ; O(n.log(n)) on JDK 7+
         (SortedLongs. a)))))

(comment
  (vec (.-a (sort-longs nil)))
  (vec (.-a (sort-longs [])))
  (vec (.-a (sort-longs (rand-vs 10)))))

(defn long-percentiles
  "Returns ?[min p50 p90 p95 p99 max]"
  [longs]
  (let [^longs a (.-a (sort-longs longs))
        max-idx (dec (alength a))]
    (if (< max-idx 0)
      nil
      [(aget a 0)
       (aget a (Math/round (* 0.50 max-idx)))
       (aget a (Math/round (* 0.90 max-idx)))
       (aget a (Math/round (* 0.95 max-idx)))
       (aget a (Math/round (* 0.99 max-idx)))
       (aget a                     max-idx)])))

(comment
  (long-percentiles nil)
  (long-percentiles [])
  (enc/qb 100
    (long-percentiles v1)
    (long-percentiles v1-sorted)) ; [1580.76 0.02]
  )

(deftype MinMax [^long vmin ^long vmax])
(defn min-max "Returns ?[<min> <max>]" [longs]
  (if (sorted-longs? longs)
    (let [^longs a (.-a ^SortedLongs longs)
          max-idx (dec (alength a))]
      (if (< max-idx 0)
        nil
        [(aget a 0) (aget a max-idx)]))

    (if (zero? (count longs))
      nil
      (let [[v1] longs
            ^MinMax min-max
            (reduce
              (fn [^MinMax acc ^long in]
                (let [vmin (.-vmin acc)
                      vmax (.-vmax acc)]
                  (if (> in vmax)
                    (MinMax. vmin in)
                    (if (< in vmin)
                      (MinMax. in vmin)
                      acc))))
              (MinMax. v1 v1)
              longs)]
        [(.-vmin min-max) (.-vmax min-max)]))))

(comment (enc/qb 1e6 (min-max [10 9 -3 12]))) ; 267.25

(comment
  (let [a (long-array v1)]
    (enc/qb 1e2
      (reduce (fn [^long acc ^long in] (unchecked-add acc in)) v1)
      (areduce a idx ret 0 (unchecked-add ret (aget a idx))))))

(defn stats
  "Given a collection of longs, returns map with keys:
  #{:n :min :max :sum :mean :mad-sum :mad :p50 :p90 :p95 :p99}, or nil if
  collection is empty."
  [longs]
  (when longs
    (let [sorted-longs (sort-longs longs)
          ^longs a (.-a sorted-longs)
          n (alength a)]
      (if (zero? n)
        nil
        (let [sum     (areduce a i acc 0 (+ acc (aget a i)))
              mean    (/ (double sum) (double n))
              mad-sum (areduce a i acc 0.0 (+ acc (Math/abs (- (double (aget a i)) mean))))
              mad     (/ (double mad-sum) (double n))

              [vmin p50 p90 p95 p99 vmax] (long-percentiles sorted-longs)]

          {:n n :min vmin :max vmax :sum sum :mean mean
           :mad-sum mad-sum :mad mad
           :p50 p50 :p90 p90 :p95 p95 :p99 p99})))))

(comment (enc/qb 100 (stats v1) (stats v1-sorted))) ; [1604.23 38.3]

(defn merge-stats
  "`(merge-stats (stats c0) (stats c1))` is a basic approximation of `(stats (into c0 c1)))`."
  [m0 m1]
  (if m0
    (if m1
      (let [_ (assert (get m0 :n))
            _ (assert (get m1 :n))

            {^long   n0       :n
             ^long   min0     :min
             ^long   max0     :max
             ^long   sum0     :sum
             ^double mad-sum0 :mad-sum
             ^long   p50-0    :p50
             ^long   p90-0    :p90
             ^long   p95-0    :p95
             ^long   p99-0    :p99} m0

            {^long   n1       :n
             ^long   min1     :min
             ^long   max1     :max
             ^long   sum1     :sum
             ^double mad-sum1 :mad-sum
             ^long   p50-1    :p50
             ^long   p90-1    :p90
             ^long   p95-1    :p95
             ^long   p99-1    :p99} m1

            _ (assert (pos? n0))
            _ (assert (pos? n1))

            n2       (+ n1 n0)
            n0-ratio (/ (double n0) (double n2))
            n1-ratio (/ (double n1) (double n2))

            sum2  (+ sum0 sum1)
            mean2 (/ (double sum2) (double n2))
            min2  (if (< min0 min1) min0 min1)
            max2  (if (> max0 max1) max0 max1)

            ;; Batched "online" MAD calculation here is better= the standard
            ;; Knuth/Welford method, Ref. http://goo.gl/QLSfOc,
            ;;                            http://goo.gl/mx5eSK.
            ;;
            ;; Note that there's empirically no advantage in using `mean2` here
            ;; asap, i.e. to reducing (- v1_i mean2).
            mad-sum2 (+ mad-sum0 ^double mad-sum1)

            ;;; These are pretty rough approximations. More sophisticated
            ;;; approaches not worth the extra cost/effort in our case.
            p50-2 (Math/round (+ (* n0-ratio (double p50-0)) (* n1-ratio (double p50-1))))
            p90-2 (Math/round (+ (* n0-ratio (double p90-0)) (* n1-ratio (double p90-1))))
            p95-2 (Math/round (+ (* n0-ratio (double p95-0)) (* n1-ratio (double p95-1))))
            p99-2 (Math/round (+ (* n0-ratio (double p99-0)) (* n1-ratio (double p99-1))))

            mad2 (/ (double mad-sum2) (double n2))]

        {:n n2 :min min2 :max max2 :sum sum2 :mean mean2
         :mad-sum mad-sum2 :mad mad2
         :p50 p50-2 :p90 p90-2 :p95 p95-2 :p99 p99-2})
      m0)
    m1))

(comment
  (def v2 [1 2 2 3 2 1])
  (def v3 [1 3 5 2 1 6])
  (def v4 (into v2 v3))

  (stats v2) {:min 1, :mean 1.8333333333333333, :mad-sum 3.333333333333333,  :p99 3, :n 6,  :p90 3, :max 3, :mad 0.5555555555555555, :p50 2, :sum 11, :p95 3}
  (stats v3) {:min 1, :mean 3.0,                :mad-sum 10.0,               :p99 6, :n 6,  :p90 6, :max 6, :mad 1.6666666666666667, :p50 3, :sum 18, :p95 6}
  (stats v4) {:min 1, :mean 2.4166666666666665, :mad-sum 14.666666666666666, :p99 6, :n 12, :p90 5, :max 6, :mad 1.222222222222222,  :p50 2, :sum 29, :p95 5}

  (merge-stats (stats v2) (stats v3))
  {:min 1, :mean 2.4166666666666665, :mad-sum 13.333333333333332, :p99 5, :n 12, :p90 5, :max 6, :mad 1.111111111111111, :p50 3, :sum 29, :p95 5}

  (stats (stats v2) v3)
  {:min 1, :mean 2.4166666666666665, :mad-sum 13.333333333333332, :p99 5, :n 12, :p90 5, :max 6, :mad 1.111111111111111, :p50 3, :sum 29, :p95 5}

  (merge-stats (stats v2) (stats v2))
  {:min 1, :mean 1.8333333333333333, :mad-sum 6.666666666666666, :p99 3, :n 12, :p90 3, :max 3, :mad 0.5555555555555555, :p50 2, :sum 22, :p95 3}

  (let [v1 (rand-vs 1e5 80)
        v2 (rand-vs 1e5 20)
        v3 (into v1 v2)]
    (mapv :mad
      [(stats v1)
       (stats v2)
       (stats v3)
       (merge-stats (stats v1) (stats v2))
       (stats (stats v1) v2)]))

  [19.943705799999858 5.015891904000014 18.906570458826117 12.479798851999936 12.479798851999936]
  [20.033054674800002 5.013648978000108 18.914174079741983 12.523351826400054 12.523351826400054])

;;;; Formatting

(defn- perc [n d] (str (Math/round (* (/ (double n) (double d)) 100.0)) "%"))
(comment [(perc 1 1) (perc 1 100) (perc 12 44)])

(let [round2 #?(:cljs enc/round2 :clj (fn [n] (format "%.2f" n)))]
  (defn- fmt [nanosecs]
    (let [ns (double nanosecs)]
      (cond
        (>= ns 6e10) (str (round2 (/ ns 6e10)) "m")
        (>= ns 1e9)  (str (round2 (/ ns 1e9))  "s")
        (>= ns 1e6)  (str (round2 (/ ns 1e6))  "ms")
        (>= ns 1e3)  (str (round2 (/ ns 1e3))  "μs")
        :else        (str (round2    ns)       "ns")))))

(comment
  (format "%.2f" 40484.005)
  (fmt 2387387870))


(def all-columns [:n-calls :min :p50 :p90 :p95 :p99 :max :mean :mad :total :clock])

(defn format-stats
  "Returns a formatted table string for given `{<id> <stats>}` map.
  Assumes nanosecond clock, stats based on profiling id'd nanosecond times."
  ([clock-total id-stats] (format-stats clock-total id-stats (fn [id m] (get m :sum))))
  ([clock-total id-stats sort-fn] (format-stats clock-total id-stats sort-fn all-columns))
  ([clock-total id-stats sort-fn columns]
   (when id-stats
     (let [clock-total (long clock-total)
           ^long accounted-total
           (reduce-kv
             (fn [^long acc _id s]
               (+ acc (long (get s :sum))))
             0 id-stats)

           sorted-ids
           (sort-by
             (fn [id] (sort-fn id (get id-stats id)))
             enc/rcompare
             (keys id-stats))

           ^long max-id-width
           (reduce-kv
             (fn [^long acc k v]
               (let [c (count (str k))]
                 (if (> c acc) c acc)))
             9 ; (count "Accounted")
             id-stats)]

       #?(:cljs ; Simplified output w/o table
          (let [sb
                (reduce
                  (fn [acc id]
                    (let [s (get id-stats id)
                          sum (get s :sum)
                          mean (get s :mean)]
                      (enc/sb-append acc
                                     (str
                                       (select-keys {:id      id
                                                     :n-calls (get s :n)
                                                     :min     (fmt (get s :min))
                                                     :p50     (fmt (get s :p50))
                                                     :p90     (fmt (get s :p90))
                                                     :p95     (fmt (get s :p95))
                                                     :p99     (fmt (get s :p99))
                                                     :max     (fmt (get s :max))
                                                     :mean    (fmt mean)
                                                     :mad     (str "±" (perc (get s :mad) mean))
                                                     :total   (fmt sum)
                                                     :clock   (perc sum clock-total)} columns)
                                       "\n"))))
                  (enc/str-builder)
                  sorted-ids)]

            (enc/sb-append sb "\n")
            (enc/sb-append sb (str "Accounted: (" (perc accounted-total clock-total) ") " (fmt accounted-total) "\n"))
            (enc/sb-append sb (str "Clock: (100%) " (fmt clock-total) "\n"))
            (str sb))

          :clj
          (let [column->pattern {:id      {:n (str "%" max-id-width "s") :s (str "%" max-id-width "s") :heading "pId"}
                                 :n-calls {:n "%,10d" :s "%10s" :heading "nCalls"}
                                 :min     {:heading "Min"}
                                 :p50     {:heading "50% ≤"}
                                 :p90     {:heading "90% ≤"}
                                 :p95     {:heading "95% ≤"}
                                 :p99     {:heading "99% ≤"}
                                 :max     {:heading "Max"}
                                 :mean    {:heading "Mean"}
                                 :mad     {:n "%5s" :s "%5s" :heading "MAD"}
                                 :total   {:n "%11s" :s "%11s" :heading "Total"}
                                 :clock   {:n "%7s" :s "%7s" :heading "Clock"}}
                ^StringBuilder sb (enc/str-builder "")
                format-n-append (fn [column s] (enc/sb-append sb (format (get-in column->pattern [column :n] "%10s") s)))
                format-s-append (fn [column s] (enc/sb-append sb (format (get-in column->pattern [column :s] "%10s") s)))]

            ; Write headers
            (doseq [column (into [:id] columns)]
              (when-not (= :id column)
                (enc/sb-append sb " "))
              (format-s-append column (get-in column->pattern [column :heading])))
            (enc/sb-append sb "\n\n")

            ; Write numbers
            (doseq [id sorted-ids]
              (let [s (get id-stats id)
                    sum (get s :sum)
                    mean (get s :mean)]
                (format-n-append :id id)
                (doseq [column columns]
                  (enc/sb-append sb " ")
                  (case column :n-calls (format-n-append column (get s :n))
                               :mean  (format-n-append column (fmt mean))
                               :mad (format-n-append column (str "±" (perc (get s :mad) mean)))
                               :total (format-n-append column (fmt sum))
                               :clock (format-n-append column (perc sum clock-total))
                               (format-n-append column (fmt (get s column)))))
                (enc/sb-append sb "\n")))

            ; Write Accounted
            (enc/sb-append sb "\n")
            (format-s-append :id "Accounted")
            (doseq [column columns]
              (enc/sb-append sb " ")
              (case column :total (format-s-append column (fmt accounted-total))
                           :clock (format-s-append column (perc accounted-total clock-total))
                           (format-s-append column "")))

            ; Write Clock
            (enc/sb-append sb "\n")
            (format-s-append :id "Clock")
            (doseq [column columns]
              (enc/sb-append sb " ")
              (case column :total (format-s-append column (fmt clock-total))
                           :clock (format-s-append column "100%")
                           (format-s-append column "")))
            (enc/sb-append sb "\n")
            (str sb)))))))

(comment
  (println
    (format-stats (* 1e6 30)
      {:foo (stats (rand-vs 1e4 20))
       :bar (stats (rand-vs 1e2 50))
       :baz (stats (rand-vs 1e5 30))}) "\n"))

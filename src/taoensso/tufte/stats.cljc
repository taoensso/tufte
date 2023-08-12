(ns ^:no-doc taoensso.tufte.stats
  "Stats stuff.
  Private ns, implementation detail."
  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc  :refer [have have? have!]]
   #?(:cljs [goog.array]))

  #?(:clj (:import [java.util LinkedList])))

(comment
  (remove-ns 'taoensso.tufte.stats)
  (:public (enc/interns-overview)))

;;;; TODO
;; - Add SummaryStatsRolling (rolling by max len and/or date)?

;;;;

#?(:clj (let [c (Class/forName "[J")] (defn longs?   "Returns true iff given long array"   [x] (instance? c x))))
#?(:clj (let [c (Class/forName "[D")] (defn doubles? "Returns true iff given double array" [x] (instance? c x))))

;;;; Sorted nums

(deftype SortedDoubles [^doubles a last]
  #?@(:clj
      [Object               (toString [_] (str "SortedDoubles[len=" (alength a) "]"))
       clojure.lang.Counted (count    [_]                           (alength a))
       clojure.lang.Indexed
       (nth [_ idx] (aget a idx))
       (nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))

       clojure.lang.IReduceInit
       (reduce [_ f init]
         #_(areduce a i acc init (f acc (aget a i)))
         (reduce (fn [acc idx]   (f acc (aget a idx)))
           init (range (alength a))))]

      :cljs
      [Object   (toString [_] (str "SortedDoubles[len=" (alength a) "]"))
       ICounted (-count   [_]                           (alength a))
       IIndexed
       (-nth [_ idx] (aget a idx))
       (-nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))

       IReduce
       (-reduce [_ f init]
         #_(areduce a i acc init (f acc (aget a i)))
         (reduce (fn [acc i]     (f acc (aget a i)))
           init (range (alength a))))]))

(defn sorted-doubles? [x] (instance? SortedDoubles x))

(defn sorted-doubles ^SortedDoubles [nums]
  (if (sorted-doubles? nums)
    nums
    #?(:clj
       (let [^doubles a (if (doubles? nums) nums #_(aclone ^doubles nums) (double-array nums))
             last-num   (let [n (dec (alength a))] (when (>= n 0) (aget a n)))]
         (java.util.Arrays/sort a) ; O(n.log_n) on JDK 7+
         (SortedDoubles.        a last-num))

       :cljs
       (let [a        (if (array? nums) (aclone nums) (to-array nums))
             last-num (let [n (dec (alength a))] (when (>= n 0) (aget a n)))]
         (goog.array/sort a)
         (SortedDoubles.  a last-num)))))

;;;; Tuples

(do
  (deftype Tup2 [x y  ])
  (deftype Tup3 [x y z]))

(defn multi-reduce
  "Like `reduce` but supports separate simultaneous accumulators
  as a micro-optimisation when reducing a large collection multiple
  times."
  ;; Faster than using volatiles
  ([f  init           coll] (reduce f init coll))
  ([f1 init1 f2 init2 coll]
   (let [^Tup2 tuple
         (reduce
           (fn [^Tup2 tuple in]
             (Tup2.
               (f1 (.-x tuple) in)
               (f2 (.-y tuple) in)))
           (Tup2. init1 init2)
           coll)]

     [(.-x tuple) (.-y tuple)]))

  ([f1 init1 f2 init2 f3 init3 coll]
   (let [^Tup3 tuple
         (reduce
           (fn [^Tup3 tuple in]
             (Tup3.
               (f1 (.-x tuple) in)
               (f2 (.-y tuple) in)
               (f2 (.-z tuple) in)))
           (Tup3. init1 init2 init3)
           coll)]

     [(.-x tuple) (.-y tuple) (.-z tuple)])))

;;;; Percentiles

(defn- weighted-nth
  ^double [nums ^double idx]
  (let [idx-floor (Math/floor idx)
        idx-ceil  (Math/ceil  idx)]

    (if (== idx-ceil idx-floor)
      (double (nth nums (int idx)))

      ;; Generalization of (floor+ceil)/2
      (let [weight-floor (- idx-ceil idx)
            weight-ceil  (- 1 weight-floor)]
        (+
          (* weight-floor (double (nth nums (int idx-floor))))
          (* weight-ceil  (double (nth nums (int idx-ceil)))))))))

(defn percentile
  "Returns ?double element."
  [nums p]
  (let [snums (sorted-doubles nums)
        max-idx  (dec (count snums))]
    (when (>= max-idx 0)
      (nth snums (Math/round (* max-idx (enc/as-pnum! p)))))))

(comment (percentile (range 101) 0.8))

(defn percentiles
  "Returns ?[min p25 p50 p75 p90 p95 p99 max] double elements in:
    - O(1) for Sorted types (SortedLongs, SortedDoubles),
    - O(n.log_n) otherwise."
  [nums]
  (let [snums (sorted-doubles nums)
        max-idx   (dec (count nums))]
    (when (>= max-idx 0)
      [(nth snums 0)
       (nth snums (Math/round (* max-idx 0.25)))
       (nth snums (Math/round (* max-idx 0.50)))
       (nth snums (Math/round (* max-idx 0.75)))
       (nth snums (Math/round (* max-idx 0.90)))
       (nth snums (Math/round (* max-idx 0.95)))
       (nth snums (Math/round (* max-idx 0.99)))
       (nth snums               max-idx)])))

(comment
  (percentiles (range 101))
  (percentiles [1 2 3]))

;;;;

(defn bessel-correction ^double [n ^double add] (+ (double n) add))

(defn rf-sum          ^double [^double acc ^double in] (+ acc in))
(defn rf-sum-variance ^double [^double xbar ^double acc x]
  (+ acc (Math/pow (- (double x) xbar) 2.0)))

(defn rf-sum-abs-deviation ^double [^double central-point ^double acc x]
  (+ acc (Math/abs (- (double x) central-point))))

;;;; SummaryStats

(deftype SummaryStats
    ;; - Field names chosen to avoid shadowing.
    ;; - Includes -sum data to support merging.
    [^boolean xlongs?
     ^long    nx
     ^double  xmin
     ^double  xmax
     ^double  xlast
     ^double  xsum
     ^double  xmean
     ^double  xvar-sum
     ^double  xmad-sum
              xvar ; May be nil
     ^double  xmad
     ^double  p25
     ^double  p50
     ^double  p75
     ^double  p90
     ^double  p95
     ^double  p99
     as-map_]

  Object (toString [_] (str "SummaryStats[n=" nx "]"))
  #?@(:clj  [clojure.lang.IDeref ( deref [this] @as-map_)]
      :cljs [             IDeref (-deref [this] @as-map_)]))

(defn summary-stats?
  "Returns true iff given a SummaryStats argument."
  [x] (instance? SummaryStats x))

(defn ^:public summary-stats
  "Given a coll of numbers or previously dereffed SummaryStats map,
  returns a new mergeable ?SummaryStats with:
    (deref ss) => {:keys [n min max p25 ... p99 mean var mad]}

  See also `summary-stats-merge`."
  {:arglists '([nums-or-ss-map])}
  [x]
  (when x
    (enc/cond
      (summary-stats? x) x
      (map?           x)
      (let [{:keys [n min max last sum mean var-sum mad-sum var mad
                    p25 p50 p75 p90 p95 p99]} x]

        (SummaryStats. (int? last)
          n min max last sum mean var-sum mad-sum var mad
          p25 p50 p75 p90 p95 p99 (delay x)))

      :else
      (let [nums x
            n1             (first nums) ; Before (possibly mutable) sort
            snums (sorted-doubles nums)
            nx            (count snums)]

        (when-not (zero? nx)
          (let [xsum (double (reduce rf-sum 0.0 snums))
                xbar (/ xsum (double nx))

                [^double xvar-sum ^double xmad-sum]
                (multi-reduce
                  (partial rf-sum-variance      xbar) 0.0
                  (partial rf-sum-abs-deviation xbar) 0.0
                  snums)

                xvar (/ xvar-sum nx) ; nx w/o bessel-correction
                xmad (/ xmad-sum nx)

                [xmin p25 p50 p75 p90 p95 p99 xmax]
                (percentiles snums)

                xlongs? (int? n1)
                xlast (.-last snums)]

            (SummaryStats. xlongs?
              nx xmin xmax xlast xsum xbar xvar-sum xmad-sum xvar xmad
              p25 p50 p75 p90 p95 p99
              (delay
                (let [fin (if xlongs? #(Math/round (double %)) identity)]
                  {:n       nx
                   :min     (fin xmin)
                   :max     (fin xmax)
                   :last    (fin xlast)
                   :sum     (fin xsum)
                   :mean    xbar
                   :var-sum xvar-sum
                   :mad-sum xmad-sum
                   :var     xvar
                   :mad     xmad
                   :p25     p25
                   :p50     p50
                   :p75     p75
                   :p90     p90
                   :p95     p95
                   :p99     p99})))))))))

(comment @(summary-stats [1 2 3]))

(defn ^:public summary-stats-merge
  "Given one or more SummaryStats, returns a new ?SummaryStats with:
    (summary-stats-merge
       (summary-stats nums1)
       (summary-stats nums2))

    an approximatation of (summary-stats (merge nums1 nums2))

  Useful when you want summary stats for a large coll of numbers for which
  it would be infeasible/expensive to keep all numbers for accurate merging."
  ([ss1    ] ss1)
  ([ss1 ss2]
   (if ss1
     (if ss2
       (let [^SummaryStats ss1 ss1
             ^SummaryStats ss2 ss2

             nx1 (.-nx ss1)
             nx2 (.-nx ss2)

             _ (assert (pos? nx1))
             _ (assert (pos? nx2))

             xlongs1?  (.-xlongs?  ss1)
             xmin1     (.-xmin     ss1)
             xmax1     (.-xmax     ss1)
             ;; xlast1 (.-xlast    ss1)
             xsum1     (.-xsum     ss1)
             xvar-sum1 (.-xvar-sum ss1)
             xmad-sum1 (.-xmad-sum ss1)
             p25-1     (.-p25      ss1)
             p50-1     (.-p50      ss1)
             p75-1     (.-p75      ss1)
             p90-1     (.-p90      ss1)
             p95-1     (.-p95      ss1)
             p99-1     (.-p99      ss1)

             xlongs2?  (.-xlongs?  ss2)
             xmin2     (.-xmin     ss2)
             xmax2     (.-xmax     ss2)
             xlast2    (.-xlast    ss2)
             xsum2     (.-xsum     ss2)
             xvar-sum2 (.-xvar-sum ss2)
             xmad-sum2 (.-xmad-sum ss2)
             p25-2     (.-p25      ss2)
             p50-2     (.-p50      ss2)
             p75-2     (.-p75      ss2)
             p90-2     (.-p90      ss2)
             p95-2     (.-p95      ss2)
             p99-2     (.-p99      ss2)

             xlongs3?  (and xlongs1? xlongs2?)
             nx3       (+ nx1 nx2)
             nx1-ratio (/ (double nx1) (double nx3))
             nx2-ratio (/ (double nx2) (double nx3))

             xsum3 (+ xsum1 xsum2)
             xbar3 (/ (double xsum3) (double nx3))
             xmin3 (if (< xmin1 xmin2) xmin1 xmin2)
             xmax3 (if (> xmax1 xmax2) xmax1 xmax2)

             ;; Batched "online" calculation here is better= the standard
             ;; Knuth/Welford method, Ref. http://goo.gl/QLSfOc,
             ;;                            http://goo.gl/mx5eSK.
             ;; No apparent advantage in using `xbar3` asap (?).
             xvar-sum3 (+ xvar-sum1 xvar-sum2)
             xmad-sum3 (+ xmad-sum1 xmad-sum2)

             ;; These are pretty rough approximations. More sophisticated
             ;; approaches not worth the extra cost/effort in our case.
             p25-3 (+ (* nx1-ratio p25-1) (* nx2-ratio p25-2))
             p50-3 (+ (* nx1-ratio p50-1) (* nx2-ratio p50-2))
             p75-3 (+ (* nx1-ratio p75-1) (* nx2-ratio p75-2))
             p90-3 (+ (* nx1-ratio p90-1) (* nx2-ratio p90-2))
             p95-3 (+ (* nx1-ratio p95-1) (* nx2-ratio p95-2))
             p99-3 (+ (* nx1-ratio p99-1) (* nx2-ratio p99-2))

             xvar3 (when (> nx3 2) (/ xvar-sum3 (bessel-correction nx3 -2.0)))
             xmad3                 (/ xmad-sum3                         nx3)]

         (SummaryStats. xlongs3?
           nx3 xmin3 xmax3 xlast2 xsum3 xbar3 xvar-sum3 xmad-sum3 xvar3 xmad3
           p25-3 p50-3 p75-3 p90-3 p95-3 p99-3
           (delay
             (let [fin (if xlongs3? #(Math/round (double %)) identity)]
               {:n       nx3
                :min     (fin xmin3)
                :max     (fin xmax3)
                :last    (fin xlast2)
                :sum     (fin xsum3)
                :mean    xbar3
                :var-sum xvar-sum3
                :mad-sum xmad-sum3
                :var     xvar3
                :mad     xmad3
                :p25     p25-3
                :p50     p50-3
                :p75     p75-3
                :p90     p90-3
                :p95     p95-3
                :p99     p99-3}))))
       ss1)
     ss2)))

;;;; Stateful SummaryStats

(defn- buf-new
  ([    ] #?(:clj (LinkedList.) :cljs (array)))
  ([init]
   #?(:clj  (if init (LinkedList. init) (LinkedList.))
      :cljs (if init (array       init) (array)))))

(defn- buf-add [buf x]
  #?(:clj  (.add ^LinkedList buf x)
     :cljs (.push            buf x)))

(defn- buf-len ^long [buf]
  #?(:clj  (.size ^LinkedList buf)
     :cljs (alength           buf)))

(defprotocol ISummaryStatsBuffered
  ;; TODO Later generalize protocol for other stateful SummaryStats types?
  (ssb-deref [_] [_ flush-buffer?] "Returns current ?sstats.")
  (ssb-clear [_]   "Clears all internal state and returns nil.")
  (ssb-flush [_]   "Flushes internal buffer and returns newly merged sstats or nil.")
  (ssb-push  [_ n] "Adds given num to internal buffer."))

(defn ^:public summary-stats-clear!
  "Clears internal state (incl. stats and buffers, etc.) for given
  stateful SummaryStats instance and returns nil."
  [stateful-summary-stats]
  (ssb-clear stateful-summary-stats))

(deftype SummaryStatsBuffered [sstats_ buf_ buf-size merge-counter merge-cb]
  Object
  (toString [_] ; "SummaryStatsBuffered[n=1, pending=8, merged=0]"
    (str
      "SummaryStatsBuffered[n=" (get @sstats_ :n 0)
      ", pending=" (buf-len @buf_)
      (when-let [mc merge-counter] (str ", merged=" @mc))
      "]"))

  #?@(:clj  [clojure.lang.IDeref ( deref [this] (ssb-deref this))]
      :cljs [             IDeref (-deref [this] (ssb-deref this))])

  #?@(:clj  [clojure.lang.IFn ( invoke [this n] (ssb-push this n))]
      :cljs [             IFn (-invoke [this n] (ssb-push this n))])

  ISummaryStatsBuffered
  (ssb-deref [this              ] (ssb-deref this true))
  (ssb-deref [this flush-buffer?] (or (and flush-buffer? (ssb-flush this)) @sstats_))
  (ssb-clear [_]
    #?(:clj (locking buf_ (reset! buf_ (buf-new)))
       :cljs              (reset! buf_ (buf-new)))

    (reset! sstats_ nil)
    (when-let [mc merge-counter] (mc :set 0))
    nil)

  (ssb-flush [this]
    (let [[drained]
          #?(:clj (locking buf_ (reset-vals! buf_ (buf-new nil)))
             :cljs              (reset-vals! buf_ (buf-new nil)))]

      (if (== (buf-len drained) 0)
        nil
        (let [t0             (when merge-cb (enc/now-nano*))
              _              (when-let [mc merge-counter] (mc))
              sstats-drained (summary-stats drained)

              sstats-merged ; Only drainer will update, so should be no contention
              (swap! sstats_ summary-stats-merge sstats-drained)]

          (when merge-cb ; Handy for profilers, etc.
            (merge-cb this (- (enc/now-nano*) ^long t0)))

          sstats-merged))))

  (ssb-push [this n]
    #?(:clj (locking buf_ (buf-add @buf_ n))
       :cljs              (buf-add @buf_ n))

    (when-let [^long nmax buf-size]
      (when (> (buf-len @buf_) nmax)
        (ssb-flush this)))

    nil))

(defn ^:public summary-stats-buffered
  "Returns a new stateful SummaryStatsBuffered with:
    (ssb <num>) => Adds given number to internal buffer.
    (deref ssb) => Flushes buffer if necessary, and returns a mergeable
                   ?SummaryStats. Deref again to get a map of summary
                   stats for all numbers ever added to ssb:
                     {:keys [n min max p25 ... p99 mean var mad]}.

  Useful for summarizing a (possibly infinite) stream of numbers.

  Options:
    :buffer-size - The maximum number of numbers that may be buffered
                   before next (ssb <num>) call will block to flush
                   buffer and merge with any existing summary stats.

                   Larger buffers mean better performance and more
                   accurate stats, at the cost of more memory use
                   while buffering.

    :buffer-init - Initial buffer content, useful for persistent ssb.
    :sstats-init - Initial summary stats,  useful for persistent ssb."

  ([] (summary-stats-buffered nil))
  ([{:keys [buffer-size buffer-init sstats-init merge-cb]
     :or   {buffer-size 1e5}
     :as   opts}]

   (SummaryStatsBuffered.
     (atom (summary-stats sstats-init))
     (atom       (buf-new buffer-init))
     (long                buffer-size)
     (enc/counter)
     merge-cb ; Undocumented
     )))

(defn summary-stats-buffered-fast
  "Returns fastest possible SummaryStatsBuffered."
  [^long buffer-size merge-cb]
  (SummaryStatsBuffered.
     (atom nil)
     (atom (buf-new))
     buffer-size
     nil
     merge-cb))

(comment
  (let [ssb (summary-stats-buffered {:buffer-size 10})] ; 266 qb
    [(enc/qb 1e6 (ssb (rand-int 1000))) (str ssb) @@ssb]))

(defn summary-stats-buffered?
  "Returns true iff given a SummaryStatsBuffered instance."
  [x] (instance? SummaryStatsBuffered x))

(defn summary-stats-stateful?
  "Returns true iff given a stateful SummaryStats instance."
  [x] (summary-stats-buffered? x))

;;;; Print methods

#?(:clj (enc/deftype-print-methods SortedDoubles SummaryStats SummaryStatsBuffered))

;;;; Formatting

(defn- perc [n d] (str (Math/round (* (/ (double n) (double d)) 100.0)) "%"))
(comment [(perc 1 1) (perc 1 100) (perc 12 44)])

#?(:clj (def ^:private locale (java.util.Locale/getDefault)))
#?(:clj (defn- fmt [pattern & args] (String/format locale pattern (to-array args))))

(defn- fmt-2f    [n] #?(:clj (fmt "%.2f" n) :cljs (str (enc/round2 n))))
(defn- fmt-calls [n] #?(:clj (fmt "%,d"  n) :cljs
                        (str ; Thousands separator
                          (when (neg? n) "-")
                          (->>
                            (str (Math/abs n))
                            (reverse)
                            (partition 3 3 "")
                            (map str/join)
                            (str/join ",")
                            (str/reverse)))))

(defn- fmt-nano [nanosecs]
  (let [ns (double nanosecs)]
    (cond
      (>= ns 6e10) (str (fmt-2f (/ ns 6e10)) "m ")
      (>= ns 1e9)  (str (fmt-2f (/ ns 1e9))  "s ")
      (>= ns 1e6)  (str (fmt-2f (/ ns 1e6))  "ms")
      (>= ns 1e3)  (str (fmt-2f (/ ns 1e3))  "μs")
      :else        (str (fmt-2f    ns)       "ns"))))

(comment
  (fmt "%.2f" 12345.67890)
  (fmt-2f     12345.67890)
  (fmt-calls  12345)
  (fmt-nano   12345.67890))

(def     all-format-columns [:n :min   :p25 :p50   :p75 :p90 :p95 :p99 :max :mean :mad :clock :sum])
(def default-format-columns [:n :min #_:p25 :p50 #_:p75 :p90 :p95 :p99 :max :mean :mad :clock :sum])

(let [migrate        {:n-calls :n, :total :sum} ; For back-compatibility
      format-column? (set all-format-columns)
      format-column  (fn [column] (have format-column? (get migrate column column)))]

  (defn- format-columns [columns]
    (enc/cond
      (identical? columns default-format-columns) default-format-columns
      (identical? columns     all-format-columns)     all-format-columns
      :else (mapv format-column columns))))

(comment (enc/qb 1e6 (format-columns [:min :n-calls :total])))

(def default-format-id-fn (fn [id] (str id)))

;; id-sstats* => {<id> <sstats>} or {<id> <sstats-map>}

(defn get-max-id-width
  [id-sstats*
   {:keys [format-id-fn]
    :or   {format-id-fn default-format-id-fn}}]

  (when id-sstats*
    (reduce-kv
      (fn [^long acc id _sstats*]
        (let [c (count (format-id-fn id))]
          (if (> c acc) c acc)))
      9 ; (count "Accounted")
      id-sstats*)))

(defn summary-stats-format
  "Given {<id> <sstats>} or {<id> <sstats-map>}, returns a formatted table
  string. Assumes nanosecond clock, and stats based on profiling id'd
  nanosecond times."
  [clock-total id-sstats*
   {:keys [columns sort-fn format-id-fn max-id-width] :as opts
    :or   {columns      default-format-columns
           sort-fn      (fn [ss] (get (enc/force-ref ss) :sum))
           format-id-fn default-format-id-fn}}]

  (when id-sstats*
    (let [columns (format-columns columns)
          clock-total (long clock-total)
          ^long accounted-total
          (reduce-kv
            (fn [^long acc _id ss]
              (+ acc (long (get (enc/force-ref ss) :sum))))
            0 id-sstats*)

          sorted-ids
          (sort-by
            (fn [id] (sort-fn (get id-sstats* id)))
            enc/rcompare
            (keys id-sstats*))

          ^long max-id-width
          (or
            max-id-width
            (get-max-id-width id-sstats* opts))

          column->pattern
          {:id      {:heading "pId" :min-width max-id-width :align :left}
           :n       {:heading "nCalls"}
           :min     {:heading "Min"}
           :p25     {:heading "25% ≤"}
           :p50     {:heading "50% ≤"}
           :p75     {:heading "75% ≤"}
           :p90     {:heading "90% ≤"}
           :p95     {:heading "95% ≤"}
           :p99     {:heading "99% ≤"}
           :max     {:heading "Max"}
           :mean    {:heading "Mean"}
           :mad     {:heading "MAD"   :min-width 5}
           :sum     {:heading "Total" :min-width 6}
           :clock   {:heading "Clock"}}

          sb (enc/str-builder "")

          append-col
          (fn [column s]
            (let [{:keys [min-width align]
                   :or   {min-width 10 align :right}}
                  (get column->pattern column)]

              (enc/sb-append sb
                (enc/format
                  (str "%" (case align :left "-" :right "") min-width "s")
                  s))))]

      ; Write header rows
      (doseq [column (into [:id] columns)]
        (when-not (= :id column)
          (enc/sb-append sb " "))
        (append-col column (get-in column->pattern [column :heading])))

      (enc/sb-append sb "\n\n")

      ; Write id rows
      (doseq [id sorted-ids]
        (let [ssm (enc/force-ref (get id-sstats* id))
              {:keys [n sum mean mad]} ssm]

          (append-col :id (format-id-fn id))
          (doseq [column columns]
            (enc/sb-append sb " ")
            (case column
              :n     (append-col column (fmt-calls n))
              :mean  (append-col column (fmt-nano mean))
              :mad   (append-col column (str "±" (perc mad mean)))
              :sum   (append-col column (perc sum clock-total))
              :clock (append-col column (fmt-nano sum))
              (do    (append-col column (fmt-nano (get ssm column))))))

          (enc/sb-append sb "\n")))

      ; Write accounted row
      (enc/sb-append sb "\n")
      (append-col :id "Accounted")
      (doseq [column columns]
        (enc/sb-append sb " ")
        (case column
          :sum   (append-col column (perc accounted-total clock-total))
          :clock (append-col column (fmt-nano accounted-total))
          (do    (append-col column ""))))

      ; Write clock row
      (enc/sb-append sb "\n")
      (append-col :id "Clock")
      (doseq [column columns]
        (enc/sb-append sb " ")
        (case column
          :sum   (append-col column "100%")
          :clock (append-col column (fmt-nano clock-total))
          (do    (append-col column ""))))

      (enc/sb-append sb "\n")
      (str sb))))

(comment
  (defn rand-vs [n & [max]]
    (take n (repeatedly (partial rand-int (or max Integer/MAX_VALUE)))))

  (println
    (summary-stats-format (* 1e6 30)
      {:foo (summary-stats (rand-vs 1e4 20))
       :bar (summary-stats (rand-vs 1e2 50))
       :baz (summary-stats (rand-vs 1e5 30))}
      {}) "\n"))

;;;; Aliases

;; (enc/defalias sstats       summary-stats)
;; (enc/defalias sstats-merge summary-stats-merge)

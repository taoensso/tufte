(ns taoensso.tufte.impl
  "Private implementation details.
  `profiled` -> [<result> <derefable-and-mergeable-pstats>].

  Profiling consists of:
    1. State init   ; On  thread
    2. Capture      ; On  thread
    3. State deref  ; On  thread
    4. ?Merging     ; Off thread, on demand (deferred cost)
    5. ?Realization ; Off thread, on demand (deferred cost)

  Basic implementation:
    - Capture [<id> <elapsed>]s into single mutable acc
      - May compact acc      to id-times, {<id> (<time>        ...)}
      - May compact id-times to id-stats, {<id> (<stats/stats> ...)}
    - Merge pours (read-only) acc0 + acc1 into id-times
      - May compact id-times to id-stats, {<id> (<stats/stats> ...)}
    - Realization:
        - Generates {<id> <stats/stats>} from id-times.
        - Merges with id-stats."

  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc :refer-macros []]
   [taoensso.tufte.stats :as stats])

  #?(:clj
     (:import [java.util LinkedList Stack]
              [java.util.concurrent ArrayBlockingQueue]))

  #?(:cljs
     (:require-macros
      [taoensso.tufte.impl :refer [mt-acc mt-add mt-count atom?]])))

;;;; Mutable accumulators

(deftype Time     [id ^long t])
(deftype TimeSpan [^long t0 ^long t1])
(comment (enc/qb 1e6 (Time. :foo 1000))) ; 33.59

#?(:clj
   (do
     (defmacro ^:private mt-acc     [] `(enc/if-cljs (cljs.core/array) (LinkedList.)))
     (defmacro ^:private mt-add [mt x] `(enc/if-cljs (.push   ~mt ~x) (.add  ~(with-meta mt {:tag 'LinkedList}) ~x)))
     (defmacro ^:private mt-count [mt] `(enc/if-cljs (alength ~mt)    (.size ~(with-meta mt {:tag 'LinkedList}))))))

(comment (enc/qb 1e6 (mt-acc) (atom nil))) ; [29.14 57.76]

;;;; PStats (Profiling Stats)
;; API-level state we'll return from `profiled`: derefable, mergeable

(deftype PStats [pd ^long t1 tspans realized_]
  #?@(:clj  [clojure.lang.IDeref    (deref     [_]           @realized_)]
      :cljs [             IDeref   (-deref     [_]           @realized_)])
  #?@(:clj  [clojure.lang.IPending (isRealized [_] (realized? realized_))]
      :cljs [             IPending (-realized? [_] (realized? realized_))]))

;;;; PData (Profiling Data)
;; Implementation-level state while profiling,
;;   - id-times: ?{<id> (<time>        ...)}
;;   - id-stats: ?{<id> (<stats/stats> ...)}

(declare ^:private deref-pdata)
(deftype PState [acc id-times id-stats])
(deftype  PData [^long nmax ^long t0 pstate_]
  #?@(:clj  [clojure.lang.IDeref  (deref [this] (deref-pdata this))]
      :cljs [             IDeref (-deref [this] (deref-pdata this))]))

(defn new-pdata-local   [^long nmax] (let [t0 (enc/now-nano*)] (PData. nmax t0 (volatile! (PState. (mt-acc)   nil nil)))))
(defn new-pdata-dynamic [^long nmax] (let [t0 (enc/now-nano*)] (PData. nmax t0 (atom      (PState. (atom nil) nil nil)))))

(comment (enc/qb 1e6 (new-pdata-local 10) (new-pdata-dynamic 10))) ; [98.18 138.28]

(declare ^:private deref-pstats)
(defn- deref-pdata "PData->PStats" [^PData pd]
  ;; NB (.-acc pd) should never be mutated from this point!
  (let [t1     (enc/now-nano*)
        t0     (.-t0 pd)
        tspans (list (TimeSpan. t0 t1))]

    (PStats. pd t1 tspans (delay (deref-pstats pd t1 tspans)))))

(comment (enc/qb 1e6 @(new-pdata-local 10))) ; 245.08

(def ^:dynamic *pdata* "nnil iff dynamic profiling active" nil)

#?(:clj
   (let [;; Stack to support `profile/d` nesting
         ^ThreadLocal stack-proxy (proxy [ThreadLocal] [] (initialValue [] (Stack.)))
         ^ThreadLocal pdata-proxy (proxy [ThreadLocal] [])]

     (defn pdata-local-get [] (.get pdata-proxy)) ; => nnil iff thread-local profiling active
     (defn pdata-local-pop []
       (let [^Stack stack (.get stack-proxy)]
         (if-let [stashed (when-not (.empty stack) (.pop stack))]
           (do (.set pdata-proxy stashed) stashed)
           (do (.set pdata-proxy nil)     nil))))

     (defn pdata-local-push [v]
       (let [^Stack stack  (.get stack-proxy)]
         (if-let [to-stash (.get pdata-proxy)]
           (do (.push stack to-stash) (.set pdata-proxy v) v)
           (do                        (.set pdata-proxy v) v)))))

   :cljs ; Note single-threaded platform
   (let [stack #js [] ; To support `profile/d` nesting
         pdata_ (volatile! false)]

     (defn pdata-local-get [] @pdata_) ; => nnil iff thread-local profiling active
     (defn pdata-local-pop []
       (if-let [stashed (.pop stack)]
         (vreset! pdata_ stashed)
         (vreset! pdata_ nil)))

     (defn pdata-local-push [v]
       (if-let [to-stash @pdata_]
         (do (.push stack to-stash) (vreset! pdata_ v))
         (do                        (vreset! pdata_ v))))))

(comment
  (pdata-local-push "foo")
  (pdata-local-pop)
  (enc/qb 1e6 *pdata* (pdata-local-get)) ; [63.7 48.77]
  (enc/qb 1e6  ; [507.58 74.62]
    (binding [*pdata* "foo"])
    (try (pdata-local-push "foo") (finally (pdata-local-pop))))

  (do
    (pdata-local-push "pd1")
    (pdata-local-push "pd2")
    [(let [pd (pdata-local-get)] (pdata-local-pop) pd)
     (let [pd (pdata-local-get)] (pdata-local-pop) pd)]))

;;;; TimeSpan utils

(deftype ElapsedTimeAcc [^long tsum ^long max-t1])
(let [sort-tspans (fn [tspans] (sort-by (fn [^TimeSpan tspan] (.-t0 tspan)) tspans))]
  (defn- tspans->tsum
    "Returns `tsum` (elapsed time) given collection of `TimeSpan`s.
    Based on https://codereview.stackexchange.com/a/126927."
    ^long [tspans]
    (if (empty? tspans)
      0
      (let [sorted-tspans (sort-tspans tspans)] ; O(n.logn)
        (.-tsum ^ElapsedTimeAcc
          (reduce
            (fn [^ElapsedTimeAcc acc ^TimeSpan tspan]
              (let [t1     (.-t1     tspan)
                    max-t1 (.-max-t1 acc)]
                (if (> t1 max-t1)
                  (let [t0   (.-t0   tspan)
                        tsum (.-tsum acc)]
                    (ElapsedTimeAcc. (+ tsum (- t1 (Math/max t0 max-t1))) t1))
                  acc)))
            (ElapsedTimeAcc. 0 0)
            sorted-tspans))))))

(comment
  (tspans->tsum nil)
  (tspans->tsum [])
  (tspans->tsum [(TimeSpan. 1   3) (TimeSpan. 3 6)])
  (tspans->tsum [(TimeSpan. 3   6) (TimeSpan. 1 3)])
  (tspans->tsum [(TimeSpan. 1  10) (TimeSpan. 3 6)])
  (enc/qb 1e6
    (tspans->tsum
      [(TimeSpan. 10 14)
       (TimeSpan.  4 18)
       (TimeSpan. 19 20)
       (TimeSpan. 19 20)
       (TimeSpan. 13 20)])))

(defn- merge-tspans [^long nmax ^long t1 tspans0 tspans1]
  (let [n0      (count tspans0)
        n1      (count tspans1)
        tspans2 (if (> n1 n0) (into tspans1 tspans0) (into tspans0 tspans1))]

    (if (> (+ n0 n1) nmax) ; Compact, may lose some accuracy
      (let [tsum (tspans->tsum tspans2)]
        (list (TimeSpan. (- t1 tsum) t1)))
      tspans2)))

(comment
  (merge-tspans 2 50
    (list (TimeSpan. 1 10) (TimeSpan. 5  20))
    (list (TimeSpan. 1 10) (TimeSpan. 20 50))))

;;;;

(defn- times-into-id-times
  "NB treats `from-times` as read-only (may be mutable `acc`)!"
  [to-id-times from-times]
  (not-empty
    (if-let [from-times (enc/force-ref from-times)]
      (persistent!
        (reduce
          (fn [m ^Time in]
            (let [id (.-id in)
                  t  (.-t  in)]
              (assoc! m id (conj (get m id) t))))
          (transient (or to-id-times {}))
          from-times))
      to-id-times)))

(comment
  (times-into-id-times nil nil)
  (times-into-id-times {}  nil)
  (let [mt (mt-acc)]
    (mt-add mt (Time. :foo 2))
    (times-into-id-times {:foo '(1)} mt)))

(defn- deref-pstats
  "PStats->{:clock _ :stats {<id> <stats/stats>}} (API output)"
  [^PData pd ^long t1 tspans]
  (let [t0      (.-t0      pd)
        pstate_ (.-pstate_ pd)
        ^PState pstate (enc/force-ref pstate_)
        id-times (.-id-times pstate)
        id-stats (.-id-stats pstate)
        id-times (times-into-id-times id-times (.-acc pstate))
        id-stats ; Final {<id> <stats/stats>}
        (reduce-kv
          (fn [m id times]
            (let [stats<times (stats/stats times)
                  merged (reduce stats/merge-stats stats<times (get id-stats id))]
              (assoc m id merged)))
          id-times
          id-times)]

    {:stats id-stats
     :clock {:t0 t0 :t1 t1 :total (tspans->tsum tspans)}}))

(comment @@(new-pdata-local 10))

(defn- fast-into [c0 c1] (if (> (count c0) (count c1)) (into c0 c1) (into c1 c0)))
(comment (fast-into nil nil))

(defn- merge-stats-when-needed [^long nmax stats]
  (if (<= (count stats) nmax)
    stats
    (list (reduce stats/merge-stats stats))))

(defn merge-pstats "Compacting merge"
  ([     ps0 ps1] (merge-pstats nil ps0 ps1))
  ([nmax ps0 ps1]
   (if ps0
     (if ps1
       (let [^PStats ps0       ps0
             ^PStats ps1       ps1
             ^PData  pd0 (.-pd ps0)
             ^PData  pd1 (.-pd ps1)

             nmax (long (or nmax (.-nmax pd0)))
             pd0-t0 (.-t0 pd0)
             ps0-t1 (.-t1 ps0)
             pd1-t0 (.-t0 pd1)
             ps1-t1 (.-t1 ps1)

             pd2-t0  (if (< pd0-t0 pd1-t0) pd0-t0 pd1-t0)
             ps2-t1  (if (> ps0-t1 ps1-t1) ps0-t1 ps1-t1)
             tspans2 (merge-tspans nmax ps2-t1 (.-tspans ps0) (.-tspans ps1))

             ^PState pd0-pstate (enc/force-ref (.-pstate_ pd0))
             ^PState pd1-pstate (enc/force-ref (.-pstate_ pd1))

             pd0-id-times (times-into-id-times (.-id-times pd0-pstate) (.-acc pd0-pstate))
             pd1-id-times (times-into-id-times (.-id-times pd1-pstate) (.-acc pd1-pstate))
             pd0-id-stats (.-id-stats pd0-pstate)
             pd1-id-stats (.-id-stats pd1-pstate)

             ;; All ids in pd0 or pd1
             pd2-ids (keys (conj (or pd0-id-times {}) pd1-id-times))

             ;; Merge pd1 into pd0 to get pd2
             [pd2-id-times pd2-id-stats]
             (reduce
               (fn [[pd2-id-times pd2-id-stats] id]
                 (let [pd0-times (get pd0-id-times id)
                       pd0-stats (get pd0-id-stats id)
                       pd1-times (get pd1-id-times id)
                       pd1-stats (get pd1-id-stats id)

                       pd2-times (fast-into pd0-times pd1-times)
                       pd2-stats (fast-into pd0-stats pd1-stats)]

                   (if (<= (count pd2-times) nmax) ; Common case
                     [(assoc pd2-id-times id pd2-times)
                      (assoc pd2-id-stats id pd2-stats)]

                     ;; Times need compaction
                     (let [stats<times (stats/stats pd2-times)]
                       [(assoc pd2-id-times id nil)
                        (assoc pd2-id-stats id
                          (merge-stats-when-needed nmax
                            (conj pd2-stats stats<times)))]))))

               [pd0-id-times pd0-id-stats]
               pd2-ids)

             pd2 (PData. nmax pd2-t0 (PState. nil pd2-id-times pd2-id-stats))]
         (PStats. pd2 ps2-t1 tspans2 (delay (deref-pstats pd2 ps2-t1 tspans2))))

       ps0)
     ps1)))

;;;; Time capture

#?(:clj
   (defmacro ^:private atom? [x]
     `(enc/if-cljs
        (instance?    cljs.core.Atom ~x)
        (instance? clojure.lang.Atom ~x))))

(declare ^:private compact-pstate)
(defn capture-time! [^PData pd id ns-elapsed]
  (let [nmax    (.-nmax    pd)
        pstate_ (.-pstate_ pd)
        ^PState pstate @pstate_
        acc (.-acc pstate)]

    (if (atom? acc)

      ;; Dynamic profiling
      (let [?pulled-times
            (loop []
              (let [old-times @acc
                    new-times (conj old-times (Time. id ns-elapsed))]
                (if (<= (count new-times) nmax)
                  (if (compare-and-set! acc old-times new-times) nil (recur))
                  (if (compare-and-set! acc old-times nil) new-times (recur)))))]

        (when-let [times ?pulled-times] ; Do compaction, rare
          (let [t0 (enc/now-nano*)]
            ;; Contention against `pstate_` unlikely since we just drained `acc`
            (swap! pstate_ (fn [pstate] (compact-pstate pstate times nmax true)))
            (recur pd :tufte/compaction (- (enc/now-nano*) t0)))))

      (do ; Common case: thread-local profiling
        (mt-add acc (Time. id ns-elapsed))

        (when (> (mt-count acc) nmax) ; Do compaction, rare
          (let [t0 (enc/now-nano*)]
            (vreset! pstate_ (compact-pstate pstate acc nmax false))
            (recur pd :tufte/compaction (- (enc/now-nano*) t0))))))))

(defn- compact-pstate [^PState pstate pulled-times ^long nmax dynamic?]
  ;; Note that compaction expense doesn't distort p times unless there's
  ;; p nesting (where outer p time includes inner p's capture time).
  (let [id-times (.-id-times pstate)
        id-stats (.-id-stats pstate)
        id-times (times-into-id-times id-times pulled-times)

        [id-times id-stats]
        (reduce-kv
          (fn [acc id times]
            (if (<= (count times) nmax)
              acc
              (let [[id-times id-stats] acc
                    stats<times (stats/stats times)]
                [(assoc id-times id nil)
                 (assoc id-stats id
                   (merge-stats-when-needed nmax
                     (conj (get id-stats id) stats<times)))])))

          [id-times id-stats]
          id-times)

        new-acc (if dynamic? (.-acc pstate) (mt-acc))]

    (PState. new-acc id-times id-stats)))

(comment
  (try
    (pdata-local-push (new-pdata-local 1e7))
    (enc/qb 1e6 (capture-time! (pdata-local-get) :foo 1))
    (finally (pdata-local-pop)))) ; 98.35

;;;; Output handlers

(enc/defonce handlers_ "{<hid> <handler-fn>}" (atom nil))

#?(:clj
   (enc/defonce ^:private ^ArrayBlockingQueue handler-queue
     "While user handlers should ideally be non-blocking, we'll use a queue
     here to be safe + make sure we never tie up the execution thread."
     (ArrayBlockingQueue. 1024)))

(defn- handle-blocking! [m]
  (enc/run-kv!
    (fn [id f]
      (enc/catching (f m) e
        (enc/catching ; Esp. nb for Cljs
          (println (str "WARNING: Uncaught Tufte `" id "` handler error\n" e)))))
    @handlers_))

#?(:clj  (declare ^:private handler-thread_))
#?(:cljs (defn handle! [m] (handle-blocking! m) nil))
#?(:clj  (defn handle! [m] (.offer handler-queue m) @handler-thread_ nil))
#?(:clj
   (defonce ^:private handler-thread_
     (delay
       (let [f (fn []
                 (loop []
                   (let [m (.take handler-queue)]
                     ;; Note: just drop if no registered handlers
                     (handle-blocking! m)
                     (recur))))]
         (doto (Thread. f)
           (.setDaemon true)
           (.start))))))

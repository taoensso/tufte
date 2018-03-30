(ns taoensso.tufte.impl
  "Private implementation details.
  `profiled` -> [<result> <derefable-and-mergeable-pstats>]."
  (:require [clojure.string  :as str]
            [taoensso.encore :as enc :refer-macros []]
            [taoensso.tufte.stats :as stats])

  #?(:clj (:import [java.util.concurrent ArrayBlockingQueue])))

;;;; PData (Profiling Data)
;; Implementation-level ~mutable state while profiling.

(def ^:dynamic *pdata* "nnil iff profiling active" nil)

(declare ^:private pdata->pstats)

;; Created once per `profiled` run, etc.
;; id-times: {<id> (<time> ...)_}_ -> {<id> (<time> ...)}
;; id-stats: {<id> (<map>  ...) }_ -> {<id> (<map>  ...)}
#?(:clj
   (deftype PData [^long nmax ^long t0 ^long t1 id-times id-stats]
     clojure.lang.IDeref (deref [this] (pdata->pstats this)))

   :cljs
   (deftype PData [^long nmax ^long t0 ^long t1 id-times id-stats]
     IDeref (-deref [this] (pdata->pstats this))))

(defmacro new-pdata
  ([    ] `(new-pdata 2e6))
  ([nmax] `(PData. ~nmax (enc/now-nano*) -1 (atom {}) (atom nil))))

(comment
  (enc/qb 1e6 (new-pdata)) ; 217.91
  @(new-pdata))

;;;; PStats (Profiling Stats)
;; API-level state we'll return from `profiled`: derefable, mergeable.

#?(:clj
   (deftype PStats [pdata realized_]
     clojure.lang.IDeref   (deref      [_]           @realized_)
     clojure.lang.IPending (isRealized [_] (realized? realized_)))

   :cljs
   (deftype PStats [pdata realized_]
     IDeref   (-deref     [_]           @realized_)
     IPending (-realized? [_] (realized? realized_))))

(declare ^:private realize-pstats)
(defn- deref-vals [m] (reduce-kv (fn [m k v] (assoc m k @v)) m m))
(defn- pdata->pstats [^PData pd0]
  (let [id-times (deref-vals @(.-id-times pd0))
        id-stats             @(.-id-stats pd0)
        pd1 (PData. (.-nmax pd0) (.-t0 pd0) (enc/now-nano*) id-times id-stats)]
    (PStats. pd1 (delay (realize-pstats pd1)))))

(comment (enc/qb 1e6 @(new-pdata))) ; 394.0

(defn- fast-into [c0 c1] (if (> (count c0) (count c1)) (into c0 c1) (into c1 c0)))
(comment (fast-into nil nil))

(defn merge-pstats
  ([     ps0 ps1] (merge-pstats nil ps0 ps1))
  ([nmax ps0 ps1]
   (if ps0
     (if ps1
       (let [^PData pd0 (.-pdata ^PStats ps0)
             ^PData pd1 (.-pdata ^PStats ps1)
             nmax (long (or nmax (.-nmax pd0)))

             pd0-t0 (.-t0 pd0)
             pd0-t1 (.-t1 pd0)
             pd1-t0 (.-t0 pd1)
             pd1-t1 (.-t1 pd1)

             pd2-t0 (if (< pd0-t0 pd1-t0) pd0-t0 pd1-t0)
             pd2-t1 (if (> pd0-t1 pd1-t1) pd0-t1 pd1-t1)

             pd0-id-times (.-id-times pd0)
             pd0-id-stats (.-id-stats pd0)
             pd1-id-times (.-id-times pd1)
             pd1-id-stats (.-id-stats pd1)

             [pd2-id-times pd2-id-stats] ; Merged with compaction
             (reduce-kv
               (fn [[pd2-id-times pd2-id-stats] id pd1-times]
                 (let [pd0-times (get pd0-id-times id)
                       pd0-stats (get pd0-id-stats id)
                       pd1-stats (get pd1-id-stats id)

                       pd2-times (fast-into pd0-times pd1-times)
                       pd2-stats (fast-into pd0-stats pd1-stats)]

                   (if (< (count pd2-times) nmax) ; Common case
                     [(assoc pd2-id-times id pd2-times)
                      (assoc pd2-id-stats id pd2-stats)]

                     ;; Times need compaction
                     (let [stats<times (stats/stats pd2-times)]
                       [(assoc pd2-id-times id nil)
                        (assoc pd2-id-stats id (conj pd2-stats stats<times))]))))

               [pd0-id-times pd0-id-stats]
               pd1-id-times)

             pd2 (PData. nmax pd2-t0 pd2-t1 pd2-id-times pd2-id-stats)]
         (PStats. pd2 (delay (realize-pstats pd2))))
       ps0)
     ps1)))

(defn- realize-pstats
  "Returns final API-level output."
  [^PData pd]
  (let [pd-t0 (.-t0 pd)
        pd-t1 (.-t1 pd)
        pd-id-times (.-id-times pd)
        pd-id-stats (.-id-stats pd)

        id-stats ; {<id> <stats/stats-map>}
        (reduce-kv
          (fn [m id times]
            (let [stats<times (stats/stats times)
                  merged (reduce stats/merge-stats stats<times (get pd-id-stats id))]
              (assoc m id merged)))

          pd-id-times
          pd-id-times)]

    {:clock {:t0 pd-t0 :t1 pd-t1 :total (- pd-t1 pd-t0)}
     :stats id-stats}))

;;;; Time capture

(defn capture-time! [^PData pd id ns-elapsed]
  (let [nmax      (.-nmax     pd)
        id-times_ (.-id-times pd) ; {<id> (<time> ...)}

        ;; Get the id'd atom (accumulator)
        times_
        (or (get @id-times_ id) ; Common case
            (get (swap! id-times_ (fn [m] (assoc m id (get m id (atom nil))))) id))

        ?pulled-times
        (loop []
          (let [times0 @times_
                times1 (conj times0 ns-elapsed)]
            (if (< (count times1) nmax) ; Times need compaction
              (if (compare-and-set! times_ times0 times1) ; Does == comparison, fast
                nil ; Common case
                (recur))

              (if (compare-and-set! times_ times0 nil) ; ''
                times1 ; Pull accumulated times, rare
                (recur)))))]

    ;; Do compaction, rare but expensive.
    ;; Though note that expense doesn't generally distort p times unless there's
    ;; p nesting (in which case outside p time will include inside p's capture
    ;; time).
    (when-let [times ?pulled-times]
      (let [t0          (enc/now-nano*)
            stats<times (stats/stats times)
            id-stats_   (.-id-stats pd)]

        (swap! id-stats_ (fn [m] (assoc m id (conj (get m id) stats<times))))
        (recur pd :tufte/compaction (- (enc/now-nano*) t0))))))

(comment (let [pd (new-pdata)] (enc/qb 1e6 (capture-time! pd :foo 1)))) ; 175.27

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

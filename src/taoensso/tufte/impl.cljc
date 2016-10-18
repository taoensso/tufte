(ns taoensso.tufte.impl
  "Private implementation details."
  (:require [clojure.string  :as str]
            [taoensso.encore :as enc :refer-macros []])
  #?(:clj
     (:import [java.util LinkedList]
              [java.util.concurrent ArrayBlockingQueue]))

  #?(:cljs
     (:require-macros
      [taoensso.tufte.impl :refer
       [mutable-times mt-add mt-count atom?]])))

;;;; Pdata

(defrecord PData [^long __t0]) ; + {<id> <times> :__m-id-stats {<id> <IdStats>}}
(def ^:dynamic *pdata_* "Non-nil iff dynamic profiling active." nil)

;; Would esp. benefit from ^:static support / direct linking / a Java class
(def ^:static pdata-proxy "Non-nil iff thread-local profiling active."
  #?(:clj
     (let [^ThreadLocal proxy (proxy [ThreadLocal] [])]
       (fn
         ([]        (.get proxy))
         ([new-val] (.set proxy new-val) new-val)))

     :cljs
     (let [state_ (volatile! false)] ; Automatically thread-local in js
       (fn
         ([]                @state_)
         ([new-val] (vreset! state_ new-val))))))

(comment (enc/qb 1e6 (pdata-proxy))) ; 48.39

(defmacro new-pdata-thread  []       `(PData. (enc/now-nano*)))
(defmacro new-pdata-dynamic [] `(atom (PData. (enc/now-nano*))))
(comment (macroexpand '(new-pdata-thread)))

;;;; Stats

(defrecord   Clock [^long t0 ^long t1 ^long total])
(defrecord IdStats [^long count ^long time ^long mean ^long mad-sum
                    ^double mad ^long min ^long max])
(defrecord   Stats [clock id-stats-map])

;;;; Time tracking
;; We can use mutable time accumulators when thread-local.
;; Note that LinkedList uses more mem but is faster than
;; java.util.ArrayList.

(defmacro ^:private mutable-times [] `(enc/if-cljs (cljs.core/array) (LinkedList.)))
(defmacro ^:private mt-add     [x t] `(enc/if-cljs (.push   ~x ~t) (.add ~(with-meta x {:tag 'LinkedList}) ~t)))
(defmacro ^:private mt-count     [x] `(enc/if-cljs (alength ~x)   (.size ~(with-meta x {:tag 'LinkedList}))))

;; Compaction (times->interim-stats) helps to prevent OOMs:
(def      ^:private ^:const nmax-times ">n will trigger compaction" (long 2e6))
(declare  ^:private times->IdStats)
(defmacro ^:private atom? [x]
  `(enc/if-cljs
     (instance?    cljs.core.Atom ~x)
     (instance? clojure.lang.Atom ~x)))

(defn ^:static capture-time! [pdata-or-pdata_ id t-elapsed]
  (if (atom? pdata-or-pdata_)

    ;; Using immutable thread-safe times, atom for coordination
    (let [pdata_ pdata-or-pdata_
          ?pulled-times
          (loop []
            (let [pdata @pdata_]
              (let [times (get pdata id ())]
                (if (>= (count times) nmax-times)
                  (if (compare-and-set! pdata_ pdata ; Never leave empty
                        (assoc pdata id (conj () t-elapsed)))
                    times ; Pull accumulated times
                    (recur))

                  (if (compare-and-set! pdata_ pdata
                        (assoc pdata id (conj times t-elapsed)))
                    nil
                    (recur))))))]

      (when-let [times ?pulled-times] ; Compact
        (let [id-stats (get-in @pdata_ [:__m-id-stats id])
              id-stats (times->IdStats times id-stats)]
          ;; Can reasonably assume that our id-stats key is
          ;; uncontended atm:
          (swap! pdata_ assoc-in [:__m-id-stats id] id-stats))))

    ;; Using mutable thread-local times (cheap), no coordination
    (let [pdata pdata-or-pdata_
          pdata (pdata-proxy) ; Must refresh to support p-nesting
          ]
      (if-let [times (get pdata id)]
        (if (>= (long (mt-count times)) nmax-times) ; Compact
          (let [m-id-stats (get pdata :__m-id-stats)
                id-stats   (get m-id-stats id)
                id-stats   (times->IdStats times id-stats)
                m-id-stats (assoc m-id-stats id id-stats)
                times      (mutable-times)]

            (mt-add times t-elapsed) ; Never leave empty
            (pdata-proxy (assoc pdata id times :__m-id-stats m-id-stats)))

          ;; Common case
          (mt-add times t-elapsed))

        ;; Init case
        (let [times (mutable-times)]
          (mt-add times t-elapsed)
          (pdata-proxy (assoc pdata id times))))))

  nil)

(defn- times->IdStats [times ?interim-id-stats]
  (let [times      (vec   times) ; Faster to reduce
        ts-count   (count times)
        _          (assert (not (zero? ts-count)))
        ts-time    (reduce (fn [^long acc ^long in] (+ acc in)) 0 times)
        ts-mean    (/ (double ts-time) (double ts-count))
        ts-mad-sum (reduce (fn [^long acc ^long in] (+ acc (Math/abs (- in ts-mean)))) 0 times)
        ts-min     (reduce (fn [^long acc ^long in] (if (< in acc) in acc)) enc/max-long times)
        ts-max     (reduce (fn [^long acc ^long in] (if (> in acc) in acc)) 0            times)]

    (if-let [^IdStats id-stats ?interim-id-stats] ; Merge over previous stats
      (let [s-count   (+ (.-count id-stats) ts-count)
            s-time    (+ (.-time  id-stats) ^long ts-time)
            s-mean    (/ (double s-time) (double s-count))
            s-mad-sum (+ (.mad-sum id-stats) ^long ts-mad-sum)
            s0-min    (.-min id-stats)
            s0-max    (.-max id-stats)]

        ;; Batched "online" MAD calculation here is better= the standard
        ;; Knuth/Welford method, Ref. http://goo.gl/QLSfOc,
        ;;                            http://goo.gl/mx5eSK.

        (IdStats. s-count s-time s-mean s-mad-sum
          (/ (double s-mad-sum) (double s-count))
          (if (< s0-min ^long ts-min) s0-min ts-min)
          (if (> s0-max ^long ts-max) s0-max ts-max)))

      (IdStats. ts-count ts-time ts-mean ts-mad-sum
        (/ (double ts-mad-sum) (double ts-count))
        ts-min
        ts-max))))

(comment (times->IdStats (mutable-times) nil)) ; Should throw

;;;;

(defn pdata->Stats
  "Wraps up a pdata run. Nb: recall that we need a *fresh* `(pdata-proxy)`
  here for thread-local profiling."
  [^PData current-pdata]
  (let [t1         (enc/now-nano*)
        t0         (.-__t0 current-pdata)
        m-id-stats (get    current-pdata :__m-id-stats)
        m-times    (dissoc current-pdata :__m-id-stats :__t0)]
    (Stats.
      (Clock. t0 t1 (- t1 t0))
      (reduce-kv
        (fn [m id times]
          (assoc m id (times->IdStats times (get m-id-stats id))))
        #_(transient {}) {} ; Usu. <10 entries
        m-times))))

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

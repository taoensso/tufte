(ns taoensso.tufte.telemere
  "Tufte -> Telemere handler,
  Ref. <https://www.taoensso.com/telemere>."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte          :as tufte]
   [taoensso.tufte.impl     :as impl]
   [taoensso.telemere       :as tel]))

(defn handler:telemere
  "Alpha, subject to change!
  Returns a signal handler that:
    - Takes a Tufte profiling signal (map).
    - Creates a corresponding Telemere signal for handling by Telemere.

  Options:
    `:level-fn` ----------- (fn [tufte-level]) => telemere-level (default to `identity`)
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)"

  ([] (handler:telemere nil))
  ([{:keys [level-fn format-pstats-opts]
     :or   {level-fn identity}}]

   (fn a-handler:telemere [signal]
     (when-let [{:keys [inst ns coords, id level, #?@(:clj [host thread]), ctx data,
                        pstats format-pstats-fn]} signal]
       (tel/signal!
         {:inst   inst
          :ns     ns
          :coords coords
          :kind   :tufte
          :id     id
          :level  (level-fn level)
          :ctx+   ctx
          :data   data
          :tufte/pstats pstats ; -> kvs
          :msg
          (when-let   [ff format-pstats-fn]
            (when-let [fs (ff pstats (conj {:incl-newline? false} format-pstats-opts))]
              (str enc/newline fs)))

          #?@(:clj [:host host, :thread thread])})))))

(comment
  ((handler:telemere)        (#'tufte/dummy-signal))
  ((handler:telemere) (assoc (#'tufte/dummy-signal) :data {:k1 :v1} :ctx {:k1 :v1})))

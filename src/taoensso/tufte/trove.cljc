(ns taoensso.tufte.trove
  "Tufte -> Trove handler,
  Ref. <https://www.taoensso.com/trove>."
  (:require
   [taoensso.encore :as enc]
   [taoensso.trove  :as trove]
   [taoensso.tufte  :as tufte]))

(defn handler:trove
  "Alpha, subject to change!
  Returns a signal handler that:
    - Takes a Tufte profiling signal (map).
    - Uses Trove to log the signal.

  Options:
    `:level-fn` ----------- (fn [tufte-level]) => trove-level (default `identity`)
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)"

  ([] (handler:trove nil))
  ([{:keys [level-fn format-pstats-opts]
     :or   {level-fn identity}}]

   (fn a-handler:trove [signal]
     (when-let [{:keys [#_inst ns coords, id level, ctx data,
                        pstats format-pstats-fn]} signal]
       (trove/log!
         {:ns     ns
          :coords coords
          :level  (level-fn level)
          :data   data
          :tufte/ctx    ctx    ; -> kvs
          :tufte/pstats pstats ; -> kvs
          :msg
          (when-let   [ff format-pstats-fn]
            (when-let [fs (ff pstats (conj {:incl-newline? false} format-pstats-opts))]
              (str "Tufte pstats:" enc/newline fs)))})))))

(comment
  ((handler:trove)        (#'tufte/dummy-signal))
  ((handler:trove) (assoc (#'tufte/dummy-signal) :data {:k1 :v1}, :ctx {:k1 :v1})))

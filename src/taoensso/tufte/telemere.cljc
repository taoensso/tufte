(ns taoensso.tufte.telemere
  "Handler for Telemere,
    Ref. <https://www.taoensso.com/telemere>."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte          :as tufte]
   [taoensso.tufte.impl     :as impl]
   [taoensso.telemere       :as tel]))

(defn handler:telemere
  "Alpha, subject to change.
  Returns a signal handler that:
    - Takes a Tufte profiling signal (map).
    - Creates a corresponding Telemere signal for handling by Telemere.

  The Telemere signal will include:
    As in Tufte signal: #{:inst :ns :coords :id :level :data :host :thread}
    And:
    `:kind` --------- `:tufte`
    `:msg_` --------- Delay of `pstats` formatted as string table
    `:ctx` ---------- (merge `telemere/*ctx*` `tufte/*ctx*`)
    `:tufte-signal` - Original Tufte signal (map)

  Options:
    `:telemere-level-fn` -- (fn [tufte-level]) => telemere-level (default to constantly `:info`)
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)"

  ([] (handler:telemere nil))
  ([{:keys [telemere-level-fn format-pstats-opts incl-keys]
     :or   {telemere-level-fn (fn [tufte-level] tufte-level)}}]

   (let []
     (fn a-handler:telemere [signal]
       (when-let [{:keys [inst ns coords, id level, #?@(:clj [host thread]), ctx data,
                          pstats format-pstats-fn]} signal]

         (taoensso.telemere/signal!
           {:inst     inst
            :ns       ns
            :coords   coords
            :kind     :tufte
            :id       id
            :level    (telemere-level-fn level)
            :data     data
            :tufte-signal signal ; Won't be printed by default (good)
            :msg      (when-let [ff format-pstats-fn] (delay (ff pstats format-pstats-opts)))
            :ctx+
            (fn [old]
              (if (or (nil? old) (map? old))
                (enc/merge-nx old ctx)
                (do           old)))

            #?@(:clj
                [:host   host
                 :thread thread])}))))))

(comment ((handler:telemere) (#'tufte/dummy-signal)))

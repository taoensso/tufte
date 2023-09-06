(ns taoensso.tufte.telemere
  "Handler for Telemere,
    Ref. <https://www.taoensso.com/telemere>."
  (:require
   [taoensso.encore     :as enc]
   [taoensso.tufte      :as tufte]
   [taoensso.tufte.impl :as impl]
   [taoensso.telemere   :as tel]))

(defn handler:telemere
  "Returns a handler fn for use with `add-handler!` that creates a Telemere signal with:
    `:kind`     - `:tufte`
    `:location` - Location of `profile` call
    `:id`       - Id       of `profile` call
    `:level`    - Level    of `profile` call (override with `:signal-level` opt)
    `:data`     - {:keys [sample-rate ctx data pstats pstats-str_ ...]} `profile` info
    `:msg_`     - pstats formatted as string table, may be a delay!

  Options:
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)
    `:telemere-level`     - Telemere signal level or fn of profiling level
                            (defaults to `profile` level)"

  {:added "vX.Y.Z (YYYY-MM-DD)"}
  ([] (handler:telemere nil))
  ([{:keys [telemere-level format-pstats-opts]
     :or   {telemere-level :info}}]

   (let [level-fn (impl/psig-level-fn telemere-level)]
     (fn a-handler:telemere [psig]
       (let [{:keys [inst location id level data ctx pstats format-pstats-fn]} psig]
         (taoensso.telemere/signal!
           {:kind     :tufte
            :location location
            :inst     inst
            :id       id
            :level    (level-fn level)
            :data     data #_psig
            :psig     psig ; Won't be printed by default (good)
            :msg      (delay
                        (str "\n<<< table <<<\n"
                          (let [ff (or format-pstats-fn impl/format-pstats)]
                            (ff pstats format-pstats-opts)) "\n>>> table >>>"))
            :ctx+
            (fn [old]
              (if (or (nil? old) (map? old))
                (enc/merge-nx old ctx)
                (do           old)))}))))))

(comment ((handler:telemere) (#'tufte/dummy-psig)))

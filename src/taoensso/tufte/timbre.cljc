(ns taoensso.tufte.timbre
  "Handler for Timbre,
    Ref. <https://www.taoensso.com/timbre>."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte          :as tufte]
   [taoensso.tufte.impl     :as impl]
   [taoensso.timbre         :as timbre]))

(defn handler:timbre
  "Returns a handler fn for use with `add-handler!` that:
    1. Formats `profile` pstats with `format-pstats`, and
    2. Logs the resulting string table with Timbre.

  Options:
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)
    `:timbre-level`       - Timbre level or fn of profiling level
                            (defaults to `profile` level)"

  {:added "vX.Y.Z (YYYY-MM-DD)"}
  ([] (handler:timbre nil))
  ([{:keys [timbre-level format-pstats-opts]
     :or   {timbre-level :info}}]

   (let [level-fn (impl/psig-level-fn timbre-level)]
     (fn a-handler:timbre [psig]
       (let [{:keys [inst location id level data ctx pstats format-pstats-fn]} psig]
         (timbre/log!
           {:loc      #_location {:ns (get location :ns), :line (get location :line)} ; TODO Needs Timbre >= v6.6.1
            :level    (level-fn level)
            :instant  #?(:clj (enc/as-dt inst), :cljs inst)
            :msg-type :p
            :vargs
            [(str
               "Tufte pstats "
               (when id (str (sigs/format-id (get location :ns) id) " ")) "-"
               (str "\n<<< table <<<\n" (let [ff (or format-pstats-fn impl/format-pstats)] (ff pstats format-pstats-opts)) "\n>>> table >>>")
               (when-let [data (enc/not-empty-coll data)] (str "\n data: " (enc/pr-edn* data)))
               (when-let [ctx  (enc/not-empty-coll ctx)]  (str "\n  ctx: " (enc/pr-edn* ctx))))]}))))))

(comment ((handler:timbre) (#'tufte/dummy-psig)))

(enc/deprecated
  (defn ^:no-doc add-timbre-logging-handler!
    "Prefer (add-handler! <handler-id> (handler:timbre) <dispatch-opts>)."
    {:deprecated "vX.Y.Z (YYYY-MM-DD)"}
    [{:keys [timbre-level ns-pattern handler-id]
      :or   {ns-pattern "*"
             handler-id :timbre}}]

    (let [handler-fn (handler:timbre {:timbre-level timbre-level})]
      ^:deprecation-nowarn (tufte/add-legacy-handler! handler-id ns-pattern handler-fn))))

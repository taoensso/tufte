(ns taoensso.tufte.timbre
  "Simple logging handler for integration with Timbre."
  (:require
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]
   [taoensso.tufte  :as tufte]
   [taoensso.tufte.impl :as impl
    #?@(:cljs [:refer [PStats ProfilingSignal]])])
  #?(:clj (:import [taoensso.tufte.impl PStats ProfilingSignal])))

(defn timbre-handler
  "Returns a simple handler fn for use with `add-handler!` that:
    1. Formats `profile` pstats with `format-pstats`, and
    2. Logs the resulting string table with Timbre.

  Options:
    `:format-pstats-opts` - Opts map provided to `format-pstats`
    `:timbre-level`       - Timbre level, or ifn to map profiling->Timbre level"

  {:added "vX.Y.Z (YYYY-MM-DD)"}
  ([] (timbre-handler nil))
  ([{:keys [timbre-level format-pstats-opts]
     :or   {timbre-level :info}}]

   (fn timbre-handler [^ProfilingSignal ps]
     (timbre/log!
       {:loc   (.-loc ps)
        :id    (.-id  ps)
        :data         ps
        :level  (impl/signal-level ps timbre-level)
        :vargs [(impl/signal-msg   ps format-pstats-opts)]}))))

(enc/deprecated
  (defn ^:no-doc add-timbre-logging-handler!
    "Prefer (add-handler! <handler-id> (timbre-handler) <dispatch-opts>)."
    {:deprecated "vX.Y.Z (YYYY-MM-DD)"}
    [{:keys [timbre-level ns-pattern handler-id]
      :or   {ns-pattern "*"
             handler-id :timbre}}]

    (let [handler-fn (timbre-handler {:timbre-level timbre-level})]
      (tufte/add-legacy-handler! handler-id ns-pattern handler-fn))))

(ns taoensso.tufte.timbre
  "Simple logging handler for integration with Timbre."
  (:require
            [taoensso.encore :as enc]
            [taoensso.tufte  :as tufte]
   #?(:clj  [taoensso.timbre :as timbre :refer        [log!]]
      :cljs [taoensso.timbre :as timbre :refer-macros [log!]])))

(defn add-timbre-logging-handler!
  "Adds a simple handler that logs `profile` stats output with Timbre.

  `timbre-level` may be a fixed Timbre level (e.g. :info), or a
  (fn [tufte-level]) -> timbre-level, e.g. {0 :trace 1 :debug ...}."
  [{:keys [timbre-level ns-pattern handler-id]
    :or   {timbre-level :info
           ns-pattern "*"
           handler-id :timbre}}]

  (tufte/add-handler! handler-id ns-pattern
    (fn [m]
      (let [{:keys [ns-str level ?id ?data pstats pstats-str_ ?file ?line]} m
            profile-opts (enc/assoc-some {:level level} :id ?id :data ?data)
            timbre-level
            (cond
              (keyword? timbre-level) timbre-level
              (ifn? timbre-level) (timbre-level level)
              :else timbre-level)]

        (log! timbre-level :p
          [(str "Tufte `profile` output " profile-opts ":\n\n" @pstats-str_ "\n")]
          {:?ns-str ns-str :?file ?file :?line ?line})))))

(comment (add-timbre-logging-handler! {}))

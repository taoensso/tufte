(ns taoensso.tufte.timbre
  "Simple logging handler for integration with Timbre."
  (:require
          [taoensso.encore :as enc]
          [taoensso.tufte  :as tufte]
   #+clj  [taoensso.timbre :as timbre :refer        (log!)]
   #+cljs [taoensso.timbre :as timbre :refer-macros (log!)]))

(defn add-timbre-logging-handler!
  "Adds a simple handler that logs `profile` stats output with Timbre."
  [{:keys [timbre-level ns-pattern handler-id]
    :or   {timbre-level :info
           ns-pattern "*"
           handler-id :timbre}}]

  (tufte/add-handler! handler-id ns-pattern
    (fn [m]
      (let [{:keys [ns-str level ?id ?data stats stats-str_ ?file ?line]} m
            stats-str (force stats-str_)
            profile-opts (enc/assoc-some {:level level} :id ?id :data ?data)]

        (log! timbre-level :p
          [(str "Tufte `profile` ouput " profile-opts ":\n\n" stats-str "\n")]
          {:?ns-str ns-str :?file ?file :?line ?line})))))

(comment (add-timbre-logging-handler! {}))

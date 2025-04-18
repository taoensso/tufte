(ns example.server
  "Quick Tufte example showing how to use Tufte for
  simple ongoing application performance monitoring.

  Note that a similar pattern can also be used for Cljs apps.
  See the Tufte README for more info!"
  {:author "Peter Taoussanis (@ptaoussanis)"}
  (:require
   [clojure.string     :as str]
   [ring.middleware.defaults]
   [ring.adapter.jetty]
   [compojure.core     :as comp :refer [defroutes GET POST]]
   [compojure.route    :as route]
   [hiccup.core        :as hiccup]
   [taoensso.encore    :as enc :refer [have have?]]
   [taoensso.tufte     :as tufte]))

(enc/defonce stats-accumulator
  "Accumulates results from all unfiltered `profile` calls.
  Deref this to get the accumulated data."
  (tufte/stats-accumulator))

;; Register handler for `profile` results
(tufte/add-handler! :my-accumulating-handler
  (tufte/handler:accumulating stats-accumulator))

(defroutes ring-routes
  (GET "/" ring-req
    (fn [ring-req]
      (hiccup/html
        [:h1 "Welcome!"]
        [:p  "Routes you can try:"]
        [:ul
         [:li "/sleep/:endpoint-id/:msecs - to sleep for given time (will be recorded), e.g.:"]
         [:li [:a {:href "/sleep/foo/500"}      [:strong "/sleep/foo/500"]]]
         [:li [:a {:href "/sleep/foo/1000"}     [:strong "/sleep/foo/1000"]]]
         [:li [:a {:href "/sleep/foo/1200"}     [:strong "/sleep/foo/1200"]]]
         [:li [:a {:href "/performance-report"} [:strong "/performance-report"]] " - to show recorded performance info"]])))

  (GET  "/sleep/:id/:msecs" ring-req
    (fn [ring-req]
      (let [{:keys [params]} ring-req]
        (tufte/profile {:id (:id params)}
          (when-let [msecs (enc/as-?int (:msecs params))]
            (tufte/p :sleep (Thread/sleep msecs))
            (hiccup/html
              [:p (str "Done! (After sleeping " msecs " msecs).")]
              [:p (str "The performance statistics for this request were recorded.")]
              [:p [:a {:href "/"} "Go back"]]
              [:p "Tip: for your real application, you'll want to have `tufte/p` wrapping your interesting/expensive forms. You could then automatically add `tufte/profile` to all your endpoints using a Ring middleware, etc."]))))))

  (GET "/performance-report" ring-req
    (fn [ring-req]
      (hiccup/html
        [:h1 "Performance report since last call:"]
        [:hr]
        [:pre
         (if-let [m (not-empty @stats-accumulator)]
           (tufte/format-grouped-pstats m)
           "Nothing (try visit the /sleep/... endpoints)")]
        [:hr]
        [:p [:a {:href "/"} "Go back"]]
        [:p "Tip: instead of having an endpoint like this, your real application could also schedule a report to be auto-generated every `n` minutes and data logged to your DB, etc. This'd also be a good opportunity to trigger alarms if any concerning performance numbers are detected."])))

  (route/not-found "<h1>Page not found</h1>"))

(def main-ring-handler
  (ring.middleware.defaults/wrap-defaults
    ring-routes ring.middleware.defaults/site-defaults))

(defn -main "For `lein run`, etc." []
  (println "Starting example Ring server...")
  (ring.adapter.jetty/run-jetty #'main-ring-handler {:port 8088 :join? false})

  ;; Cleanly stop handlers on shutdown
  (tufte/call-on-shutdown!
    (fn [] (tufte/stop-handlers!)))

  (let [uri "http://localhost:8088/"]
    (println (str "Jetty running on: " uri))
    (try
      (.browse (java.awt.Desktop/getDesktop) (java.net.URI. uri))
      (catch java.awt.HeadlessException _))))

(comment (-main))

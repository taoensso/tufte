(defproject com.taoensso.examples/tufte "2.6.1"
  :description "Tufte example web-app project"
  :url "https://github.com/ptaoussanis/tufte"

  :license
  {:name "Eclipse Public License"
   :url  "http://www.eclipse.org/legal/epl-v10.html"
   :distribution :repo
   :comments "Same as Clojure"}

  :plugins [[lein-ancient "0.7.0"]]
  :dependencies
  [[org.clojure/clojure "1.11.1"]
   [ring                "1.9.6"]
   [ring/ring-defaults  "0.3.4"]
   [compojure           "1.7.0"]
   [hiccup              "1.0.5"]
   [com.taoensso/tufte  "2.6.1"]]

  :main example.server)

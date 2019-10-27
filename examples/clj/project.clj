(defproject com.taoensso.examples/tufte "2.1.0-RC5"
  :description "Tufte example web-app project"
  :url "https://github.com/ptaoussanis/tufte"

  :license
  {:name "Eclipse Public License"
   :url  "http://www.eclipse.org/legal/epl-v10.html"
   :distribution :repo
   :comments "Same as Clojure"}

  :plugins [[lein-ancient "0.6.15"]]
  :dependencies
  [[org.clojure/clojure "1.10.1"]
   [ring                "1.8.1"]
   [ring/ring-defaults  "0.3.2"]
   [compojure           "1.6.2"]
   [hiccup              "1.0.5"]
   [com.taoensso/tufte  "2.1.0"]]

  :main example.server)

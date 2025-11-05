(defproject com.taoensso.examples/tufte "3.0.0"
  :description "Tufte example web-app project"
  :url "https://github.com/ptaoussanis/tufte"
  :main    example.server
  :plugins [[lein-ancient "0.7.0"]]
  :dependencies
  [[org.clojure/clojure "1.12.4"]
   [ring                "1.15.3"]
   [ring/ring-defaults  "0.7.0"]
   [ring/ring-codec     "1.3.0"]
   [compojure           "1.7.2"]
   [hiccup              "1.0.5"]
   [com.taoensso/tufte  "3.0.0"]])

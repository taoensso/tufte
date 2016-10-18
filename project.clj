(defproject com.taoensso/tufte "1.0.2"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Simple profiling and performance monitoring for Clojure/Script"
  :url "https://github.com/ptaoussanis/tufte"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same as Clojure"}
  :min-lein-version "2.3.3"
  :global-vars {*warn-on-reflection* true
                *assert*             true}

  :dependencies
  [[org.clojure/clojure "1.7.0"]
   [com.taoensso/encore "2.86.1"]]

  :plugins
  [[lein-pprint    "1.1.2"]
   [lein-ancient   "0.6.10"]
   [lein-codox     "0.10.1"]
   [lein-cljsbuild "1.1.4"]]

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :server-jvm {:jvm-opts ^:replace ["-server"]}
   :1.8      {:dependencies [[org.clojure/clojure "1.8.0"]]}
   :1.9      {:dependencies [[org.clojure/clojure "1.9.0-alpha13"]]}
   :test     {:dependencies [[org.clojure/test.check "0.9.0"]]}
   :provided {:dependencies [[org.clojure/clojurescript "1.9.229"]]}
   :dev
   [:1.9 :test :server-jvm
    {:dependencies [[com.taoensso/timbre "4.7.4"]]}]}

  :cljsbuild
  {:test-commands
   {"node"    ["node" :node-runner "target/main.js"]
    "phantom" ["phantomjs" :runner "target/main.js"]}

   :builds
   [{:id :main
     :source-paths ["src" "test"]
     :compiler
     {:output-to "target/main.js"
      :optimizations :advanced
      :pretty-print false}}]}

  :aliases
  {"build-once" ["cljsbuild" "once"]
   "deploy-lib" ["do" "build-once," "deploy" "clojars," "install"]
   "start-dev"  ["with-profile" "+dev" "repl" ":headless"]}

  :repositories
  {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"})

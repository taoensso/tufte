(defproject com.taoensso/tufte "3.0.0"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Simple performance monitoring library for Clojure/Script"
  :url "https://github.com/taoensso/tufte"

  :license
  {:name "Eclipse Public License - v 1.0"
   :url  "https://www.eclipse.org/legal/epl-v10.html"}

  :test-paths ["test" #_"src"]

  :dependencies
  [[com.taoensso/encore "3.148.0"]]

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :provided {:dependencies [[org.clojure/clojurescript "1.12.42"]
                             [org.clojure/clojure       "1.12.1"]]}
   :c1.12    {:dependencies [[org.clojure/clojure       "1.12.1"]]}
   :c1.11    {:dependencies [[org.clojure/clojure       "1.11.4"]]}
   :c1.10    {:dependencies [[org.clojure/clojure       "1.10.3"]]}

   :graal-tests
   {:source-paths ["test"]
    :main taoensso.graal-tests
    :aot [taoensso.graal-tests]
    :uberjar-name "graal-tests.jar"
    :dependencies
    [[org.clojure/clojure                  "1.11.1"]
     [com.github.clj-easy/graal-build-time "1.0.5"]]}

   :dev
   {:jvm-opts ["-server" "-Dtaoensso.elide-deprecated=true"]
    :global-vars
    {*warn-on-reflection* true
     *assert*             true
     *unchecked-math*     false #_:warn-on-boxed}

    :dependencies
    [[org.clojure/test.check "1.1.1"]
     [com.taoensso/trove     "1.0.0-beta1"]
     [com.taoensso/telemere  "1.0.1"]
     [com.taoensso/timbre    "6.7.1"]]

    :plugins
    [[lein-pprint    "1.3.2"]
     [lein-ancient   "0.7.0"]
     [lein-cljsbuild "1.1.8"]]}}

  :cljsbuild
  {:test-commands {"node" ["node" "target/test.js"]}
   :builds
   [{:id :main
     :source-paths ["src"]
     :compiler
     {:output-to "target/main.js"
      :optimizations :advanced}}

    {:id :test
     :source-paths ["src" "test"]
     :compiler
     {:output-to "target/test.js"
      :target :nodejs
      :optimizations :simple}}]}

  :aliases
  {"start-dev"  ["with-profile" "+dev" "repl" ":headless"]
   "build-once" ["do" ["clean"] ["cljsbuild" "once"]]
   "deploy-lib" ["do" ["build-once"] ["deploy" "clojars"] ["install"]]

   "test-clj"   ["with-profile" "+c1.12:+c1.11:+c1.10" "test"]
   "test-cljs"  ["with-profile" "+c1.12" "cljsbuild"   "test"]
   "test-all"   ["do" ["clean"] ["test-clj"] ["test-cljs"]]})

(defproject com.taoensso/tufte "3.0.2"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Simple performance monitoring library for Clojure/Script"
  :url "https://github.com/taoensso/tufte"

  :license
  {:name "Eclipse Public License - v 1.0"
   :url  "https://www.eclipse.org/legal/epl-v10.html"}

  :test-paths ["test" #_"src"]

  :dependencies
  [[com.taoensso/encore "3.171.1"]]

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :provided {:dependencies [[org.clojure/clojurescript "1.12.145"]
                             [org.clojure/clojure       "1.12.5"]]}
   :c1.13    {:dependencies [[org.clojure/clojure       "1.13.0-alpha6"]]}
   :c1.12    {:dependencies [[org.clojure/clojure       "1.12.5"]]}
   :c1.11    {:dependencies [[org.clojure/clojure       "1.11.4"]]}
   :c1.10    {:dependencies [[org.clojure/clojure       "1.10.3"]]}

   :incl-deprecated {:jvm-opts ["-Dtaoensso.elide-deprecated=false"]}

   :graal-tests
   {:source-paths ["test"]
    :main taoensso.graal-tests
    :aot [taoensso.graal-tests]
    :uberjar-name "graal-tests.jar"
    :dependencies
    [[org.clojure/clojure                  "1.11.4"]
     [com.github.clj-easy/graal-build-time "1.0.6"]]}

   :dev
   {:jvm-opts ["-server" "-Dtaoensso.elide-deprecated=true"]
    :global-vars
    {*warn-on-reflection* true
     *assert*             true
     *unchecked-math*     false #_:warn-on-boxed}

    :dependencies
    [[org.clojure/test.check "1.1.3"]
     [com.taoensso/trove     "1.2.0"]
     [com.taoensso/telemere  "1.4.0"]
     [com.taoensso/timbre    "6.8.0"]]

    :plugins
    [[lein-pprint    "1.3.2"]
     [lein-ancient   "1.0.0"]
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

   "test-clj"        ["with-profile" "+c1.13:+c1.12:+c1.11:+c1.10" "test"]
   "test-clj-java8"  ["with-profile" "+c1.12:+c1.11:+c1.10"        "test"]
   "test-deprecated" ["with-profile" "+incl-deprecated"            "test"]
   "test-cljs"       ["with-profile" "+c1.12" "cljsbuild" "test"]
   "test-all"        ["do" ["clean"] ["test-clj"] ["test-cljs"]]})

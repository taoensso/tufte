(defproject com.taoensso/tufte "2.5.0"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Simple profiling and performance monitoring for Clojure/Script"
  :url "https://github.com/ptaoussanis/tufte"
  :min-lein-version "2.3.3"

  :license
  {:name "Eclipse Public License 1.0"
   :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :global-vars
  {*warn-on-reflection* true
   *assert*             true
   *unchecked-math*     false #_:warn-on-boxed}

  :dependencies
  [[com.taoensso/encore "3.62.0"]]

  :plugins
  [[lein-pprint    "1.3.2"]
   [lein-ancient   "0.7.0"]
   [lein-cljsbuild "1.1.8"]
   [com.taoensso.forks/lein-codox "0.10.9"]]

  :codox
  {:language #{:clojure :clojurescript}
   :base-language :clojure}

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :dev        [:c1.11 :test :server-jvm :depr]
   :depr       {:jvm-opts ["-Dtaoensso.elide-deprecated=true"]}
   :server-jvm {:jvm-opts ^:replace ["-server"]}

   :provided {:dependencies [[org.clojure/clojurescript "1.11.60"]
                             [org.clojure/clojure       "1.11.1"]]}
   :c1.11    {:dependencies [[org.clojure/clojure       "1.11.1"]]}
   :c1.10    {:dependencies [[org.clojure/clojure       "1.10.1"]]}
   :c1.9     {:dependencies [[org.clojure/clojure       "1.9.0"]]}
   :test     {:dependencies [[org.clojure/test.check    "1.1.1"]
                             [com.taoensso/timbre       "6.2.1"]]}

   :graal-tests
   {:dependencies [[org.clojure/clojure "1.11.1"]
                   [com.github.clj-easy/graal-build-time "0.1.4"]]
    :main taoensso.graal-tests
    :aot [taoensso.graal-tests]
    :uberjar-name "graal-tests.jar"}}

  :test-paths ["test" #_"src"]

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
   "build-once" ["do" ["clean"] "cljsbuild" "once"]
   "deploy-lib" ["do" ["build-once"] ["deploy" "clojars"] ["install"]]

   "test-clj"   ["with-profile" "+c1.11:+c1.10:+c1.9" "test"]
   "test-cljs"  ["with-profile" "+test" "cljsbuild"   "test"]
   "test-all"   ["do" ["clean"] "test-clj," "test-cljs"]}

  :repositories
  {"sonatype-oss-public"
   "https://oss.sonatype.org/content/groups/public/"})

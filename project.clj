(defproject machine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  ;; 2021-12-03 See deps.edn for the real deps. We are migrating away from Leiningen.
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.namespace "0.2.11"]]

  ;; :main ^:skip-aot machine.core
  :main ^:skip-aot coordisc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

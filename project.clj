(defproject graphql "0.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.11"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [reduce-fsm "0.1.3"]]
  :main ^:skip-aot graphql.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

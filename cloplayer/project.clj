(defproject cloplayer "0.1.0-SNAPSHOT"
  :description "Clojure Apache log file replayer"
  :url "http://github.com/jimm/clojure/cloplayer"
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot cloplayer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

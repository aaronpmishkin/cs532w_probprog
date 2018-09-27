(defproject foppl-compiler "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [anglican "1.0.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

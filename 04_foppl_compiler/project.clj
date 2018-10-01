; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(defproject foppl-compiler "0.1.0-SNAPSHOT"
  :dependencies [[proto-repl-charts "0.3.1"]
                 [proto-repl "0.3.1"]
                 [org.clojure/clojure "1.8.0"]
                 [anglican "1.0.0"]]
  :target-path "target/%s"
  :source-paths ["src"]
  :profiles {:uberjar {:aot :all}})

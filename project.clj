(defproject adventofcode "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [refactor-nrepl "1.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/tools.cli "0.3.5"]]

  ; :main ^:skip-aot adventofcode.core

  :main adventofcode.day7
  :target-path "target/%s"
  :plugins [[cider/cider-nrepl "0.14.0-snapshot"]
            [refactor-nrepl "1.1.0"]]
  :profiles {:uberjar {:aot :all}}
  :resource-paths ["shared" "resources"])

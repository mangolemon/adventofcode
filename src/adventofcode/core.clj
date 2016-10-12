(ns adventofcode.core
  ;(:gen-class)
  (require [clojure.string :as str :refer [split-lines]]))

;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))


(defn get-lines [file]
  (str/split-lines (slurp file)))

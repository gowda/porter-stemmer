(ns porter-stemmer.core
  (:require [clojure.string :as string])
  (:use [porter-stemmer.steps :only [stem-it]]
        [clojure.tools.cli :only [cli]])
  (:gen-class))

(def options [["-h" "--help" "Display this message" :default false :flag true]])

(defn -main [& args]
  (let [[options-map words banner] (apply (partial cli args) options)]
    (cond
     (:help options-map) (println banner)
     (> (count words) 0) (doseq [word words]
                              (println word " ==> " (stem-it word)))
     :else (println banner))))

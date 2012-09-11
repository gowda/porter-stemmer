(ns porter-stemmer.core
  (:require [clojure.string :as string])
  (:use [porter-stemmer.steps :only [stem-it]]))

(defn find-differential [words-file output-file]
  (let [words (string/split (slurp words-file) #"\n")
        outputs (string/split (slurp output-file) #"\n")
        gowtputs (map stem-it words)]
    (reduce (fn [res [word gow porter]]
              (if (= gow porter)
                res
                (assoc res word [gow porter])))
            {}
            (map (fn [w g p] [w g p]) words gowtputs outputs))))

(defn conflate [strings]
  (let [output (map stem-it strings)]
    (reduce (fn [res [word stem]]
              (assoc res stem (conj (get res stem []) word)))
            {}
            (zipmap strings output))))

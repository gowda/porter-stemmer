(ns porter-stemmer.core
  (:use [porter-stemmer.steps :only [stem-it]]))

(defn conflate [strings]
  (let [output (map stem-it strings)]
    (reduce (fn [res [word stem]]
              (assoc res stem (conj (get res stem []) word)))
            {}
            (zipmap strings output))))

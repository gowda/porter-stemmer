(ns porter-stemmer.test.steps
  (:use [porter-stemmer.steps]
        [clojure.test]))

(deftest test-step-1a
  (is (every? (fn [[word stem]]
                (= (step-1a word) stem))
              {"caresses" "caress", "ponies" "poni", "ties" "ti",
               "caress" "caress", "cats" "cat"})))

(deftest test-step-1b
  (is (every? (fn [[word stem]]
                (= (step-1b word) stem))
              {"feed" "feed", "agreed" "agree", "plastered" "plaster",
               "bled" "bled", "motoring" "motor", "sing" "sing"
               "conflated" "conflate", "troubled" "trouble", "sized" "size"
               "hopping" "hop", "tanned" "tan", "falling" "fall",
               "hissing" "hiss", "failing" "fail", "filing" "file"})))

(deftest test-step-1c
  (is (every? (fn [[word stem]]
                (= (step-1c word) stem))
              {"happy" "happi", "sky" "sky"})))

(defn stem-checker [f]
  (fn [[word stem]]
    (= (f word) stem)))

(deftest test-step-2
  (is (every? (stem-checker step-2)
              {"relational" "relate", "conditional" "condition",
               "rational" "rational", "valenci" "valence",
               "hesitanci" "hesitance", "digitizer" "digitize",
               "conformabli" "conformable", "radicalli" "radical",
               "differentli" "different", "vileli" "vile",
               "analogousli" "analogous", "vietnamization" "vietnamize",
               "predication" "predicate", "operator" "operate",
               "feudalism" "feudal", "decisiveness" "decisive",
               "hopefulness" "hopeful", "callousness" "callous",
               "formaliti" "formal", "sensitiviti" "sensitive",
               "sensibiliti" "sensible"})))

(deftest test-step-3
  (is (every? (stem-checker step-3)
              {"triplicate" "triplic", "formative" "form"
               "formalize" "formal", "electriciti" "electric"
               "electrical" "electric", "hopeful" "hope"
               "goodness" "good"})))

(deftest test-step-4
  (is (every? (stem-checker step-4)
              {"revival" "reviv", "allowance" "allow",
               "inference" "infer", "airliner" "airlin"
               "gyroscopic" "gyroscop", "adjustable" "adjust",
               "defensible" "defens", "irritant" "irrit",
               "replacement" "replac", "adjustment" "adjust",
               "dependent" "depend", "adoption" "adopt",
               "homologou" "homolog", "communism" "commun",
               "activate" "activ", "angulariti" "angular",
               "homologous" "homolog", "effective" "effect",
               "bowlderize" "bowlder"})))

(deftest test-step-5a
  (is (every? (stem-checker step-5a)
              {"probate" "probat", "rate" "rate", "cease" "ceas"})))

(deftest test-step-5b
  (is (every? (stem-checker step-5b)
              {"controll" "control", "roll" "roll"})))


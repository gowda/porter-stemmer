(ns porter-stemmer.test.utils
  (:use [porter-stemmer.utils] [clojure.test]))

(deftest test-string->charvec
  (let [s "string"]
    (is (= (count s)
           (count (string->charvec s)))))
  (let [s ""]
    (is (= (count s)
           (count (string->charvec s))))))

(deftest test-vowel?
  (let [s "string"]
    (is (= (map vowel? (string->charvec s))
           '(false false false true false false))))
  (let [s "sky"]
    (is (= (map vowel? (string->charvec s))
           '(false false true))))
  (let [s "toy"]
    (is (= (map vowel? (string->charvec s))
           '(false true false)))))

(deftest test-consonant?
  (let [s "string"]
    (is (= (map consonant? (string->charvec s))
           '(true true true false true true))))
  (let [s "sky"]
    (is (= (map consonant? (string->charvec s))
           '(true true false))))
  (let [s "toy"]
    (is (= (map consonant? (string->charvec s))
           '(true false true)))))

(deftest test-cvc-count
  (is (every? #(= 0 (cvc-count %)) ["tr" "ee" "tree" "y" "by"]))
  (is (every? #(= 1 (cvc-count %)) ["trouble" "oats" "trees" "ivy"]))
  (is (every? #(= 2 (cvc-count %)) ["private" "private" "oaten" "orrery"])))

(deftest test-contains-vowel?
  (is (true? (contains-vowel? "plaster")))
  (is (false? (contains-vowel? "bl")))
  (is (true? (contains-vowel? "happ")))
  (is (false? (contains-vowel? "sk"))))

(deftest test-double-consonant-end?
  (is (true? (double-consonant-end? "hopp")))
  (is (true? (double-consonant-end? "tann")))
  (is (true? (double-consonant-end? "fall")))
  (is (true? (double-consonant-end? "hiss")))
  (is (true? (double-consonant-end? "fizz"))))

(deftest test-cvc-end?
  (is (false? (cvc-end? "fail")))
  (is (true? (cvc-end? "fil")))
  (is (false? (cvc-end? "ceas"))))

(deftest test-ends?
  (is (false? (ends? "al" "ational")))
  (is (ends? "caresses" "sses"))
  (is (ends? "ponies" "ies"))
  (is (ends? "ties" "ies"))
  (is (ends? "caress" "ss"))
  (is (ends? "cats" "s"))
  (is (ends? "feed" "eed"))
  (is (ends? "agreed" "eed"))
  (is (ends? "plastered" "ed"))
  (is (ends? "bled" "ed"))
  (is (ends? "motoring" "ing"))
  (is (ends? "sing" "ing"))
  (is (ends? "conflat" "at"))
  (is (ends? "troubl" "bl"))
  (is (ends? "siz" "iz"))
  (is (ends? "relational" "ational"))
  (is (ends? "conditional" "tional"))
  (is (ends? "rational" "tional"))
  (is (ends? "valenci" "enci"))
  (is (ends? "histanci" "anci"))
  (is (ends? "digitizer" "izer"))
  (is (ends? "conformabli" "abli"))
  (is (ends? "radicalli" "alli"))
  (is (ends? "differentli" "entli"))
  (is (ends? "vileli" "eli"))
  (is (ends? "analogousli" "ousli"))
  (is (ends? "vietnamization" "ization"))
  (is (ends? "predication" "ation"))
  (is (ends? "operator" "ator"))
  (is (ends? "feudalism" "alism"))
  (is (ends? "decisiveness" "iveness"))
  (is (ends? "hopefulness" "fulness"))
  (is (ends? "callousness" "ousness"))
  (is (ends? "formaliti" "aliti"))
  (is (ends? "sensitiviti" "iviti"))
  (is (ends? "sensibiliti" "biliti"))
  (is (ends? "triplicate" "icate"))
  (is (ends? "formalize" "alize"))
  (is (ends? "electriciti" "iciti"))
  (is (ends? "electrical" "ical"))
  (is (ends? "hopeful" "ful"))
  (is (ends? "goodness" "ness"))
  (is (ends? "revival" "al"))
  (is (ends? "allowance" "ance"))
  (is (ends? "inference" "ence"))
  (is (ends? "airliner" "er"))
  (is (ends? "gyropscopic" "ic"))
  (is (ends? "adjustable" "able"))
  (is (ends? "defensible" "ible"))
  (is (ends? "irritant" "ant"))
  (is (ends? "replacement" "ement"))
  (is (ends? "dependent" "ent"))
  (is (ends? "adoption" "ion"))
  (is (ends? "homologou" "ou"))
  (is (ends? "communism" "ism"))
  (is (ends? "activate" "ate"))
  (is (ends? "angulariti" "iti"))
  (is (ends? "homologous" "ous"))
  (is (ends? "effective" "ive"))
  (is (ends? "bowdlerize" "ize"))
  (is (ends? "probate" "e"))
  (is (ends? "rate" "e"))
  (is (ends? "cease" "e"))
  (is (ends? "controll" "l"))
  (is (ends? "roll" "l")))

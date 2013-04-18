(ns porter-stemmer.utils)

;;;
;;; helper procedures
;;;

(defn ends?
  "True if string @s ends with string @end."
  [s end]
  (every? true? (map = (reverse s) (reverse end))))

(defn stem
  "Return string @s without the end @end, only if @s actually ends with @end."
  [s end]
  (if (ends? s end)
    (apply str (subvec (into [] s) 0 (- (count s) (count end))))
    s))

(def proper-vowel? #{\a \e \i \o \u})
(def not-a-proper-vowel? (complement proper-vowel?))

(defn consonant? [[preceding-c c]]
  (-> (if (= c \y)
        (proper-vowel? preceding-c)
        (not-a-proper-vowel? c))
      boolean))

(def vowel? (complement consonant?))

(defn string->charvec [s]
  (->> s
       (map identity)
       (concat [nil])
       (partition 2 1)))

(defn cvc-map [s]
  (->> s
       string->charvec
       (map consonant?)
       (reduce (fn [result v]
                 (if (= (last result) v)
                   result
                   (conj result v)))
               [])))

(defn conditional-rest [predicate? coll]
  (if (predicate? (first coll))
    (rest coll)
    coll))

(defn conditional-butlast [predicate? coll]
  (if (predicate? (last coll))
    (rest coll)
    coll))

(def without-first-true (partial conditional-rest true?))
(def without-last-false (partial conditional-butlast false?))

(defn cvc-count
  "Count VC patterns after shrinking the string @s to the form [C](VC){m}[V]"
  [s]
  (->> s
       cvc-map
       without-first-true
       without-last-false
       (partition 2)
       count))

(defn sandwiched-vowel?
  "True if a minimum of one vowel is present inside of string, borders
   excluded."
  [s]
  (->> s cvc-map rest butlast (some false?) boolean))

(defn second-last [coll]
  (-> coll butlast last))

(defn third-last [coll]
  (-> coll butlast butlast last))

(defn double-consonant-end?
  "True if string ends with same letter and the letter is consonant."
  [s]
  (and (= (last s)
          (second-last s))
       (consonant? (-> s string->charvec last))))

(defn cvc-end?
  "True if string ends with <consonant><vowel><consonant>."
  [s]
  (and (consonant? (-> s string->charvec third-last))
       (vowel? (-> s string->charvec second-last))
       (and (consonant? (-> s string->charvec last))
            (not (#{\w \x \y} (last s))))))


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

(defn without-first-true [coll]
  (if (true? (first coll))
    (rest coll)
    coll))

(defn without-last-false [coll]
  (if (false? (last coll))
    (butlast coll)
    coll))

(defn cvc-count
  "Count VC patterns after shrinking the string @s to the form [C](VC){m}[V]"
  [s]
  (->> s
       cvc-map
       without-first-true
       without-last-false
       (partition 2)
       count))

(defn sandwiched-v?
  "True if a minimum of one vowel is present inside of string, borders
   excluded."
  [s]
  (some vowel? (butlast (rest s))))

(defn n-sandwiched-v?
  "True if a minimum of one vowel is present inside of string, borders
   excluded."
  [s]
  (some vowel? (string->charvec s)))

(defn double-c?
  "True if string ends with same letter and the letter is consonant."
  [s]
  (and (consonant? (last s))
       (= (last s) (last (butlast s)))))

(defn n-double-c?
  "True if string ends with same letter and the letter is consonant."
  [s]
  (let [cvec (string->charvec s)]
    (and (consonant? (last cvec))
         (= (last s) (last (butlast s))))))

(defn ends-cvc?
  "True if string ends with <consonant><vowel><consonant>."
  [s]
  (and (>= (count s) 3)
       (and (consonant? (last (butlast (butlast s))))
            (vowel? (last (butlast s)))
            (consonant? (last s))
            (not (#{\w \x \y} (last s))))))

(defn n-ends-cvc?
  "True if string ends with <consonant><vowel><consonant>."
  [s]
  (let [cvec (string->charvec s)]
    (and (>= (count s) 3)
         (and (consonant? (last (butlast (butlast cvec))))
              (vowel? (last (butlast cvec)))
              (consonant? (last cvec))
              (not (#{\w \x \y} (second (last cvec))))))))


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

(defn n-vowel? [char-vec]
  (or (#{\a \e \i \o \u} (second char-vec))
      (and (not (or (#{\a \e \i \o \u} (first char-vec))
                    (nil? (first char-vec))))
           (= (second char-vec) \y))))

(def n-consonant? (complement n-vowel?))

(def vowel? #{\a \e \i \o \u})
(def consonant? (complement vowel?))

(defn cvc-count
  "Count VC patterns after shrinking the string to the form [C](VC){m}[V]."
  [s]
  (let [cvc-map (reduce #(if (= (last %1) %2)
                           %1
                           (conj %1 %2))
                        []
                        (keep consonant? s))
        vcvc-map (if (= (first cvc-map) true)
                   (rest cvc-map)
                   cvc-map)]
    (count (partition 2 vcvc-map))))

(defn str->charvec [s]
  (map (fn [x y] [x y]) (into [nil] s) s))

(defn n-cvc-count
  "Count VC patterns after shrinking the string to the form [C](VC){m}[V]."
  [s]
  (let [cvc-map (reduce #(if (= (last %1) %2)
                           %1
                           (conj %1 %2))
                        []
                        (keep n-consonant? (str->charvec s)))
        vcvc-map (if (= (first cvc-map) true)
                   (rest cvc-map)
                   cvc-map)]
    (count (partition 2 vcvc-map))))

(defn sandwiched-v?
  "True if a minimum of one vowel is present inside of string, borders
   excluded."
  [s]
  (some vowel? (butlast (rest s))))

(defn n-sandwiched-v?
  "True if a minimum of one vowel is present inside of string, borders
   excluded."
  [s]
  (some n-vowel? (str->charvec s)))

(defn double-c?
  "True if string ends with same letter and the letter is consonant."
  [s]
  (and (consonant? (last s))
       (= (last s) (last (butlast s)))))

(defn n-double-c?
  "True if string ends with same letter and the letter is consonant."
  [s]
  (let [cvec (str->charvec s)]
    (and (n-consonant? (last cvec))
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
  (let [cvec (str->charvec s)]
    (and (>= (count s) 3)
         (and (n-consonant? (last (butlast (butlast cvec))))
              (n-vowel? (last (butlast cvec)))
              (n-consonant? (last cvec))
              (not (#{\w \x \y} (second (last cvec))))))))


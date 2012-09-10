(ns porter-stemmer.steps)

;;;
;;; helper procedures
;;;

(defn ends?
  "True if string @s ends with string @end."
  [s end]
  (let [end-length (count end)
        s-length (count s)]
    (if (> s-length end-length)
      (let [s-end (subvec (into [] s) (- s-length end-length))]
        (every? true? (map = end s-end))))))

(defn stem
  "Return string @s without the end @end, only if @s actually ends with @end."
  [s end]
  (if (ends? s end)
    (apply str (subvec (into [] s) 0 (- (count s) (count end))))
    s))

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

(defn sandwitched-v?
  "True if a minimum of one vowel is present inside of string, borders
   excluded."
  [s]
  (some vowel? (butlast (rest s))))

(defn double-c?
  "True if string ends with same letter and the letter is consonant."
  [s]
  (and (consonant? (last s))
       (= (last s) (last (butlast s)))))

(defn ends-cvc?
  "True if string ends with <consonant><vowel><consonant>."
  [s]
  (and (>= (count s) 3)
       (and (consonant? (last (butlast (butlast s))))
            (vowel? (last (butlast s)))
            (consonant? (last s))
            (not (#{\w \x \y} (last s))))))

;;;
;;; algorithm steps
;;;

(defn post-1b [s]
  (apply str (cond
              (ends? s "at") (concat (stem s "at") "ate")
              (ends? s "bl") (concat (stem s "bl") "ble")
              (ends? s "iz") (concat (stem s "iz") "ize")
              (double-c? s) (if (not (#{\l \s \z} (last s)))
                              (butlast s)
                              s)
              (and (= (cvc-count s) 1) (ends-cvc? s)) (concat s "e")
              :else s)))

(defn step-1a [s]
  (apply str (cond (ends? s "sses") (concat (stem s "sses") "ss")
                   (ends? s "ies") (concat (stem s "ies") "i")
                   (ends? s "ss") (concat (stem s "ss") "ss")
                   (ends? s "s") (concat (stem s "s") "")
                   :else s)))

(defn step-1b [s]
  (apply str (cond
              (ends? s "eed") (if (> (cvc-count (stem s "eed")) 0)
                                (concat (stem s "eed") "ee")
                                s)
              (ends? s "ed") (if (sandwitched-v? (stem s "ed"))
                               (post-1b (concat (stem s "ed") ""))
                               s)
              (ends? s "ing") (if (sandwitched-v? (stem s "ing"))
                                (post-1b (concat (stem s "ing") ""))
                                s)
              :else s)))

(defn step-1c [s]
  (apply str (if (and (ends? s "y")
                      (sandwitched-v? (stem s "y")))
               (concat (stem s "y") "i")
               s)))


(defn step-2 [s]
  (let [trans-map [["ational" "ate"] ["tional" "tion"] ["enci" "ence"]
                   ["anci" "ance"] ["izer" "ize"] ["abli" "able"] ["alli" "al"]
                   ["entli" "ent"] ["eli" "e"] ["ousli" "ous"]
                   ["ization" "ize"] ["ation" "ate"] ["ator" "ate"]
                   ["alism" "al"] ["iveness" "ive"] ["fulness" "ful"]
                   ["ousness" "ous"] ["aliti" "al"] ["iviti" "ive"]
                   ["biliti" "ble"]]]
    (apply str
           (or (some (fn [[end alt]]
                       (if (and (ends? s end)
                                (> (cvc-count (stem s end)) 0))
                         (concat (stem s end) alt)))
                     trans-map)
               s))))

(defn step-3 [s]
  (let [trans-map [["icate" "ic"] ["ative" ""] ["alize" "al"]
                   ["iciti" "ic"] ["ical" "ic"] ["ful" ""] ["ness" ""]]]
    (apply str
           (or (some (fn [[end alt]]
                       (if (and (ends? s end)
                                (> (cvc-count (stem s end)) 0))
                         (concat (stem s end) alt)))
                     trans-map)
               s))))

(defn step-4 [s]
  (let [trans-map [["al" ""] ["ance" ""] ["ence" ""] ["er" ""] ["ic" ""]
                   ["able" ""] ["ible" ""] ["ant" ""] ["ement" ""] ["ment" ""]
                   ["ent" ""] ["ion" ""] ["ou" ""] ["ism" ""] ["ate" ""]
                   ["iti" ""] ["ous" ""] ["ive" ""] ["ize" ""]]]
    (apply str
           (or (some (fn [[end alt]]
                       (if (and (ends? s end)
                                (> (cvc-count (stem s end)) 1))
                         (if (= end "ion")
                           (or (and (or (ends? (stem s "ion") "s")
                                        (ends? (stem s "ion") "t"))
                                    (concat (stem s end) alt))
                               s)
                           (concat (stem s end) alt))))
                     trans-map)
               s))))

(defn step-5a [s]
  (apply str
         (if (ends? s "e")
           (if (> (cvc-count (stem s "e")) 1)
             (concat (stem s "e") "")
             (if (and (= (cvc-count (stem s "e")) 1)
                      (not (ends-cvc? (stem s "e"))))
               (concat (stem s "e") "")
               s))
           s)))

(defn step-5b [s]
  (apply str (if (and (> (cvc-count s) 1)
                      (double-c? s)
                      (ends? s "l"))
               (butlast s)
               s)))

(defn stem-it [s]
  (-> s
      step-1a
      step-1b
      step-1c
      step-2
      step-3
      step-4
      step-5a
      step-5b))

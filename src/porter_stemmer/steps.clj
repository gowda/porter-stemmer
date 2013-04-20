(ns porter-stemmer.steps
  (:use [porter-stemmer.utils]))

(defmacro defstep [name params & body]
  `(defn ~name ~params (apply str ~@body)))

;;;
;;; algorithm steps
;;;

(defstep post-1b [s]
  (cond
   (ends? s "at") (concat (stem s "at") "ate")
   (ends? s "bl") (concat (stem s "bl") "ble")
   (ends? s "iz") (concat (stem s "iz") "ize")
   (double-consonant-end? s) (if (not (#{\l \s \z} (last s)))
                               (butlast s)
                               s)
   (and (= (cvc-count s) 1) (cvc-end? s)) (concat s "e")
   :else s))


(defstep step-1a [s]
  (cond (ends? s "sses") (concat (stem s "sses") "ss")
        (ends? s "ies") (concat (stem s "ies") "i")
        (ends? s "ss") (concat (stem s "ss") "ss")
        (ends? s "s") (concat (stem s "s") "")
        :else s))

(defstep step-1b [s]
  (cond
   (ends? s "eed") (if (> (cvc-count (stem s "eed")) 0)
                     (concat (stem s "eed") "ee")
                     s)
   (ends? s "ed") (if (contains-vowel? (stem s "ed"))
                    (post-1b (concat (stem s "ed") ""))
                    s)
   (ends? s "ing") (if (contains-vowel? (stem s "ing"))
                     (post-1b (concat (stem s "ing") ""))
                     s)
   :else s))

(defstep step-1c [s]
  (if (and (ends? s "y")
           (contains-vowel? (stem s "y")))
    (concat (stem s "y") "i")
    s))


(defstep step-2 [s]
  (let [[end replacement]
        (->> [["ational" "ate"] ["tional" "tion"] ["enci" "ence"]
              ["anci" "ance"] ["izer" "ize"] ["bli" "ble"] ["alli" "al"]
              ["entli" "ent"] ["eli" "e"] ["ousli" "ous"] ["ization" "ize"]
              ["ation" "ate"] ["ator" "ate"] ["alism" "al"] ["iveness" "ive"]
              ["fulness" "ful"] ["ousness" "ous"] ["aliti" "al"]
              ["iviti" "ive"] ["biliti" "ble"] ["logi" "log"]]
             (filter (fn [[end _]]
                       (ends? s end)))
             first)]
    (if (> (cvc-count (stem s end)) 0)
      (concat (stem s end) replacement)
      s)))

(defstep step-3 [s]
  (let [[end replacement]
        (->> [["icate" "ic"] ["ative" ""] ["alize" "al"] ["iciti" "ic"]
             ["ical" "ic"] ["ful" ""] ["ness" ""]]
            (filter (fn [[end _]]
                      (ends? s end)))
            first)]
    (if (> (cvc-count (stem s end)) 0)
      (concat (stem s end) replacement)
      s)))

(defstep step-4 [s]
  (let [[end replacement]
        (->> [["al" ""] ["ance" ""] ["ence" ""] ["er" ""] ["ic" ""]
              ["able" ""] ["ible" ""] ["ant" ""] ["ement" ""] ["ment" ""]
              ["ent" ""] ["ion" ""] ["ou" ""] ["ism" ""] ["ate" ""]
              ["iti" ""] ["ous" ""] ["ive" ""] ["ize" ""]]
             (filter (fn [[end _]]
                       (ends? s end)))
             first)]
    (if (> (cvc-count (stem s end)) 1)
      (if (= end "ion")
        (or (and (or (ends? (stem s "ion") "s")
                     (ends? (stem s "ion") "t"))
                 (concat (stem s end) replacement))
            s)
        (concat (stem s end) replacement))
      s)))

(defstep step-5a [s]
  (if (ends? s "e")
    (if (> (cvc-count (stem s "e")) 1)
      (concat (stem s "e") "")
      (if (and (= (cvc-count (stem s "e")) 1)
               (not (cvc-end? (stem s "e"))))
        (concat (stem s "e") "")
        s))
    s))

(defstep step-5b [s]
  (if (and (> (cvc-count s) 1)
           (double-consonant-end? s)
           (ends? s "l"))
    (butlast s)
    s))

(defn stem-it [s]
  (if (> (count s) 2)
    (-> s
        step-1a
        step-1b
        step-1c
        step-2
        step-3
        step-4
        step-5a
        step-5b)
    s))

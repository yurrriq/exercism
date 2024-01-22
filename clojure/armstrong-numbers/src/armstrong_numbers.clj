(ns armstrong-numbers
  "The Armstrong Numbers exercise from Exercism"
  (:require [clojure.math :as math]))

(defn- armstrong-xform
  [k]
  (comp
    (map #(Character/digit % 10))
    (map #(math/pow % k))))

(defn armstrong?
  "Determine whether `n` is an Armstrong number."
  [n]
  (or (zero? n)
      (let [k (inc (int (math/floor (math/log10 n))))]
        (->> (str n)
            (map (comp #(math/pow % k) #(Character/digit % 10)))
            (reduce + 0)
            int
            (= n)))))

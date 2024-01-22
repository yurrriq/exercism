(ns armstrong-numbers
  "The Armstrong Numbers exercise from Exercism"
  (:require [clojure.math :as math]))

(defn- armstrong-accum
  [k]
  (fn [acc c]
    (-> (Character/digit c 10)
        (math/pow k)
        (+ acc))))

(defn armstrong?
  "Determine whether `n` is an Armstrong number."
  [n]
  (or (zero? n)
      (-> (inc (int (math/floor (math/log10 n))))
          armstrong-accum
          (reduce 0 (str n))
          int
          (= n))))

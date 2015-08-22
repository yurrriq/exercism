(ns raindrops
  "Converting numbers to raindrops, based on their prime factors."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [ann]]))

;; == PRIVATE API ==============================================================

(defn- divides?
  "Given a denominator `d` and a numerator `n`, return `true` if `d | n`,
  otherwise `false`."
  [d n]
  (zero? (rem n d)))


;; == PUBLIC API ===============================================================

(defn convert
  "Given a number `n`, convert it to a string, the contents of which
  depends on `n`'s prime factors.

  - If 3 divides `n`, output 'Pling'.
  - If 5 divides `n`, output 'Plang'.
  - If 7 divides `n`, output 'Plong'.
  - If neither 3 nor 5 nor 7 divides `n`, return the `n` as a string."
  [n]
  (or (reduce-kv #(cond-> %1 (divides? %2 n) (str %3))
                 nil {3 "Pling", 5 "Plang", 7 "Plong"})
      (str n)))

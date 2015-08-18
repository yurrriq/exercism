(ns triangle
  "Determining the type of a triangle."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [Int U ann defalias]])
  (:import (clojure.lang Keyword))
  (:refer-clojure :exclude [type]))

;; == TYPE ALIASES =============================================================

(defalias Side         Int)
(defalias TriangleType (U ':equilateral ':illogical ':isosceles ':scalene))


;; == PUBLIC API ===============================================================

(ann type [Side Side Side -> TriangleType])
(defn type
  "Given three sides, return a keyword—`:equilateral`, `:illogical`,
  `:isosceles`, or `:scalene`—describing the quality of the given triangle."
  [a b c]
  (cond
    (= a b c)                   :equilateral
    (or (>= a (+ b c))
        (>= b (+ c a))
        (>= c (+ a b)))         :illogical
    (= 2 (count (set [a b c]))) :isosceles
    :else                       :scalene))


;; == EMACS CONFIG =============================================================

;; Local Variables:
;; mode: clojure
;; eval: (typed-clojure-mode)
;; End:

(ns triangle
  "Determining the type of a triangle."
  {:author "Eric Bailey"}
  (:refer-clojure :exclude [type]))

;; == PRIVATE API ==============================================================

(def ^:private otherwise
  "A needlessly Haskell-y function that takes any number of arguments and
  returns `true`."
  (constantly true))

(defn- picks
  "Given a collection, return a list of vector pairs, consisting of each element
  and a list of every other element.

  See also: [Answer on Stack Overflow](http://stackoverflow.com/a/12872133)"
  [coll]
  (let [n    (dec (count coll))
        coll (cycle coll)]
    (for [i (range (inc n))]
      [(nth coll i) (take n (drop (inc i) coll))])))


;; == PUBLIC API ===============================================================

(defn type
  "Given three sides, return a keyword—`:equilateral`, `:illogical`,
  `:isosceles`, or `:scalene`—describing the quality of the given triangle."
  [a b c]
  (if (= a b c) :equilateral
      (condp #(%1 %2) (picks [a b c])
        #(some (fn [[x yz]] (>= x (apply + yz))) %) :illogical
        #(some (fn [[x yz]] (some #{x} yz)) %)      :isosceles
        otherwise                                  :scalene)))

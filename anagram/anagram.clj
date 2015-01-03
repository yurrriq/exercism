(ns anagram
  (:require [clojure.string :refer [lower-case]]))

(defn anagrams-for [s coll]
  (letfn [(letters [s] (reduce #(conj %1 (int %2)) [] (lower-case s)))]
    (filter #(and (not= (lower-case s)
                        (lower-case %))
                  (= (sort (letters s))
                     (sort (letters %))))
            coll)))

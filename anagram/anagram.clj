(ns anagram
  (:require [clojure.string :refer [lower-case]]))

(defn anagrams-for [s coll]
  (filter #(let [comparison (map lower-case [s %])]
             (when (apply not= comparison)
               (apply = (map sort comparison))))
          coll))

(ns anagram
  "Selecting sublists of anagrams."
  {:author "Eric Bailey"}
  (:require [clojure.string :refer [lower-case]]))

(defn- anagram-for?
  "Given a word, return a transducer that removes candidates which are
  case-insensitive equal to or not an anagram of the given word."
  [word]
  (let [word-lowered (lower-case word)
        word-sorted  (sort word-lowered)]
    (remove (fn [candidate]
              (let [candidate-lowered (lower-case candidate)]
                (or (= candidate-lowered word-lowered)
                    (not= (sort candidate-lowered) word-sorted)))))))

(defn anagrams-for
  "Given a word and a list of anagram candidates, return a sublist
  where each candidate is an anagram for the given word."
  [word candidates]
  (transduce (anagram-for? word) conj [] candidates))

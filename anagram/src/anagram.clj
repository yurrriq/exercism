(ns anagram
  "Selecting sublists of anagrams."
  {:author "Eric Bailey"}
  (:require [clojure.string :refer [lower-case]]))

(def ^:private normalize (memoize lower-case))

(def ^:private sort-norm (memoize (comp sort normalize)))

;; http://stackoverflow.com/a/31755514/1793234
(defn- on
  ([f g]   (fn [x y] (f (g x) (g y))))
  ([f g x] (partial (on f g) x)))

(defn- anagram-for?
  "Given a word, return a transducer that removes candidates which are
  case-insensitive equal to or not an anagram of the given word."
  [word]
  (remove (some-fn (on    = normalize word)
                   (on not= sort-norm word))))

(defn anagrams-for
  "Given a word and a list of anagram candidates, return a sublist
  where each candidate is an anagram for the given word."
  [word candidates]
  (sequence (anagram-for? word) candidates))

;;;; -*- mode: clojure; mode: typed-clojure -*-

(ns allergies
  "Determining people's allergies."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [IFn Seq U Vec ann cf defalias loop]])
  (:refer-clojure :exclude [loop]))


;;;=============================================================
;;; Type Aliases
;;;=============================================================

(ann Allergen (U ':eggs ':peanuts ':shellfish ':strawberries
                 ':tomatoes ':chocolate ':pollen ':cats))
(defalias Allergen
  "A keyword representing a known allergen."
  (U ':eggs ':peanuts ':shellfish ':strawberries
     ':tomatoes ':chocolate ':pollen ':cats))

(ann AllergenIndex Number)
(defalias AllergenIndex
  "A number representing the index of a known [[Allergen]]."
  Number)

(ann Score Number)
(defalias Score
  "A number representing a person's [[allergies]]."
  Number)


;;;=============================================================
;;; Private API
;;;=============================================================

(ann allergens (Vec Allergen))
(def ^:private allergens
  "A vector of keywords representing all the allergens."
  [:eggs :peanuts :shellfish :strawberries :tomatoes :chocolate :pollen :cats])

(ann from-enum [Allergen -> AllergenIndex])
(defn- from-enum
  "Given an [[Allergen]], return its [[AllergenIndex]]. If the allergen is
  unknown, return -1.

  Named after Haskell's `fromEnum`."
  [allergen]
  (loop [s :- (U (Vec Allergen) (Seq Allergen)) allergens
         i :- AllergenIndex 0]
    (cond
      (empty? s)             -1
      (= (first s) allergen) i
      :else                  (recur (rest s) (inc i)))))


;;;=============================================================
;;; Public API
;;;=============================================================

(ann allergic-to? (IFn [Score -> [Allergen -> Boolean]]
                       [Score Allergen -> Boolean]))
(defn allergic-to?
  "Given a [[Score]], return a function that takes an [[Allergen]] and calls the
  binary version of [[allergic-to?]].

  Given a [[Score]] and an [[Allergen]], return `true` if the [[Score]]
  indicates an allergy to the given [[Allergen]], otherwise `false`.  If the
  [[Allergen]] is not known, return `false`."
  ([score]
   (fn [allergen]
     (allergic-to? score allergen)))
  ([score allergen]
   (-> (->> (from-enum allergen)
            (bit-shift-right score))
       (bit-and 1)
       (pos?))))

(ann allergies [Score -> (Vec Allergen)])
(defn allergies
  "Given a [[Score]], return the vector of [[Allergen]]s it represents,
  i.e. the person's allergies."
  [score]
  (filterv (allergic-to? score) allergens))

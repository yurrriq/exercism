(ns beer-song
  (:require [clojure.string :as string]))


;;;; ==== PRIVATE API ==========================================================

(defn- bottles [n]
  (case n
    0 "no more bottles"
    1 "1 bottle"
    (str n " bottles")))

(defn- of-beer [s] (str s " of beer"))

(defn- bottles-of-beer [n] (-> n bottles of-beer))

(def ^:private buy-more "Go to the store and buy some more, ")

(defn- on-the-wall [s] (str s " on the wall"))

(defn- pass-around [s] (str s " and pass it around, "))

(defn- pause [s] (str s ", "))

(defn- stop [s] (str s ".\n"))

(defn- take-down [n] (str "Take " (if (= n 1) "it" "one") " down"))

(defn- take-and-pass-or-buy [n]
  (if (zero? n)
    buy-more
    (-> n take-down pass-around)))


;;;; ==== PUBLIC API -==========================================================

(defn verse
  ([n] (apply str (map #(verse n %) [:A :b :c])))
  ([n position]
   (case position
     :A (-> (verse n :a) (cond-> (zero? n) string/capitalize) pause)
     :a (-> n bottles-of-beer on-the-wall)
     :b (-> n bottles-of-beer stop)
     :c (str (take-and-pass-or-buy n) (verse n :d))
     :d (-> (dec n) (mod 100) (verse :a) stop))))

(defn sing
  ([start]     (sing start 0))
  ([start end] (string/join "\n" (map verse (range start (dec end) -1)))))

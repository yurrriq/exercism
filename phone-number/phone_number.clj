(ns phone-number
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer [replace]]))

(defn number [s]
  (let [digits     (replace s #"\D" "")
        num-digits (count digits)]
    (cond
      (= num-digits 10)                        digits
      (and (= num-digits 11) (= (first s) \1)) (subs digits 1)
      :else                                    "0000000000")))

(defn area-code [s] (subs (number s) 0 3))

(defn pretty-print [s]
  (->> (number s)
       ((juxt #(subs % 0 3) #(subs % 3 6) #(subs % 6 10)))
       (apply format "(%s) %s-%s")))

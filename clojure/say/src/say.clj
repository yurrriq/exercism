(ns say
  "The Say exercise from Exercism"
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]))

(defn number
  "Given an integer `n`, return a string with `n` as a cardinal English number."
  [n]
  (if (<= 0 n 999999999999)
    ;; https://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm
    (-> (pprint/cl-format nil "~R" n)
        (string/replace "," ""))
    (throw (IllegalArgumentException. "n must be within (0, 999999999999]"))))

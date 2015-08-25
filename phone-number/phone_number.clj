(ns phone-number
  (:refer-clojure :exclude [replace])
  (:require [clojure.core.typed :as t :refer [ann defalias fn let]]
            [clojure.string :as s])
  (:refer-clojure :exclude [fn let]))


;;;; ==== TYPE ALIASES =========================================================

(defalias PhoneNumber String)


;;;; ==== PRIVATE API ==========================================================

(ann bad-number PhoneNumber)
(def ^:private bad-number "0000000000")

(ann ten-digits? [Number -> Boolean])
(defn- ten-digits? [x] (= 10 x))

(ann north-american? [PhoneNumber -> [Number -> Boolean]])
(defn- north-american?
  "TODO: write docstring"
  [^String digits]
  (fn [num-digits :- Number]
    (and (= 11 num-digits)
         (.startsWith digits "1"))))


;;;; ==== PUBLIC API ===========================================================

(ann number [String -> PhoneNumber])
(defn number
  "TODO: write docstring"
  [s]
  (let [digits (s/replace s #"\D" "")]
    (condp apply [(count digits)]
      ten-digits?              digits
      (north-american? digits) (subs digits 1)
      bad-number)))

(ann area-code [String -> PhoneNumber])
(defn area-code
  "TODO: write docstring"
  [s]
  (subs (number s) 0 3))

(ann pretty-print [String -> PhoneNumber])
(defn pretty-print
  "TODO: write docstring"
  [s]
  (let [d (number s)]
    (str \( (subs d 0 3) ") " (subs d 3 6) \- (subs d 6 10))))


;;;; ==== EMACS CONFIG =========================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

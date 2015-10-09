(ns atbash-cipher
  "Encoding messages using the Atbash cipher."
  {:author "Eric Bailey"}
  (:require [cats.monad.maybe :refer [just map-maybe nothing]]
            [clojure.string :as string]))


;;;; ==== PRIVATE API ==========================================================

(defn- rotate
  "Given a lowercase (non-uppercase) character, if it is a letter,
  transpose it with the reversed alphabet and return just the result,
  otherwise return nothing."
  [^Character c]
  {:pre [(not (Character/isUpperCase c))]}
  (let [a (int \a), lc (int c), z (int \z)]
    (if (Character/isLetter c)
      (just (->> (compare c \a) (- (int z)) char))
      (nothing))))

(defn- cipher
  "Given a character, if it is a digit, return just the character, otherwise
  [[rotate]] it."
  [^Character c]
  (if (Character/isDigit c)
    (just c)
    (rotate (Character/toLowerCase c))))


;;;; ==== PUBLIC API ===========================================================

(defn encode
  "Given a message, encode it using the Atbash cipher and return the result."
  [^String s]
  (->> (map-maybe cipher s)
       (partition-all 5)
       (map (partial apply str))
       (string/join " ")))


;;;; ==== EMACS CONFIG =========================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

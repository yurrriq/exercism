(ns atbash-cipher
  "Encoding messages using the Atbash cipher."
  {:author "Eric Bailey"
   :lang   :core.typed}
  (:require [cats.core :refer [>>=]]
            [cats.context :as ctx]
            [cats.monad.maybe :refer [just map-maybe nothing]]
            [clojure.core.typed :as t :refer [ann ann-datatype defalias fn]]
            [clojure.string :as string])
  (:refer-clojure :exclude [fn]))


;;;=============================================================
;;; cats.monad.maybe annotations
;;;=============================================================

;; FIXME
(ann-datatype cats.monad.maybe.Just [a :- t/Any] :extends [Object])

;; FIXME
(ann-datatype cats.monad.maybe.Nothing [] :extends [Object])

;; FIXME
(ann ^:no-check cats.monad.maybe/just (t/All [a] [a -> cats.monad.maybe.Just]))

;; FIXME
(ann ^:no-check cats.monad.maybe/nothing [-> cats.monad.maybe.Nothing])

;; FIXME
(defalias Maybe
  (t/TFn [[a :variance :covariant]]
    (t/U cats.monad.maybe.Just cats.monad.maybe.Nothing)))

;; FIXME
(ann ^:no-check cats.monad.maybe/map-maybe
     (t/All [a]
       [[a -> (Maybe a)] (t/Nilable (t/Seqable a)) -> (t/ASeq a)]))


;;;=============================================================
;;; Private API
;;;=============================================================

(def int-z (int \z))

(ann digit? [Character -> Boolean])
(defn- digit? [^Character c] (Character/isDigit c))

(ann letter? [Character -> Boolean])
(defn- letter? [^Character c] (Character/isLetter c))

(ann lower-case [Character -> Character])
(defn- lower-case [^Character c] (Character/toLowerCase c))

(ann upper-case? [Character -> Boolean])
(defn- upper-case? [^Character c] (Character/isUpperCase c))

(ann rotate [Character -> (Maybe Character)])
(defn- rotate
  "Given a lowercase (non-uppercase) character, if it is a letter,
  transpose it with the reversed alphabet and return just the result,
  otherwise return nothing."
  [^Character c]
  {:pre [(not (upper-case? c))]}
  (if (letter? c)
    (just (->> (compare c \a) (- int-z) char))
    (nothing)))

(ann cipher [char -> (Maybe char)])
(defn- cipher
  "Given a character, if it is a digit, return just the character,
  otherwise [[rotate]] it."
  [^Character c]
  (if (digit? c)
    (just c)
    (rotate (lower-case c))))


;;;=============================================================
;;; Public API
;;;=============================================================

(ann encode [String -> String])
(defn encode
  "Given a message, encode it using the Atbash cipher and return the result."
  [^String s]
  (->> (map-maybe cipher s)
       (partition-all 5)
       (map (fn [cs :- (t/ASeq Character)] (apply str cs)))
       (string/join " ")))

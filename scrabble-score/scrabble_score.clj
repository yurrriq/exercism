(ns scrabble-score
  "Computing the Scrabble score for words."
  {:author "Eric Bailey"}
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed :refer [Int Map U ann fn defalias]])
  (:import (clojure.lang Seqable)))

;; ==== TYPE ALIASES ===========================================================

(defalias Letter (U Character String))

(defalias Score  Int)

(defalias Word   String)


;; ==== PRIVATE API ============================================================

(ann legend (Map Score String))
(def ^:private legend
  {1  "AEIOULNRST"
   2  "DG"
   3  "BCMP"
   4  "FHVWY"
   5  "K"
   8  "JX"
   10 "QZ"})

(ann points (Map Character Score))
(def ^:private points
  (reduce-kv (fn [char->score :- (Map Character Score)
                 score       :- Score
                 letters     :- String]
               (merge char->score (zipmap letters (repeat score))))
             {} legend))


;; ==== PUBLIC API =============================================================

(ann ->char [Letter -> Character])
(defn- ->char [x] (if (char? x) x (.charAt ^String x 0)))

(ann score-letter [Letter -> Score])
(defn score-letter [letter]
  (get points (Character/toUpperCase ^Character (->char letter)) 0))

(ann score-word [Word -> Score])
(defn score-word [word]
  (reduce (fn [score  :- Score
              letter :- Character]
            (+ score (score-letter letter)))
          0 word))


;; ==== EMACS CONFIG ===========================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

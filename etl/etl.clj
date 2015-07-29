(ns etl
  "Converting Scrabble scoring systems."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [Int Map ann defalias fn]]
            [clojure.string     :refer [lower-case]])
  (:import (clojure.lang Seqable))
  (:refer-clojure :exclude [fn]))

;; == TYPE ALIASES =============================================================

(defalias Letter  String)
(defalias Letters (Seqable Letter))
(defalias Score   Int)
(defalias OldData (Map Score Letters))
(defalias NewData (Map Letter Score))


;; == PRIVATE API ==============================================================

(ann one-to-many->many-to-one ['[Score Letters] -> (Seqable '[Letter Score])])
(defn- one-to-many->many-to-one
  "Given an old data entry (score, list of letters),
  return a list of new data entries (letter, score)."
  [[score letters]]
  (map (fn [letter :- Letter] [(lower-case letter) score]) letters))


;; == PUBLIC API ===============================================================

(ann transform [OldData -> NewData])
(defn transform
  "Given a map from score to letters, return a map from letter to score."
  [old-data]
  (into {} (mapcat one-to-many->many-to-one old-data)))


;; == EMACS CONFIG =============================================================

;; Local Variables:
;; mode: clojure
;; eval: (typed-clojure-mode)
;; End:

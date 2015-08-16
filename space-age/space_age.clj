(ns space-age
  "Calculating how old someone is in terms of a given planet's solar years."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [ann defalias doseq fn letfn>]])
  (:import (clojure.lang Keyword))
  (:refer-clojure :exclude [doseq fn]))

;; == TYPE ALIASES =============================================================

(defalias Age       Long)
(defalias Factor    Double)
(defalias Planet    Keyword)
(defalias EarthYear Double)


;; == PRIVATE API ==============================================================

(ann seconds-per-year [(U Factor Planet) -> EarthYear])
(defn seconds-per-year [x]
  (if (number? x)
    (* x (seconds-per-year :earth))
    (case x
      :earth   (* 365.25 24 60 60)
      :mercury (recur 0.2408467)
      :venus   (recur 0.61519726)
      :mars    (recur 1.8808158)
      :jupiter (recur 11.862615)
      :saturn  (recur 29.447498)
      :uranus  (recur 84.016846)
      :neptune (recur 164.79132))))


;; == PUBLIC API (GENERATED) ===================================================

(letfn> [age-on :- [Planet -> [Age -> EarthYear]]
         (age-on [planet]
           (fn [seconds :- Age]
             (/ seconds (seconds-per-year planet))))]
  (doseq [planet :- Planet
          [:earth :mercury :venus :mars :jupiter :saturn :uranus :neptune]]
    (intern *ns* (symbol (str "on-" (name planet)))
            (age-on planet))))


;; == EMACS CONFIG =============================================================

;; Local Variables:
;; mode: clojure
;; eval: (typed-clojure-mode)
;; End:

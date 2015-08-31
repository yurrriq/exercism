(ns allergies
  ""
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [IFn Seq U Vec ann cf defalias loop]])
  (:refer-clojure :exclude [loop]))

;; == TYPE ALIASES =============================================================

(defalias Allergen
  (U ':eggs ':peanuts ':shellfish ':strawberries
     ':tomatoes ':chocolate ':pollen ':cats))


;; == PRIVATE API ==============================================================

(ann allergens (Vec Allergen))
(def ^:private allergens
  [:eggs :peanuts :shellfish :strawberries :tomatoes :chocolate :pollen :cats])

(ann from-enum [Allergen -> Number])
(defn- from-enum [allergen]
  (loop [s :- (U (Vec Allergen) (Seq Allergen)) allergens
         i :- Number 0]
    (cond
      (empty? s)             -1
      (= (first s) allergen) i
      :else                  (recur (rest s) (inc i)))))


;; == PUBLIC API ===============================================================

(ann allergic-to? (IFn [Number -> [Allergen -> Boolean]]
                       [Number Allergen -> Boolean]))
(defn allergic-to?
  ([score]
   (fn [allergen]
     (allergic-to? score allergen)))
  ([score allergen]
   (-> (->> (from-enum allergen)
            (bit-shift-right score))
       (bit-and 1)
       (pos?))))

(ann allergies [Number -> (Vec Allergen)])
(defn allergies [score] (filterv (allergic-to? score) allergens))

(comment ;; These are poorly named and use strings in the tests.
  (ann ^:no-check allergies [Number -> (Vec String)])
  (defn list [score] (mapv name (allergies score)))

  (ann ^:no-check allergic-to? [Number String -> Boolean])
  (defn allergic_to? [score allergen] (allergic-to? score (keyword allergen))))


;; == EMACS CONFIG =============================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

(ns roman-numerals
  "Converting decimal numbers to Roman numerals."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [Int NilableNonEmptyASeq
                                        ann defalias fn let loop]])
  (:refer-clojure :exclude [fn let loop]))

;; == TYPE ALIASES =============================================================

(defalias Arabic Int)
(defalias Roman  String)


;; == PRIVATE API ==============================================================

(ann arabic->roman '['[Arabic Roman] *])
(def ^:private arabic->roman
  [[1000  "M"], [900 "CM"],
   [500   "D"], [400 "CD"],
   [100   "C"], [90  "XC"],
   [50    "L"], [40  "XL"],
   [10    "X"], [9   "IX"],
   [5     "V"], [4   "IV"],
   [1     "I"]])

(ann divmod [Arabic Arabic -> '[Arabic Arabic]])
(defn- divmod [^Integer n ^Integer d] [(unchecked-divide-int n d) (mod n d)])


;; == PUBLIC API ===============================================================

(ann numerals [Arabic -> Roman])
(defn numerals [n]
  (loop [f   :- [Roman -> Roman] str
         ars :- (NilableNonEmptyASeq '[Arabic Roman]) (seq arabic->roman)
         n   :- Arabic n]
    (if-let [[[a r] & ars'] ars]
      (if (< n a) (recur f ars' n)
          (let [[n m] (divmod n a)]
            (recur (comp f #(str (apply str (repeat n r)) %)) ars' m)))
      (f ""))))


;; == EMACS CONFIG =============================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

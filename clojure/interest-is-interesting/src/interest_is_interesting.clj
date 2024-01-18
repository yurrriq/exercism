(ns interest-is-interesting
  "The Interest is Interesting exercise

  https://exercism.org/tracks/clojure/exercises/interest-is-interesting"
  (:require [clojure.math :as math]))

(defn interest-rate
  "Return the interest rate based on the specified balance."
  [balance]
  (condp > balance
    0 -3.213
    1000 0.5
    5000 1.621
    2.475))

(defn annual-balance-update
  "Return the annual balance update, taking into account the interest rate."
  [balance]
  (-> balance
      (* (/ (bigdec (abs (interest-rate balance))) 100M))
      (+ balance)
      bigdec))

(defn amount-to-donate
  "Return the amount to donate based on the balance and tax-free percentage."
  [balance tax-free-percentage]
  (if (pos? balance)
    (-> balance (* 2M (/ tax-free-percentage 100)) math/floor int)
    0))

(ns prime-factors
  "Computing the prime factors of a given natural number."
  {:author "Eric Bailey"})


;; == PRIVATE API ==============================================================

(defn- divides?
  "Return `true` if `d` divides `n`, otherwise `false`."
  [d n]
  (zero? (rem n d)))

;; Adapted from a Clojure port of an F# implementation
;; http://stackoverflow.com/a/7625207/1793234
(def ^:private primes
  "Infinite, lazy sequence of prime numbers."
  ((fn step [m n]
     (or (some-> (get m n)
           (-> (->> (reduce #(update %1 (+ %2 n) conj %2) (dissoc m n)))
               (step (inc n))))
         (-> (assoc m (* n n) `(~n))
             (step (inc n))
             (->> (cons n) (lazy-seq)))))
   {} 2))


;; == PUBLIC API ===============================================================

;; Adapted from my Haskell solution
;; http://exercism.io/submissions/957c2e30e630484bb19572f7ed2edab4
(defn of [x]
  ((fn go [n [p & ps :as primez]]
     (cond
       (< n 2)              ()
       (< n (* p p))        `(~n)
       (divides? p n)       (cons p (go (/ n p) primez))
       (not (divides? p n)) (go n ps)))
   x primes))

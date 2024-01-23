(ns accumulate
  "The Accumulate exercise from Exercism")

(defn accumulate
  "Return a lazy sequence consisting of the result of applying `f` to
  each item in `coll`."
  [f coll]
  (lazy-seq
   (when-let [[x & xs] (seq coll)]
     (cons (f x) (accumulate f xs)))))

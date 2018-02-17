(ns trinary)

(defn to-decimal [string]
  (loop [digits (seq string), sum 0]
    (if-let [n (and (seq digits) (compare (first digits) \0))]
      (if-not (<= 0 n 2) 0 (recur (rest digits) (+ n (* sum 3))))
      sum)))

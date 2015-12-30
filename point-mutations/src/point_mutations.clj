(ns point-mutations)

(defn hamming-distance
  "Given two DNA strands, compute and return their Hamming distance,
  i.e. the number of positions at which the corresponding nucleotides are
  different. If the strands differ in length, return nil."
  [a b]
  (when (= (count a) (count b))
    (reduce #(cond-> %1 %2 inc) 0 (map not= a b))))

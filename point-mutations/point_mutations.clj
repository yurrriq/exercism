(ns point-mutations)

(defn hamming-distance [a b]
  (when (apply = (map count [a b]))
    (reduce (fn [n matches?] (cond-> n matches? inc)) 0
            (map not= a b))))

(comment
  ;; Slower
  (count (filter true? (map not= a b)))
  
  ;; Slowest
  ;; https://gist.github.com/mihneadb/4318257
  (count (filter true? (map (partial reduce not=) (map vector a b)))))

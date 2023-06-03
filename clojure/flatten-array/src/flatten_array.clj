(ns flatten-array)

(defn flatten [arr]
  (->> (tree-seq sequential? seq arr)
       (filter (complement sequential?))
       (filter some?)))

(ns point-mutations)

(defn hamming-distance
  "Given two DNA strands, compute and return their Hamming distance,
  i.e. the number of positions at which the corresponding nucleotides are
  different. If the strands differ in length, return nil."
  [a b]
  (when (= (count a) (count b))
    (reduce (fn [distance equal?]
              (if equal?
                distance
                (inc distance)))
            0 (map = a b))))

(comment
  (defn tranducing [a b]
    (let [hamming-xform (comp (map (partial apply =))
                              (map (fn [same?] (if same? 0 1))))]
      (when (= (count a) (count b))
        (transduce hamming-xform + (map vector a b)))))

  (defn looping [a b]
    (loop [d 0
           a a
           b b]
      (if (and (empty? a) (empty? b)) d
          (when-let [[h & t] (seq b)]
            (recur (if (= (first a) h) d (inc d))
                   (rest a)
                   t))))))

(ns grains)

(defn- expt [b p]
  (loop [b (bigint b), p p, z 1]
    (cond
      (zero? p) z
      (even? p) (recur (* b b) (/ p 2) z)
      :else     (recur b (dec p) (* b z)))))

(def square (comp (partial expt 2) dec))

(def total (constantly (dec (expt 2 64))))

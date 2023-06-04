(ns sublist)

(defn- starts-with? [xs ys]
  (or (empty? xs)
      (and (seq? (seq ys))
           (= (first xs) (first ys))
           (recur (rest xs) (rest ys)))))

(defn- tails [coll]
  (reductions (fn [s _] (rest s)) (seq coll) coll))

(defn- sublist? [xs ys]
  (or (empty? xs)
      (and (seq? (seq ys))
           (some #(starts-with? (seq xs) %) (butlast (tails ys))))))

(defn classify
  "Determine the relation between `list1` and `list2`."
  [list1 list2]
  (cond
    (= list1 list2) :equal
    (sublist? list1 list2) :sublist
    (sublist? list2 list1) :superlist
    :else :unequal))

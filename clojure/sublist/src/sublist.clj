(ns sublist)

(defn- sublist? [xs ys]
  (some #{xs} (partition (count xs) 1 ys)))

(defn classify
  "Determine the relation between `list1` and `list2`."
  [list1 list2]
  (cond
    (= list1 list2)        :equal
    (sublist? list1 list2) :sublist
    (sublist? list2 list1) :superlist
    :else                  :unequal))

(ns binary-search)

(defn middle
  "Given a collection, return the middle index, i.e. `⌊number of items / 2⌋`."
  [coll]
  (-> (count coll) (quot 2)))

(defn search-for [elem coll]
  "Given an element to search for and a collection,
  perform a binary search for `elem` in `coll` and return the its index.
  If `elem` is not found, throw an exception."
  {:pre [(or (apply <= coll)
            (throw (Exception. "Collection must be sorted.")))]}
  (loop [acc 0, s coll]
    (let [middle (middle s)]
      (condp #(%1 %2 elem) (nth s middle)
        = (+ acc middle),
        (constantly (zero? middle))
        (throw (Exception. (str elem " not found in " coll))),
        < (recur (+ acc middle) (drop middle s)),
        > (recur    acc         (take middle s))))))

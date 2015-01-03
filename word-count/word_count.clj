(ns word-count)

(defn word-count [s]
  (reduce #(update-in %1 [%2] (fnil inc 0)) {}
          (re-seq #"\w+" (clojure.string/lower-case s))))

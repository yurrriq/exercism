(ns nucleotide-count
  (:refer-clojure :exclude [count]))

(defn nucleotide-counts [s]
  (reduce #(update-in %1 [%2] inc) (zipmap "ACGT" (repeat 4 0)) s))

(defn count [nucleotide s]
  (if (some #{nucleotide} "ACGT")
    (get (nucleotide-counts (filter #(= nucleotide %) s)) nucleotide)
    (throw (Exception. "invalid nucleotide"))))

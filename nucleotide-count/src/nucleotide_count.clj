(ns nucleotide-count
  (:require [clojure.core.typed :refer [HSet IFn Int Map U Val
                                        ann defalias fn]])
  (:refer-clojure :exclude [count fn]))

;; == TYPE ALIASES =============================================================

(defalias Count            Int)
(defalias Nucleotide       (U (Val \A) (Val \C) (Val \G) (Val \T)))
(defalias NucleotideCounts (Map Nucleotide Count))


;;;; == ANNOTATION HACKS =======================================================

;; NOTE: This is NOT universally valid, but it works for nucleotide-count
(ann ^:no-check clojure.core/update
     [NucleotideCounts Nucleotide [Count -> Count] -> NucleotideCounts])


;; == PRIVATE API ==============================================================

(ann ^:no-check dna-nucleotides (HSet #{\A \C \G \T}))
(def ^:private dna-nucleotides (set "ACGT"))


;; == PUBLIC API ===============================================================

(ann nucleotide-counts (IFn [-> (Map Nucleotide (Val 0))]
                            [String -> NucleotideCounts]))
(defn nucleotide-counts
  ([] (zipmap dna-nucleotides (repeat 0)))
  ([strand]
   (reduce (fn [freqs :- NucleotideCounts, c :- Character] :- NucleotideCounts
             (if-let [nucleotide (dna-nucleotides c)]
               (update freqs nucleotide inc)
               freqs))
           (nucleotide-counts) strand)))

(ann count [Nucleotide String -> Int])
(defn count [nucleotide strand]
  (if (dna-nucleotides nucleotide)
    (reduce (fn [cnt :- Int, c :- Character] :- Int
              (cond-> cnt (= c nucleotide) inc))
            0 strand)
    (throw (Exception. (format "%s is an invalid nucleotide."
                               (pr-str nucleotide))))))


;; == EMACS CONFIG =============================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

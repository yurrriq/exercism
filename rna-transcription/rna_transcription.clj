(ns rna-transcription
  "Transcribing DNA strands to their RNA complements."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [Map Seq U Val ann defalias]]))


;;;; ==== TYPE ALIASES =========================================================

(defalias DNA (U (Val \A) (Val \C) (Val \G) (Val \T)))

(defalias DNAStrand (Seq DNA))

(defalias RNA (U (Val \A) (Val \C) (Val \G) (Val \U)))

(defalias RNAStrand (Seq RNA))


;;;; ==== PRIVATE API ==========================================================

(ann dna->rna (Map DNA RNA))
(def ^:private dna->rna
  "A map from DNA nucleotide to RNA complement."
  {\A \U
   \C \G
   \G \C
   \T \A})

(ann invalid-nucleotide [Character -> Error])
(defn- invalid-nucleotide
  "Throw a self-describing `AssertionError` for an invalid DNA nucleotide."
  [n]
  (throw (AssertionError. (str "Invalid nucleotide: " n))))

(ann transcribe [(U Character DNA) -> (U Error RNA)])
(defn- transcribe
  "Given a valid DNA nucleotide, return its RNA complement per [[dna->rna]].
  If the input is not valid, call [[invalid-nucleotide]] on it."
  [dna]
  (or (dna->rna dna)
      (invalid-nucleotide dna)))


;;;; ==== PUBLIC API ===========================================================

(ann to-rna [DNAStrand -> RNAStrand])
(defn to-rna
  "Given a DNA strand, translate each nucleotide to RNA and return the result,
  throwing an `AssertionError` if any invalid nucleotide is found."
  [dna]
  (apply str (map transcribe dna)))


;;;; ==== EMACS CONFIG =========================================================

;; Local Variables:
;; mode: clojure
;; mode: typed-clojure
;; End:

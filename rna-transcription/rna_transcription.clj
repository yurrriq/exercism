(ns rna-transcription
  "Transcribing DNA strands to their RNA complements."
  {:author "Eric Bailey"})

(def ^:private dna->rna
  "A map from DNA nucleotide to RNA complement."
  {\A \U
   \C \G
   \G \C
   \T \A})

(defn- invalid
  "Throw a self-describing `AssertionError` for and invalid DNA nucleotide."
  [n] (throw (AssertionError. (str "Invalid nucleotide: " n))))

(defn to-rna
  "Given a DNA strand, translate each nucleotide to RNA and return the result,
  throwing an `AssertionError` if any invalid nucleotide is found."
  [dna]
  (apply str (map (some-fn dna->rna invalid) dna)))

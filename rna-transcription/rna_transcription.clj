(ns rna-transcription)

(defn transcribe [nucleotide]
  (case nucleotide
    \G \C
    \C \G
    \T \A
    \A \U
    (throw (AssertionError. "Invalid DNA strand."))))

(defn to-rna [dna]
  (loop [dna dna
         rna []]
    (let [nucleotide (transcribe (first dna))]
      (if-let [remaining (not-empty (rest dna))]
        (recur remaining
               (conj rna nucleotide))
        (apply str (conj rna nucleotide))))))

(ns rna-transcription)

(defn transcribe [nucleotide]
  (case nucleotide
    \G \C
    \C \G
    \T \A
    \A \U
    (throw (AssertionError. "Invalid DNA strand."))))

(defn to-rna [dna]
  (loop [to-convert dna rna (transient [])]
    (let [nucleotide (transcribe (first to-convert))
          rna        (conj! rna nucleotide)]
      (if-let [remaining (not-empty (rest to-convert))]
        (recur remaining rna)
        (apply str (persistent! rna))))))

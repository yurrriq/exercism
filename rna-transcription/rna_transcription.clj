(ns rna-transcription)

(defn to-rna [dna]
  (let [invalid #(throw (AssertionError. "Invalid DNA strand."))]
    (apply str (map #(case % \G \C, \C \G, \T \A, \A \U, (invalid)) dna))))

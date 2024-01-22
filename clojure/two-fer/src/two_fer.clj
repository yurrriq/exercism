(ns two-fer
  "The Two Fer exercise from Exercism")

(defn two-fer
  ([]
   (two-fer "you"))
  ([name]
    (format "One for %s, one for me." name)))

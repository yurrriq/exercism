(defmodule rna-transcription
  (export (to-rna 1)))

(defun to-rna (strand)
  (lists:map
    (match-lambda
      ([#\G] #\C)
      ([#\C] #\G)
      ([#\T] #\A)
      ([#\A] #\U))
    strand))

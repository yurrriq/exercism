(define-module (dna)
  #:export (to-rna)
  #:use-module (ice-9 hash-table))

(define dna->rna
  '((#\G . #\C)
    (#\C . #\G)
    (#\T . #\A)
    (#\A . #\U)))

(define (complement nucleotide)
  "Given a DNA nucleotide, returns its RNA complement,
  or shows an error message when an invalid nucleotide is given."
  (define (invalid nucleotide)
    (error (format #f "Invalid nucleotide: ~a" nucleotide)))
  (or (cdr (assoc nucleotide dna->rna))
      (invalid nucleotide)))

(define (to-rna dna)
  "Given a DNA strand, returns its transcribed RNA strand."
  (string-map complement dna))

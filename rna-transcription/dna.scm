(define-module (dna)
  #:export (to-rna)
  #:use-module (ice-9 hash-table))

(define hash-dna->rna
  (alist->hash-table
   '((#\G . #\C)
     (#\C . #\G)
     (#\T . #\A)
     (#\A . #\U))))

(define (complement nucleotide)
  "Given a DNA nucleotide, returns its RNA complement,
  or shows an error message when an invalid nucleotide is encountered."
  (define (invalid nucleotide)
    (error (format #f "Invalid nucleotide: ~a" nucleotide)))
  (or (hash-ref hash-dna->rna nucleotide)
      (invalid nucleotide)))

(define (to-rna dna)
  "Given a DNA strand, returns its transcribed RNA strand."
  (list->string (map complement (string->list dna))))

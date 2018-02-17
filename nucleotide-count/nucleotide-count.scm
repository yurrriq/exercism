(define-module (nucleotide-count)
  #:export (nucleotide-counts dna-count)
  #:autoload (srfi srfi-1) (fold))

(define valid-nucleotides (string->list "ACGT"))

(define (validate nucleotide)
  (if (memv nucleotide valid-nucleotides)
      nucleotide
      (error "Invalid nucleotide")))

(define (dna-count nucleotide strand)
  (validate nucleotide) ; If no error, throw away results
  (fold (λ (nucleotide* sum)
          (if (char=? (validate nucleotide*) nucleotide)
              (1+ sum)
              sum))
        0
        (string->list strand)))

(define (nucleotide-counts strand)
  (map (λ (nucleotide)
         `(,nucleotide . ,(dna-count nucleotide strand)))
       valid-nucleotides))

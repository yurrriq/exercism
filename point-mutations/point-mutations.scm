(define-module (point-mutations)
  #:export (hamming-distance))

(define (hamming-distance x y)
  (if (eqv? (string-length x) (string-length y))
      (length (filter not (map char=? (string->list x) (string->list y))))
      (error "Can't calculate distance for strings of unequal length.")))

#lang racket

(require (only-in srfi/13 string-fold))

(provide hamming-distance)

(define (hamming-distance input against)
  (if (= (string-length input) (string-length against))
      (foldl hamming-distance* 0 (string->list input) (string->list against))
      (error "String length mismatch.")))

(define (hamming-distance* char-a char-b distance)
  (if (char=? char-a char-b) distance (add1 distance)))

(define-module (squares)
  #:export   (sum-of-squares square-of-sums difference)
  #:autoload (srfi srfi-1) (reduce iota))

(define (square x) (expt x 2))

(define (square-of-sums x) (square (reduce + 0 (iota x 1))))

(define (sum-of-squares x) (reduce (λ (y sum) (+ sum (square y))) 0 (iota x 1)))

(define (difference x) (- (square-of-sums x) (sum-of-squares x)))

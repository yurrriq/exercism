(define-module (raindrops)
  #:export (convert))

;; Notes
;; =====
;; - No mutated state, because gross.
;; - Internal procedures are highly specialized and nearly useless elswhere,
;;   hence they're locally scoped.
;; - "Premature optimization is the root of all evil."
;;   Whatever, man.

(define (convert n)
  (define (number->raindrops n)
    "Given a number, returns a possibly empty list of raindrops."
    (define (prime-factors* n)
      "Given a number, returns a possibly empty list of its prime factors
      from 3 to 7, inclusive."
      (let recur ((n n) (divisor 3) (acc (list)))
        (cond
         ((or (= n 1) (> divisor 7)) acc)
         ((zero? (modulo n divisor)) (recur (/ n divisor) 3 (cons divisor acc)))
         (else                       (recur n (1+ divisor) acc)))))
    (define (factors->raindrops factors<=7)
      "Given a list of factors, returns a possibly empty list of the
      appropriate raindrops."
      (filter string?
              (list (when (member 3 factors<=7) "Pling")
                    (when (member 5 factors<=7) "Plang")
                    (when (member 7 factors<=7) "Plong"))))
    (factors->raindrops (prime-factors* n)))
  (let ((raindrops (number->raindrops n)))
    (if (null? raindrops)
        (number->string n)
        (string-concatenate raindrops))))

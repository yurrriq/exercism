(define-module (hello-world)
  #:export (hello))

(define* (hello #:optional (name "world"))
  (format #f "Hello, ~a!" name))

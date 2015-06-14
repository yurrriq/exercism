(define-module (hello-world)
  #:export (hello))

(define hello
  (lambda* (#:optional (name "world"))
    (format #f "Hello, ~a!" name)))

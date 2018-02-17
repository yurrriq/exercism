(define-module (list-ops)
  #:export (my-length
            my-reverse
            my-map
            my-filter
            my-fold
            my-append
            my-concatenate))

(define (my-length lst) (my-fold (lambda (_ acc) (1+ acc)) 0 lst))

(define (my-reverse lst) (my-fold cons '() lst))

(define (my-map proc lst) (map-cps proc lst identity))

(define (my-filter pred lst) (filter pred lst))

(define (my-fold proc init lst)
  (if (null? lst) init
      (my-fold proc (proc (car lst) init) (cdr lst))))

(define (my-append lst1 lst2) (append-cps lst1 lst2 identity))

(define (my-concatenate lol) (foldr-cps my-append '() lol))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(define (append-cps lst1 lst2 k)
  (if (null? lst1) (k lst2)
      (append-cps (cdr lst1) lst2 (lambda (x) (k (cons (car lst1) x))))))

(define (map-cps proc lst k)
  (if (null? lst) (k '())
      (map-cps proc (cdr lst) (lambda (x) (k (cons (proc (car lst)) x))))))

(define (foldr-cps proc init lst)
  (letrec ((foldr-cps* (lambda (lst2 k)
                         (if (null? lst2) (k init)
                             (foldr-cps* (cdr lst2)
                                         (lambda (x)
                                           (k (proc (car lst2) x))))))))
    (foldr-cps* lst identity)))


;; Local Variables:
;; compile-command: "guile list-ops-test.scm"
;; End:

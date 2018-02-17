(define-module (robot)
  #:export (build-robot
            robot-name
            reset-name)
  #:autoload (srfi srfi-1) (iota))

(use-modules (ice-9 hash-table) (srfi srfi-42))

(define (rand-nth lst)
  (list-ref lst (random (length lst))))

(define (random-char-alpha-upcase)
  (rand-nth (list-ec (:char-range c #\A #\Z) c)))

(define (random-char-digit)
  (rand-nth (list-ec (:char-range c #\0 #\9) c)))

(define (random-name)
  (string
   (random-char-alpha-upcase)
   (random-char-alpha-upcase)
   (random-char-digit)
   (random-char-digit)
   (random-char-digit)))

#! (list->string
    (concatenate
     `(,(list-ec (: _ 2) (random-char-alpha-upcase))
       ,(list-ec (: _ 3) (random-char-digit))))) !#

(define (build-robot)
  (alist->hash-table `((name . ,(random-name)))))

(define (robot-name robot)
  (hash-ref robot 'name))

(define (reset-name robot)
  (hash-set! robot 'name (random-name)))


;; Local Variables:
;; compile-command: "guile robot-name-test.scm"
;; End:

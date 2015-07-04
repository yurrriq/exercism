(define-module (phone-number)
  #:export (numbers area-code pprint)
  #:autoload (ice-9 format) (format)
  #:autoload (srfi srfi-1)  (split-at)
  #:autoload (srfi srfi-11) (let*-values))

(define (explode-digits digit-string)
  "Given a string of digits (see: `numbers`), returns a list of strings,
  representing the area code, exchange code and subscriber number, in order."
  (letrec ((l (string->list digit-string))
           (f (lambda (lst) (split-at lst 3))))
    (let*-values (((x h) (f l))
                  ((y z) (f h)))
      (map list->string `(,x ,y ,z)))))

(define (numbers number-string)
  "Given an potentially unsanitized phone number as a string, attempts to
  sanitize it and return a string of only digits. If the number is invalid,
  returns \"0000000000\" (ten zeros)."
  (let* ((digits (string-filter char-numeric? number-string))
         (n      (string-length digits)))
    (cond
     ((= n 10) digits)
     ((and (= n 11) (char=? (string-ref digits 0) #\1)) (substring digits 1))
     (else "0000000000"))))

(define (area-code number-string)
  "Given a potentially unsanitized phone number as a string, calls `numbers` on
  it and returns the area. If the number is invalid, returns \"000\".

  Note: This is similar to the `car` of a call to `explode-digits`, except
  `explode-digits` expects a santized phone number."
  (substring (numbers number-string) 0 3))

(define (pprint number-string)
  "Given a potentially unsanitized phone number as a string, calls `numbers`
  then `explode-digits` on it, prety printing the results. If the number is
  invalid, returns \"(000) 000-0000\"."
  (let ((digits (numbers number-string)))
    (apply format #f "(~a) ~a-~a"
           (explode-digits digits))))

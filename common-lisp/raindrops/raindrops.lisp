(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defconstant number->sound
  '((3 . "Pling")
    (5 . "Plang")
    (7 . "Plong")))

(defun divides-p (d n)
  (zerop (rem n d)))

(defun convert (number)
  "Convert a number to a string of raindrop sounds."
  (let ((sounds (reduce (lambda (sounds divisor-sound)
                          (if (divides-p (car divisor-sound) number)
                              (concatenate 'string sounds (cdr divisor-sound))
                              sounds))
                        number->sound
                        :initial-value "")))
    (if (= 0 (length sounds))
        (format nil "~d" number)
        sounds)))

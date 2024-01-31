(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :alexandria))

(defpackage :raindrops
  (:use :cl :alexandria)
  (:export :convert))

(in-package :raindrops)

(defconstant +raindrops+
  '((3 . "Pling")
    (5 . "Plang")
    (7 . "Plong")))

(defun dividesp (divisor number)
  "Returns true if number is divisible by divisor; otherwise returns false."
  (zerop (rem number divisor)))

(defun convert (number)
  "Converts a number to a string of raindrop sounds."
  (labels ((makes-sound-p (divisor)
             (dividesp divisor number))
           (collect-sounds (divisor-sound sounds)
             (if (makes-sound-p (car divisor-sound))
                 (cons (cdr divisor-sound) sounds)
                 sounds)))
    (if-let ((sounds (reduce #'collect-sounds +raindrops+
                             :from-end t :initial-value '())))
      (apply #'concatenate 'string sounds)
      (write-to-string number))))

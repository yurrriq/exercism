(defpackage #:leap
  (:use #:common-lisp)
  (:export #:leap-year-p))
(in-package #:leap)

(defun leap-year-p (year)
  (and (zerop (rem year 4))
       (or (not (zerop (rem year 100)))
           (zerop (rem year 400)))))

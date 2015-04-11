(defpackage #:leap
  (:use #:common-lisp)
  (:export #:leap-year-p))
(in-package #:leap)

(defun leap-year-p (year)
  (cond ((zerop (rem year 400)) t)
	((zerop (rem year 100)) nil)
	((zerop (rem year   4))   t)
	(t nil)))

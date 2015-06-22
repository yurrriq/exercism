;; -*- compile-command: "sbcl --non-interactive --load grains-test.lisp" -*-

(in-package #:cl-user)
(defpackage #:grains
  (:use #:cl)
  (:export :square :total))
(in-package #:grains)

(defun square (n)
  (expt 2 (1- n)))

(defun total ()
  (1- (square 65)))

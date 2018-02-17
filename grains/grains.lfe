(defmodule grains
  (export (square 1)
          (total 0)))

(defun 1- (n)
  (- n 1))

(defun 2^ (x)
  (trunc (: math pow 2 x)))

(defun square (n)
  (trunc (2^ (1- n))))

(defun total ()
  (1- (square 65)))

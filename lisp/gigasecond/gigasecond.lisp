;; -*- compile-command: "sbcl --non-interactive --load gigasecond-test.lisp" -*-

(ql:quickload "cl-arrows")

(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl #:cl-arrows)
  (:export #:from))
(in-package #:gigasecond)

(defconstant gigasecond 1000000000 "The number one billion")

(defun take* (sequence end)
  "Sort of like a strict version of #'clojure.core/take."
  (subseq sequence 0 end))

(defun from (year month date hour minute second)
  "Given a year, month, date, hour, minute a second, returns a list of the
  same arguments a gigasecond later."
  (-> (encode-universal-time second minute hour date month year 0)
      (+ gigasecond)
      (decode-universal-time 0)
      (multiple-value-list)
      (take* 6)
      (reverse)))

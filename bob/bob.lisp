;; -*- compile-command: "sbcl --non-interactive --load bob-test.lisp" -*-

(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:response-for))
(in-package #:bob)

(defun question-p (prompt)
  (char-equal (char prompt (1- (length prompt))) #\?))

(defun silent-p (prompt)
  (zerop (length (string-trim #(#\Space #\Tab) prompt))))

(defun yelled-p (prompt)
  (and (some #'upper-case-p prompt)
       (string= prompt (string-upcase prompt))))

(defun response-for (prompt)
  (cond ((silent-p prompt)   "Fine. Be that way!")
        ((yelled-p prompt)   "Whoa, chill out!")
        ((question-p prompt) "Sure.")
        (t "Whatever.")))

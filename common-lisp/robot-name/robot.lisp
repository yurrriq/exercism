(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)

(defun random-char (start distance)
  (cond ((not (characterp start))
         (error "~S is not of type character." start))
        ((not (integerp distance))
         (error "~S is not of type integer." distance))
        ((not (plusp distance))
         (error "~S is not greater than zero." distance))
        (t (code-char (+ (char-code start) (random distance))))))

(defun random-upper-case-alpha-char () (random-char #\A 26))

(defun random-digit-char () (random-char #\0 10))

(defun random-name ()
  "Return a random robot name of the form `[A-Z]{2}[0-9]{3}'."
  (concatenate 'string
               (list (random-upper-case-alpha-char)
                     (random-upper-case-alpha-char)
                     (random-digit-char)
                     (random-digit-char)
                     (random-digit-char))))

(defclass robot ()
  ((name
    :initform (random-name)
    :accessor robot-name)))

(defun build-robot ()
  "Return a new `robot' instance."
  (make-instance 'robot))

(defgeneric reset-name (robot)
  (:documentation "Given a `ROBOT', reset its name to a `random-name'."))

(defmethod reset-name ((robot robot))
  (setf (robot-name robot) (random-name)))

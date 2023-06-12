;;; robot-name.el --- Robot Name (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

(defun random-upper-case-alpha-char ()
  (nth (random 26) (number-sequence ?A ?Z)))

(defun random-digit-char ()
  (nth (random 10) (number-sequence ?0 ?9)))

(defun random-name ()
  "Return a random robot name of the form `[A-Z]{2}[0-9]{3}'."
  (concat (list (random-upper-case-alpha-char)
                (random-upper-case-alpha-char)
                (random-digit-char)
                (random-digit-char)
                (random-digit-char))))

(defun build-robot ()
  "Build a new robot with a random name."
  (list :name (random-name)))

(defun robot-name (robot)
  "Get the ROBOT's name."
  (plist-get robot :name))

(defun reset-robot (robot)
  "Reset the name of ROBOT.  Factory reset!"
  (plist-put robot :name nil))

(provide 'robot-name)
;;; robot-name.el ends here

;;; gigasecond.el --- Gigasecond exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs's handling of time zones and dst
;; in the encode-time and decode-time functions.

;;; Code:

(defconst +gigasecond+ (seconds-to-time (expt 10 9))
  "One gigasecond (10^9 seconds) as a Lisp timestamp.")

(defmacro -> (&rest body)
  "The Thrush combinator from To Mock a Mockingbird."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun from (seconds minutes hours day-of-month month year)
  "Calculate the date one gigasecond (10^9 seconds) from the given date."
  (-> (list seconds minutes hours day-of-month month year nil nil t)
      (encode-time)
      (time-add +gigasecond+)
      (decode-time t)
      (butlast 3)))

(provide 'gigasecond)
;;; gigasecond.el ends here

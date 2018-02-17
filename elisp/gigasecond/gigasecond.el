;;; gigasecond.el --- Gigasecond exercise (exercism)

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs's handling of time zones and dst
;; in the encode-time and decode-time functions.

;;; Code:

(defun from (seconds minutes hours day-of-month month year)
  (set-time-zone-rule t)
  (butlast
   (decode-time
    (time-add (seconds-to-time (expt 10 9))
              (encode-time seconds minutes hours day-of-month month year)))
   3))

(provide 'gigasecond)
;;; gigasecond.el ends here

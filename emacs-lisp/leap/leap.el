;;; leap.el --- Leap exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun leap-year-p (year)
  "Determine with `year' is a leap year."
  (and (zerop (% year 4))
       (or (not (zerop (% year 100)))
           (zerop (% year 400)))))

(provide 'leap-year-p)
;;; leap.el ends here

;;; bob.el --- Bob exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'subr-x))

(defun response-for (prompt)
  "Given a `PROMPT', return a teenager's response."
  (cond ((string-blank-p prompt) "Fine. Be that way!")
        ((yelled-p prompt)       "Whoa, chill out!")
        ((question-p prompt)     "Sure.")
        (t                       "Whatever.")))

(defun question-p (prompt)
  "Return t iff `PROMPT' ends with `??'."
  (not (null (string-match "?\\'" prompt))))

(defun yelled-p (prompt)
  "Return t iff `PROMPT' is yelled.
A string is yelled iff there exists an uppercase letter and
`PROMPT' is equal to itself uppercased."
  (and (string-match "[[:upper:]]" prompt)
       (string= prompt (upcase prompt))))

(provide 'bob)
;;; bob.el ends here

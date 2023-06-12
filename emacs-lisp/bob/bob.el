;;; bob.el --- Bob exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'subr-x))

(defun response-for (prompt)
  "Given a `PROMPT', return a teenager's response."
  (let ((trimmed-prompt (string-trim prompt)))
    (cond ((string-blank-p trimmed-prompt)
           "Fine. Be that way!")
          ((question-p trimmed-prompt)
           (if (yelled-p trimmed-prompt)
               "Calm down, I know what I'm doing!"
             "Sure."))
          ((yelled-p trimmed-prompt)
           "Whoa, chill out!")
          (t "Whatever."))))

(defun question-p (prompt)
  "Return t iff `PROMPT' ends with `?'."
  (string-suffix-p "?" prompt))

(defun yelled-p (prompt)
  "Return t iff `PROMPT' is yelled.
A string is yelled iff there exists an uppercase letter and
`PROMPT' is equal to itself uppercased."
  (and (string-match "[[:upper:]]" prompt)
       (string= prompt (upcase prompt))))

(provide 'bob)
;;; bob.el ends here

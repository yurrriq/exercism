;;; matching-brackets.el --- Matching Brackets (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'rx)
(require 'subr-x)

(defun is-paired (value)
  (let ((only-brackets (replace-regexp-in-string (rx (not (in "[]{}()"))) "" value)))
    (or (string-empty-p only-brackets)
        (let ((pairs-removed (replace-regexp-in-string (rx (or "[]" "{}" "()")) "" only-brackets)))
          (or (string-empty-p pairs-removed)
              (when (not (string= only-brackets pairs-removed))
                (is-paired pairs-removed)))))))

(provide 'matching-brackets)
;;; matching-brackets.el ends here

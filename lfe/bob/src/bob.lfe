(defmodule bob
  (export (response-for 1)
          (question? 1)
          (silence? 1)
          (shouting? 1)))

(defun question? (phrase)
  (case (re:run phrase "\\?$")
    ((tuple 'match _) 'question)
    ('nomatch 'statement)))

(defun silence? (phrase)
  (case (string:strip phrase)
    ("" 'silence)
    (_ 'statement)))

(defun shouting? (phrase)
  (case (and (== phrase (string:to_upper phrase))
             (!= (string:to_upper phrase) (string:to_lower phrase)))
    ('true 'shouting)
    ('false 'statement)))

(defun response-for (phrase)
  (case (lists:foldl (lambda (f acc) (min acc (funcall f phrase)))
          phrase (list #'silence?/1 #'shouting?/1 #'question?/1))
    ('silence "Fine. Be that way!")
    ('shouting "Whoa, chill out!")
    ('question "Sure.")
    ('statement "Whatever.")))

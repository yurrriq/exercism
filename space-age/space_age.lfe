(defmodule space_age
  (export (ageOn 2)))

(defun ageOn (planet seconds)
  (/ seconds (secondsInYear planet)))

(defun secondsInYear
  (('earth)
   (* 365.25 24 60 60))
  (('mercury)
   (secondsInYear 0.2408467))
  (('venus)
   (secondsInYear 0.61519726))
  (('mars)
   (secondsInYear 1.8808158))
  (('jupiter)
   (secondsInYear 11.862615))
  (('saturn)
   (secondsInYear 29.447498))
  (('uranus)
   (secondsInYear 84.016846))
  (('neptune)
   (secondsInYear 164.79132))
  ((n)
   (* n (secondsInYear 'earth))))


;; lfec space_age.lfe; and erlc space_age_tests.erl; and \
;; erl -noshell -eval "eunit:test(space_age_tests, [verbose])" -s init stop

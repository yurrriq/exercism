(defmodule space_age
  (export (ageOn 2)))

(defun ageOn (planet seconds)
  (/ seconds (secondsInYear planet)))

(defun secondsInYear
  (('earth)
   (* 365.25 24 60 60))
  (('mercury)
   (* 0.2408467 (secondsInYear 'earth)))
  (('venus)
   (* 0.61519726 (secondsInYear 'earth)))
  (('mars)
   (* 1.8808158 (secondsInYear 'earth)))
  (('jupiter)
   (* 11.862615 (secondsInYear 'earth)))
  (('saturn)
   (* 29.447498 (secondsInYear 'earth)))
  (('uranus)
   (* 84.016846 (secondsInYear 'earth)))
  (('neptune)
   (* 164.79132 (secondsInYear 'earth))))


;; lfec space_age.lfe; and erlc space_age_tests.erl; and \
;; erl -noshell -eval "eunit:test(space_age_tests, [verbose])" -s init stop

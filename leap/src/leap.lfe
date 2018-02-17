(defmodule leap
  "Determining whether a year is a leap year."
  (export (leap-year 1)))

(defmacro divides? (d n) `(=:= (rem ,n ,d) 0))

(defun leap-year
  "Return true iff `year` is a leap year."
  ([year]  (when (divides? 400 year)) 'true)
  ([year]  (when (divides? 100 year)) 'false)
  ([year]  (when (divides?   4 year)) 'true)
  ([_year]                            'false))

(ns leap
  "Determining if a year is a leap year."
  {:author "Eric Bailey"})

(defn- divides?
  "Given a denominator `d` and a numerator `n`, return `true` if `d | n`,
  otherwise `false`."
  [d n]
  (zero? (rem n d)))

(defn leap-year?
  "Given a year, return `true` if it is a leap year, otherwise `false`."
  [year]
  (and (divides? 4 year)
       (or (not (divides? 100 year))
           (divides? 400 year))))

(ns gigasecond
  (:import (java.util Calendar GregorianCalendar TimeZone)))

(defn from
  "Given a year, month and date, return a vector of the same arguments a
  gigasecond later."
  [year month day]
  ((juxt #(.get % Calendar/YEAR)
         #(inc (.get % Calendar/MONTH))
         #(.get % Calendar/DATE))
   (doto (GregorianCalendar. (TimeZone/getTimeZone "US/Central"))
     (.set Calendar/YEAR   year)
     (.set Calendar/MONTH  (dec month))
     (.set Calendar/DATE   (dec day))
     (.add Calendar/SECOND 1e9))))

(ns meetup
  "Calculating the date of meetups."
  {:author "Eric Bailey"}
  ;; (:require [clojure.set :refer [map-invert]])
  (:import (java.util Calendar GregorianCalendar)))

;; == PRIVATE API ==============================================================

(defn- schedule
  "TODO: write docstring"
  ([sched]
   (letfn [(nth-day
             ([n]   (fn [x] (nth-day x n)))
             ([x n] (+ (inc x) (* 7 (dec n)))))
           (teenth [x]
             (->> (map #(nth-day x %) '(1 2 3 4))
                  (filter #(>= % 13))
                  (first)))]
     ({:first  (nth-day 1)
       :second (nth-day 2)
       :third  (nth-day 3)
       :fourth (nth-day 4)
       :teenth teenth}
      sched)))
  ([offset sched]
   ((schedule sched) offset)))

(def ^:private weekday
  "A bidirectional map from numeric and keyword representations of
  days of the week, where [1 ↔ :sunday, ..., 7 ↔ :saturday]."
  (-> (comp (juxt (comp vec reverse) identity) vector)
      (mapcat [:sunday :monday :tuesday :wednesday :thursday :friday :saturday]
              (iterate inc 1))
      (->> (into {}))))

(defn- to-gregorian
  "Given a month (where January is 1), year and day,
  return the GregorianCalendar representation."
  [month year day]
  (doto (GregorianCalendar.)
    (.set Calendar/YEAR  year)
    (.set Calendar/MONTH (dec month))
    (.set Calendar/DATE  day)))

(defn- day-of-week
  "Given a GregorianCalendar, return the
  numeric representation of its [[weekday]]."
  [gc]
  (.get gc Calendar/DAY_OF_WEEK))

(defn- diff-weekday
  "Given two numeric representations of a [[weekday]], return the
  distance from the first to the second, going forward in time."
  [from to]
  (-> (- to from) (+ 7) (rem 7)))

(defn- first-day-of-week
  "Given a month and a year, return the [[weekday]] of the
  first day of the given month in the given year."
  [month year]
  (day-of-week (to-gregorian month year 1)))

(defn- last-weekday
  "Given a month and a year, return a vector pair of
  the last date in the month and a numeric representation
  of which [[weekday]] it is."
  [month year]
  (let [day (-> (to-gregorian month year 1) (.getActualMaximum Calendar/DATE))]
    [day (day-of-week (to-gregorian month year day))]))

(defn- last-day
  "Given a month, year, and numeric representation of a [[weekday]],
  return a vector pair of the last date in the month and the date of
  the last occurrence of the given [[weekday]]."
  [month year day]
  (let [[to from] (last-weekday month year)]
    (- to (diff-weekday (weekday day) from))))


;; == PUBLIC API ===============================================================

(defn meetup
  "Given a month, year, and keywords representing a day and a schedule,
  return a triple of the year, month and the date of the specified occurrence of
  the given day."
  [month year day sched]
  (->> (if (= :last sched)
         (last-day month year day)
         (-> (first-day-of-week month year)
             (diff-weekday (weekday day))
             (schedule sched)))
       (conj [year month])))

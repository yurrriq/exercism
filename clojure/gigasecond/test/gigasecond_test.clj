(ns gigasecond-test
  (:require [clojure.test :refer :all]
            [gigasecond]))

(deftest from-apr-25-2011
  (is (= [2043 1 1] (gigasecond/from 2011 4 25))))

(deftest from-jun-13-1977
  (is (= [2009 2 19] (gigasecond/from 1977 6 13))))

(deftest from-jul-19-1959
  (is (= [1991 3 27] (gigasecond/from 1959 7 19))))

(deftest lovecraft-birthday
  (is (= [1922 4 29] (gigasecond/from 1890 8 20))))

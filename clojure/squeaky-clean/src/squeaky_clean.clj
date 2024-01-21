(ns squeaky-clean
  (:require [clojure.string :as str]))

(defn clean
  "Return a squeaky clean copy of a given string."
  [dirty-str]
  (-> dirty-str
      ;; 1. Replace any spaces encountered with underscores
      (str/replace #"\s" "_")
      ;; 2. Replace control characters with the upper case string "CTRL"
      (str/replace #"\p{IsControl}" "CTRL")
      ;; 3. Convert kebab-case to camelCase
      (str/replace #"-(\p{IsLowercase})" #(str/upper-case (%1 1)))
      ;; 4. Omit characters that are not letters
      ;; 5. Omit Greek lower case letters
      (str/replace #"[^_\p{IsLetter}]|[\p{IsGreek}&&\p{IsLowerCase}]" "")))

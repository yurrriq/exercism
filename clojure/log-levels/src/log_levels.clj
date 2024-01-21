(ns log-levels
  (:require [clojure.string :as str]))

(defn message
  "Given a string representing a log line,
  return its message with whitespace trimmed."
  [log-line]
  (let [[_ msg] (str/split log-line #":" 2)]
    (str/trim msg)))

(defn log-level
  "Given a string representing a log line,
   return its level in lower-case."
  [log-line]
  (let [[_ level] (re-find #"\[(\w+)\]" log-line)]
    (str/lower-case level)))

(defn reformat
  "Given a string representing a log line,
  format it with the message first and the log level in parentheses."
  [log-line]
  (let [msg (message log-line)
        level (log-level log-line)]
    (format "%s (%s)" msg level)))

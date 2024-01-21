(ns log-levels
  (:require [clojure.string :as str]))

(def ^:private log-line-regex
  #"^\[(ERROR|INFO|WARNING)\]:\s*(.+)")

(defn- parse-log-line
  [log-line]
  (rest (re-find log-line-regex log-line)))

(defn message
  "Given a string representing a log line,
  return its message with whitespace trimmed."
  [log-line]
  (let [[_ msg] (parse-log-line log-line)]
    (str/trim msg)))

(defn log-level
  "Given a string representing a log line,
   return its level in lower-case."
  [log-line]
  (let [[level _] (parse-log-line log-line)]
    (str/lower-case level)))

(defn reformat
  "Given a string representing a log line,
  format it with the message first and the log level in parentheses."
  [log-line]
  (let [msg (message log-line)
        level (log-level log-line)]
    (format "%s (%s)" msg level)))

(ns bob
  "Mimicking the conversational ineptitude of a lackadaisical teenager."
  {:author "Eric Bailey"}
  (:require [clojure.string :as s]))

(defn- question?
  "Given a prompt, return `true` if it ends with `?`, otherwise `false`."
  [prompt]
  (.endsWith prompt "?"))

(defn- yelled?
  "Given a prompt, return `true` if there exists an uppercase letter and the
  prompt is equal to itself uppercased, otherwise `false`."
  [prompt]
  (and #(re-find #"[A-Z]" prompt)
       (= prompt (s/upper-case prompt))))

(defn response-for
  "Given a prompt, return a teenager's response."
  [prompt]
  (condp apply [prompt]
    s/blank?  "Fine. Be that way!"
    yelled?   "Whoa, chill out!"
    question? "Sure."
    :else     "Whatever."))

;;;; -*- mode: clojure; mode: typed-clojure -*-

(ns bob
  "Mimicking the conversational ineptitude of a lackadaisical teenager."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [ann defalias]]
            [clojure.string :as s]))


;;;=============================================================
;;; Type Aliases
;;;=============================================================

(ann Prompt String)
(defalias Prompt
  "A string."
  String)

(ann PromptPredicate [Prompt -> Boolean])
(defalias PromptPredicate
  "A function from [[Prompt]] to `Boolean`."
  [Prompt -> Boolean])

(ann Response String)
(defalias Response
  "A string."
  String)


;;;=============================================================
;;; Private API
;;;=============================================================

(ann silent? PromptPredicate)
(def ^:private silent? s/blank?)

(ann question? PromptPredicate)
(defn- question?
  "Given a prompt, return `true` if it ends with `?`, otherwise `false`."
  [^String prompt]
  (.endsWith prompt "?"))

(ann yelled? PromptPredicate)
(defn- yelled?
  "Given a prompt, return `true` if there exists an uppercase letter and the
  prompt is equal to itself uppercased, otherwise `false`."
  [prompt]
  (and (boolean (re-find #"\p{Lu}\p{M}*" prompt))
       (= prompt (s/upper-case prompt))))


;;;=============================================================
;;; Public API
;;;=============================================================

(ann response-for [Prompt -> Response])
(defn response-for
  "Given a prompt, return a teenager's response."
  [prompt]
  (condp apply [prompt]
    silent?   "Fine. Be that way!"
    yelled?   "Whoa, chill out!"
    question? "Sure."
    "Whatever."))

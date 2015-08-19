(ns bob
  "Mimicking the conversational ineptitude of a lackadaisical teenager."
  {:author "Eric Bailey"})

(defn response-for [prompt]
  "Given a prompt, return a teenager's response."
  (cond
    (re-matches #"^\s*$" prompt) "Fine. Be that way!"
    (and (re-find #"[a-zA-Z]" prompt)
         (= prompt (clojure.string/upper-case prompt))) "Whoa, chill out!"
    (.endsWith prompt "?") "Sure."
    :else "Whatever."))

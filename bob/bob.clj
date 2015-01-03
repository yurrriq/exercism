(ns bob)

(defn response-for [prompt]
  (cond
    (re-matches #"^\s*$" prompt) "Fine. Be that way!"
    (and (re-find #"[a-zA-Z]" prompt)
         (= prompt (clojure.string/upper-case prompt))) "Whoa, chill out!"
    (.endsWith prompt "?") "Sure."
    :else "Whatever."))

(ns reverse-string
  "The Reverse String exercise from Exercism")

(defn reverse-string
  "Return a string with the characters in a given string in reverse order."
  [s]
  (loop [chars s, reversed '()]
    (if-let [[c & cs] chars]
      (recur cs (cons c reversed))
      (apply str reversed))))

(ns binary
  "Converting binary number strings to their decimal equivalents."
  {:author "Eric Bailey"})

;; == NILABLE VERSION ==========================================================

(defn to-decimal
  "Given a string representing a binary number, return its decimal equivalent.

  With 0 as the initial value, perform a left-associative fold over the string,
  using case analysis on each character.  If a character is `\1` or `\0`, add
  its numeric value to double the current result.  Otherwise, when any character
  other than `\0` or `\1` is present, return 0."
  [string]
  (-> (reduce (fn [sum c]
                (when sum
                  (some-> (case c \1 1, \0 0, nil)
                    (+ (* 2 sum)))))
              0 string)
      (or 0)))


;; == MONADIC VERSION ==========================================================

(comment
  ;; http://funcool.github.io/cats/latest/
  (require '[cats.core [>>= foldm]]
           '[cats.monad.maybe :refer [from-maybe just nothing]])

  (defn to-decimal [string]
    (-> (foldm (fn [sum c]
                 (>>= (case c \1 (just 1), \0 (just 0), (nothing))
                      #(just (+ % (* 2 sum)))))
               0 string)
        (from-maybe 0))))

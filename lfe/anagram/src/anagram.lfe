;;; ============================================================ [ anagram.lfe ]

(defmodule anagram
  (doc "Find anagrams for a word in a list of candidates.")
  (export (find 2))
  (import (from lists (filter 2) (sort 1))
          (rename string ((to_lower 1) lower-case))))

;;; ==================================================================== [ API ]

(defun find (word candidates)
  "Return the sublist of `candidates` which are anagrams of `word`."
  (filter (anagram? (normalize word)) candidates))

;;; ===================================================== [ Internal functions ]

(defun anagram? (normalized-word)
  "Return a unary function that takes a `candidate`, normalizes it and
  returns true iff it is an anagram of `normalized-word`.

  See also: [[normalize/1]]"
  (lambda (candidate)
    (anagram? (normalize candidate) normalized-word)))

(defun anagram?
  "Given a normalized candidate and a normalized word,
  return true iff they are anagrams.

  See also: [[normalize/1]]"
  ([`#(,la ,sa) `#(,lb ,sb)]
   (andalso (=/= la lb) (=:= sa sb))))

(defun normalize (word)
  "Return a tuple of the form `#(lower-cased-word sorted-lower-cased-word).`"
  (let ((lowered (lower-case word)))
    (tuple lowered (sort lowered))))

;;; ==================================================================== [ EOF ]

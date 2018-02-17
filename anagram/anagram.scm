(define-module (anagram)
  #:export (anagrams-for)
  #:autoload (srfi srfi-1) (every)
  #:autoload (srfi srfi-26) (cute))

;;; ===== HELPER FUNCTIONS =====

(define (anagram? x y)
  (every (cute <> x y) (list string-length=? (negate string=?) same-letters?)))

(define (char-ci-sort str)
  (sort (string->list str) char-ci<?))

(define (same-letters? x y)
  (every char-ci=? (char-ci-sort x) (char-ci-sort y)))

(define (string-length=? x y)
  (= (string-length x) (string-length y)))


;;; ===== PUBLIC API =====

(define (anagrams-for word candidates)
  (filter (cute anagram? word <>) candidates))

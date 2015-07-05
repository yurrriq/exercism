(defmodule anagram
  (export (find 2)))

(defun find (word candidates)
  (let* ((word-length  (length word))
         (word-lowered (: string to_lower word))
         (word-sorted  (: lists sort word-lowered)))
    (: lists filter
      (lambda (candidate)
        (let ((candidate-lowered (: string to_lower candidate)))
          (andalso (and (/= word-lowered candidate-lowered)
                        (== word-length (length candidate)))
                   (: lists all
                     (lambda (z) (let (((tuple x y) z)) (== x y)))
                     (: lists zip
                       word-sorted
                       (: lists sort candidate-lowered))))))
      candidates)))

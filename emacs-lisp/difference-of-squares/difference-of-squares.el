;;; difference-of-squares.el --- Difference of Squares (exercism)

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun square (x)
  (expt x 2))

(defun square-of-sum (x)
  (square (cl-reduce #'+ (number-sequence 1 x))))

(defun sum-of-squares (x)
  (cl-reduce (lambda (sum y) (+ sum (square y)))
             (number-sequence 1 x)))

(defun difference (x)
  (- (square-of-sum x)
     (sum-of-squares x)))

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here

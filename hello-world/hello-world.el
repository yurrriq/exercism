;;; hello-world.el --- Hello World Exercise (exercism)

;;; Commentary:

;;; Code:

(defun hello (&optional name)
  "Say hello, optionally to `NAME'."
  (format "Hello, %s!" (or name "World")))

(provide 'hello-world)
;;; hello-world.el ends here

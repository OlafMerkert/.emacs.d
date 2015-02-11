(defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body)))

(defmacro defpar (var value)
  `(progn (defvar ,var)
          (setf ,var ,value)))

(provide 'cnf-utils)

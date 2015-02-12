(defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body)))

(defmacro defpar (var value)
  `(progn (defvar ,var)
          (setf ,var ,value)))

(defun flatten (x)
  (cond ((null x) nil)
        ((atom x)
         (list x))
        (t (append (flatten (car x))
                   (flatten (cdr x))))))

(provide 'cnf-utils)

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

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; get rid of any local keybindings for our precious global commands
(defvar clean-local-keybindings nil)

(defun clean-local-keybindings-hook ()
  (dolist (key clean-local-keybindings)
    (local-set-key (kdb key) nil)))

(add-hook 'after-change-major-mode-hook 'clean-local-keybindings-hook)

(provide 'cnf-utils)

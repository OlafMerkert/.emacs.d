(defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body)))

(put 'after-load 'lisp-indent-function 1)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(after-load\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face nil t))))

(defmacro defpar (var value)
  `(progn (defvar ,var)
          (setf ,var ,value)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(defpar\\)\\_>[ \t\n]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face nil t))))

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
    (local-set-key (kbd key) nil)))

(add-hook 'after-change-major-mode-hook 'clean-local-keybindings-hook)

(defmacro lambdai (&rest body)
  `(lambda () (interactive) ,@body))

(provide 'cnf-utils)

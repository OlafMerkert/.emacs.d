(require 'cnf-org)

;;; configure babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)))

(defun babel-language-p (language)
  (find language org-babel-load-languages :test 'string-equal
        :key (lambda (x) (symbol-name (car x)))))

(setq org-confirm-babel-evaluate
      (lambda (language body) (not (babel-language-p language)))
      ;; do not add leading whitespace for source-blocks after editing
      ;; (why would anybody want that?)
      org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-export-babel-evaluate nil)

(defun strip-blank-lines (str)
  "Remove all blank lines from the given string `str'."
  ; the space or tab at the beginning of is necessary, because we
  ; don't want (and need to) strip blank lines between top-level forms
  (replace-regexp-in-string "[\n]\+\\([ \t]\\)" "\n\\1" str))

(defun ob-py-strip-blank-lines (f session body &optional result-type result-params)
  (funcall f session (strip-blank-lines body) result-type result-params))

(advice-add 'org-babel-python-evaluate-session :around 'ob-py-strip-blank-lines)
;; note that this also removes blank lines in strings, where they could
;; be wanted. But for now, it is a decent workaround.


(provide 'cnf-org-babel)

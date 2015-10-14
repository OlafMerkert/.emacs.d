(require 'cnf-org)

;;; configure babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (sh . t)))

(defun babel-language-p (language)
  (and (not (string-equal language "sh"))
       (find language org-babel-load-languages :test 'string-equal
             :key (lambda (x) (symbol-name (car x))))))

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

(setf org-babel-sh-command "sh")

;; evaluating source blocks from org-src mode
(defun org-edit-src-evaluate-code-block ()
  (interactive)
  (org-src-in-org-buffer
   (org-babel-execute-maybe)))

(define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-evaluate-code-block)
(define-key org-src-mode-map (kbd "<f1>") 'org-edit-src-exit)

;; removing superfluous prompts in output
(defun strip-python-shell-prompt (string)
  (let ((regexp (concat "^\\(" python-shell-prompt-regexp "\\|"
                        python-shell-prompt-block-regexp "\\)*")))
    (if (string-match regexp string)
        (substring string (match-end 0))
        string)))

(advice-add 'org-babel-trim :filter-return 'strip-python-shell-prompt)

;; figure out if we are using sage
(defmacro org-src-value-in-org-buffer (&rest body)
  `(save-window-excursion
    (switch-to-buffer (marker-buffer org-edit-src-beg-marker))
    ,@body))

(defun org-src-turn-on-sage ()
  (when (setf sage (org-src-value-in-org-buffer sage))
    (turn-on-sage)))

(add-hook 'org-src-mode-hook 'org-src-turn-on-sage)

(provide 'cnf-org-babel)

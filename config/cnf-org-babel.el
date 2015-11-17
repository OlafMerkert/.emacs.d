(require 'cnf-org)

;;; configure babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (sh . t)
   (maxima . t)))

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
(defmacro org-src-value-in-org-buffer (&rest body)
  `(let ((beg org-src--beg-marker))
     (with-current-buffer (marker-buffer beg)
       (save-excursion
         (goto-char beg)
         ,@body))))

(defun org-edit-src-evaluate-code-block ()
  (interactive)
  (org-edit-src-save)
  (org-src-value-in-org-buffer
   (org-babel-execute-maybe)))

(defun org-src-eval-and-next ()
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-babel-next-src-block))

;;; a function to paste the contents of current code block into other
;;; buffer. Essentially, this is a universal poor man's session
;;; support.
(defun org-src-block-append-other-buffer ()
  (interactive)
  (org-babel-when-in-src-block
   (let ((contents (org-element-property :value (org-element-at-point))))
     (other-window 1)
     (end-of-buffer)
     (insert contents))))

(defun org-edit-src-exit-and-eval ()
  (interactive)
  (org-edit-src-exit)
  (org-babel-execute-maybe))

(defhydra org-src-actions (org-mode-map "<f5>")
  "src block:"
  ("r" org-babel-execute-maybe "eXec")
  ("s" org-babel-execute-subtree "eXec subtree")
  ("e" org-edit-special "Edit" :color blue)
  ("a" ob-abort-sage-calculation "Abort calculation")
  ("p" org-babel-previous-src-block "Previous")
  ("n" org-babel-next-src-block "Next")
  ("P" org-backward-heading-same-level "Previous heading")
  ("N" org-forward-heading-same-level "Next heading")
  ("d" org-babel-demarcate-block "Split")
  ("z" org-babel-switch-to-session "repl" :color blue)
  ("k" org-babel-remove-result "remove result")
  ("x" org-src-eval-and-next)
  ("o" org-src-block-append-other-buffer "append other buffer" )
  ("q" nil "quit"))

(defhydra org-src-edit-actions (org-src-mode-map "<f5>")
  "src edit:"
  ("r" org-edit-src-evaluate-code-block "eXec")
  ;; TODO exit, eval and next
  ("e" org-edit-src-exit "close Edit" :color blue)
  ("x" org-edit-src-exit-and-eval "close and eval" :color blue)
  ("a" org-src-abort-sage-calculation "Abort calculation")
  ("k" org-babel-remove-result "remove result")
  ("q" nil "quit"))

;; removing superfluous prompts in output
(defun strip-python-shell-prompt (string)
  (let ((regexp (concat "^\\(" python-shell-prompt-regexp "\\|"
                        python-shell-prompt-block-regexp "\\)*")))
    (if (string-match regexp string)
        (substring string (match-end 0))
        string)))

(advice-add 'org-babel-trim :filter-return 'strip-python-shell-prompt)

;; figure out if we are using sage
(defun org-src-turn-on-sage ()
  (when (setf sage (org-src-value-in-org-buffer sage))
    (turn-on-sage)))

(defun ob-abort-sage-calculation ()
  (interactive)
  (save-window-excursion
    (org-babel-when-in-src-block
    (org-babel-switch-to-session)
    (comint-interrupt-subjob))))

(defun org-src-abort-sage-calculation ()
  (interactive)
  (org-src-value-in-org-buffer
   (ob-abort-sage-calculation)))


(add-hook 'org-src-mode-hook 'org-src-turn-on-sage)

(provide 'cnf-org-babel)

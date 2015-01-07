;;; customisations for python programming
(elpy-enable)
;; (elpy-clean-modeline)

;; (remove-hook 'python-mode-hook 'esk-paredit-nonlisp)

(define-key python-mode-map (kbd "<return>") 'newline-and-indent)

(defun py-use-tab-indentation ()
  ;; both these vars are buffer local
  (setq tab-width 4
        indent-tabs-mode t))

;; (remove-hook 'python-mode-hook 'py-use-tab-indentation)

(defun py-smart-newline ()
  (interactive)
  (cond ((blank-line-p 2)
         (newline))
        ((or (blank-line-p)
             (search-on-line "pass")
             (search-on-line "return"))
         (newline-and-indent)
         (delete-char -4)) ; todo use a smarter function for this
        (t (newline-and-indent))))

;; indent function for python that knows about preceding
;; newlines, return, pass
;; figure out what to do with nested function definitions and class
;; definition: perhaps the convention is that single blank lines,
;; return and pass removes one level, while double blank lines reset
;; indentation to 0

(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(setq org-babel-python-command "python")

(defun elpy-shell-send-line ()
  "Send the current line to the Python shell. "
  (interactive)
  ;; Ensure process exists
  (elpy-shell-get-or-create-process)
  (let ((region (elpy-shell--region-without-indentation
                 (save-excursion (beginning-of-line) (point))
                 (save-excursion (end-of-line) (point)))))
    (python-shell-send-string region))
  (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                  nil
                  'visible))

(defun elpy-shell-send-definition ()
  "Send the part of the buffer from current point up to the beginning of the current toplevel form"
  (interactive)
  ;; Ensure process exists
  (elpy-shell-get-or-create-process)
  (let ((region (elpy-shell--region-without-indentation
                 (save-excursion
                   (search-backward-regexp "^\\(def\\|class\\)")
                   ;; todo add support for decorators
                   (point))
                 ;; todo figure out if we can also autofind the end of
                 ;; a definition: two empty lines, or the next time
                 ;; a non-trivial character appears at beginning of line
                 (point))))
    (python-shell-send-string region))
  (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                  nil
                  'visible))

(define-key python-mode-map (kbd "C-x C-e") 'elpy-shell-send-line)
(define-key python-mode-map (kbd "C-c C-e") nil)
(define-key elpy-mode-map (kbd "C-c C-e") nil)
(define-key python-mode-map (kbd "C-M-x") 'elpy-shell-send-definition)

(defun py-remove-debug-statements ()
  (interactive)
  (let ((re "^\s*print \"debug.*$"))
    (delete-matching-lines re (region-beginning) (region-end))))


(provide 'cnf-python)

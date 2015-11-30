;;; customisations for python programming
(use-package elpy
    :ensure t
    :init (elpy-enable)
    :config (setf python-check-command "flake8-python2"))
;; (elpy-clean-modeline)

(use-package ipython :disabled t)
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

(defun string-join (sep lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) (car lst))
        (t (string-join sep (cons (concat (car lst) sep (cadr lst))
                                  (cddr lst))))))

(defun set-interpreter-python (cmd &rest args)
  (setq python-shell-interpreter cmd
        python-shell-interpreter-args (string-join " " args)
        org-babel-python-command (string-join " " (cons cmd args))
        python-shell-buffer-name cmd))


(defun set-interpreter-python3 ()
  (interactive)
  (set-interpreter-python "python3" "-i")
  (setf python-check-command "flake8"))

(defun set-interpreter-python2 ()
  (interactive)
  (set-interpreter-python "python2" "-i")
  (setf python-check-command "flake8-python2"))

(defun set-interpreter-sage ()
  (interactive)
  (set-interpreter-python "sage" "-python -i")
  (setf python-check-command "flake8-python2"))

(set-interpreter-python3)

(defvar sage nil)
(make-variable-buffer-local 'sage)

(defun turn-on-sage ()
  (when sage
    (make-local-variable 'python-shell-interpreter)
    (make-local-variable 'python-shell-interpreter-args)
    (make-local-variable 'org-babel-python-command)
    (make-local-variable 'python-shell-buffer-name)
    (make-local-variable 'python-check-command)
    (message "Use sage as python interpreter for this buffer.")
    (set-interpreter-sage)))

(add-hook 'hack-local-variables-hook 'turn-on-sage)

;; TODO create a function for adding to path
(setenv "PYTHONPATH" "/home/olaf/Projekte/olsage:/home/olaf/Perfezionamento/thesis")
(setenv "SAGE_PATH" "/home/olaf/Projekte/olsage:/home/olaf/Perfezionamento/thesis")

(defun sage-var-index-transform ()
  (interactive)
  (save-excursion
    (mark-sexp)
    (replace-regexp "\\([qb]\\)\\([0-9]+\\)" "\\1[\\2]" nil (region-beginning) (region-end))))

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
  (let ((re "^\s*print[ (]\"debug.*$"))
    (delete-matching-lines re (region-beginning) (region-end))))

(defun sage-var-transform (assignment-string)
  (let ((variable-names (s-split " *, *" (s-trim assignment-string) t)))
    (if (<= (length variable-names) 1)
        (concat "\"" (or (car variable-names) "") "\"")
        (concat "[\"" (s-join "\", \"" variable-names) "\"]"))))


(provide 'cnf-python)

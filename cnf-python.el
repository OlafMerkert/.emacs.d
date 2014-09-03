;;; customisations for python programming
(elpy-enable)
;; (elpy-clean-modeline)

(add-hook 'python-mode 'esk-paredit-nonlisp)

(define-key python-mode-map (kbd "<return>") 'newline-and-indent)

(defun py-use-tab-indentation ()
  ;; both these vars are buffer local
  (setq tab-width 4
        indent-tabs-mode t))

(add-hook 'python-mode 'py-use-tab-indentation)

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

(provide 'cnf-python)

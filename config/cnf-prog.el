(setq c-default-style            "k&r"  ; C indentation style
      whitespace-line-column     80
      whitespace-style           '(face trailing tabs lines-tail empty indentation))

(setq-default indent-tabs-mode nil)

(use-package js2-mode
    :ensure t
    :commands js2-minor-mode
    :init (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package lua-mode :ensure t :commands lua-mode)

(after-load 'c-mode
  (define-key c-mode-map  (kbd "<return>") 'reindent-then-newline-and-indent))

;; auto fill comments
(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-add-watchwords)

(provide 'cnf-prog)

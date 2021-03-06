(global-set-key (kbd "C-x C-d") 'mark-defun)

(use-package expand-region
  :ensure t
  :bind ("C-M-o" . er/expand-region)
  :init (add-to-list 'clean-local-keybindings "C-M-o"))

(use-package mark-more-like-this
  :ensure t
  :bind ("C-M-m" . mark-more-like-this))

(use-package whole-line-or-region
  :ensure t
  :config (progn
            (add-to-list 'whole-line-or-region-extensions-alist
                         '(comment-dwim whole-line-or-region-comment-dwim-2 nil))
            (whole-line-or-region-mode 1)
            (diminish 'whole-line-or-region-mode)))

(provide 'cnf-selection)

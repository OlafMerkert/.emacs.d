(use-package expand-region
    :ensure t
    :disabled t
    :bind ("C-M-o" . er/expand-region))

(use-package mark-more-like-this
    :ensure t
    :bind ("C-M-m" . mark-more-like-this))

(use-package whole-line-or-region
    :ensure t
    :init (progn
            (add-to-list 'whole-line-or-region-extensions-alist
                         '(comment-dwim whole-line-or-region-comment-dwim-2 nil))
            (whole-line-or-region-mode 1)
            (diminish 'whole-line-or-region-mode)))

(provide 'cnf-selection)

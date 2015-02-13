(use-package magit
    :ensure t
    :bind ("<f6>" . magit-status))

(after-load 'magit
    ;; (fullframe magit-status magit-mode-quit-window)
    )

(provide 'cnf-vc)

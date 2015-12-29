(use-package auto-complete
    :ensure t
    :init (progn
            (setq tab-always-indent 'complete))
    :config (progn
              (setq-default ac-expand-on-auto-complete nil)
              (setq-default ac-auto-start nil)
              (setq-default ac-dwim nil)))

(provide 'cnf-autocomplete)

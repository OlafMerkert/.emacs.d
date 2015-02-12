(require 'smartparens-config)

;; smartparens-strict-mode
(setq sp-autoinsert-if-followed-by-same 3
      sp-autoinsert-if-followed-by-word t)

(add-hook 'python-mode-hook 'smartparens-strict-mode)
(add-hook 'js-mode-hook 'smartparens-strict-mode)

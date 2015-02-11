;; use visual line instead of auto-fill
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(provide 'cnf-text)

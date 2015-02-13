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

(provide 'cnf-prog)

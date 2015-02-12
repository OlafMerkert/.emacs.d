;; enable some commands
(put 'downcase-region   'disabled nil)
(put 'upcase-region     'disabled nil)
(put 'capitalize-region 'disabled nil)
(put 'narrow-to-region  'disabled nil)

(use-package dash :ensure t)
(use-package fullframe :ensure t)

;; TODO replace starter-kit with own customisations
(use-package starter-kit :ensure t)
(use-package starter-kit-lisp :ensure t)
(use-package starter-kit-eshell :ensure t)
(use-package starter-kit-js :ensure t)
(use-package starter-kit-bindings :ensure t)
;; disable hl-line-mode
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(column-number-mode t) ; show line and column in modeline
(setq initial-scratch-message    nil
      woman-use-own-frame        nil
      make-backup-files          nil
      ps-print-color-p           'black-white
      uniquify-buffer-name-style 'post-forward)

(use-package ido :ensure t)

(use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode 1))

(use-package helm :ensure t)

(use-package hydra :ensure t)

(use-package ibuffer-vc
    :commands ibuffer-vc-set-filter-groups-by-vc-root
    :ensure t
    :init (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; some keybindings
(global-set-key (kbd "S-<return>") 'split-line)
(global-set-key (kbd "C-x M-o")    'other-frame)
(global-set-key (kbd "C-x c")      'comment-dwim)

(provide 'cnf-base)

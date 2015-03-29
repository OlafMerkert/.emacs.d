;; enable some commands
(put 'downcase-region   'disabled nil)
(put 'upcase-region     'disabled nil)
(put 'capitalize-region 'disabled nil)
(put 'narrow-to-region  'disabled nil)

;; repeat last command
(global-set-key (kbd "C-z") 'repeat)

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
    :ensure t
    :init (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)
    :config (progn
              (defun ibuffer-vc--deduce-backend (file)
                "Return the vc backend for FILE, or nil if not under VC supervision."
                (ignore-errors (vc-backend file)))
              (defun ibuffer-vc-set-filter-groups-by-vc-root ()
                "Set the current filter groups to filter by vc root dir."
                (interactive)
                (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
                (message "ibuffer-vc: groups set")
                (let ((ibuf (get-buffer "*Buffer List*")))
                  (when ibuf
                    (with-current-buffer ibuf
                      (pop-to-buffer ibuf)
                      (ibuffer-update nil t)))))))

(setq ibuffer-use-other-window t
      ibuffer-default-shrink-to-minimum-size nil
      ibuffer-default-directory "~")
(global-set-key (kbd "C-x C-b") (lambda () (interactive) (ibuffer t "*Buffer List*")))

;; if we call `ibuffer' from itself, then `ibuffer-quit' does not work
;; anymore. So just update instead
(after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-x C-b") 'ibuffer-vc-set-filter-groups-by-vc-root)
  (define-key ibuffer-mode-map (kbd "g") 'ibuffer-vc-set-filter-groups-by-vc-root))

;;; some keybindings
(global-set-key (kbd "S-<return>") 'split-line)
(global-set-key (kbd "C-x M-o")    'other-frame)
(global-set-key (kbd "C-x c")      'comment-dwim)

(provide 'cnf-base)

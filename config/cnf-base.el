;; TODO replace starter-kit with own customisations
(use-package 'starter-kit :ensure t)
(use-package 'starter-kit-lisp :ensure t)
(use-package 'starter-kit-eshell :ensure t)
(use-package 'starter-kit-js :ensure t)
(use-package 'starter-kit-bindings :ensure t)

(use-package 'ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode 1))

(use-package 'expand-region
    :ensure t
    :disabled t
    :bind ("C-M-o" . er/expand-region))

(use-package 'ace-jump-mode
    :ensure t
    :bind ("C-j" . ace-jump-mode))

(use-package 'iy-go-to-char
    :ensure t
    :bind (("M-j"  . iy-go-to-char)
           ("M-J"  . iy-go-to-char-backward)))

(use-package 'mark-more-like-this
    :ensure t
    :bind ("C-M-m" . mark-more-like-this))

(use-package 'multiple-cursors
    :ensure t
    :bind (("C-&" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this)
           ("C-*" . mc/mark-all-like-this-dwim)))

(use-package 'iedit
    :commands iedit-mode
    :ensure t
    :bind (("C-;" . iedit-dwim)))

(defun iedit-dwim (arg)
  "Starts iedit but uses `narrow-to-defun' to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (narrow-to-defun)
          (if iedit-mode
              (iedit-done)
              ;; `current-word' can of course be replaced by other
              ;; functions.
              (iedit-start (current-word)))))))

(provide 'cnf-base)

;; Turn off mouse graphical interfaceinterface
(dolist (mode '(menu-bar-mode tool-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Turn off graphical distraction
(when window-system
  (dolist (mode '(tooltip-mode mouse-wheel-mode blink-cursor-mode))
    (when (fboundp mode) (funcall mode -1)))
  ;; but we like scroll bars
  (scroll-bar-mode +1)
  (setq frame-title-format "%b  [emacs]"
        ;;'(buffer-file-name "GNU Emacs: %f" "GNU Emacs: %b")
        )
  )

;; smarter M-x
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t ; yank at point even with mouse
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; never have to type full yes
(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

(after-load 'grep
  (when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "*.class")))

(provide 'cnf-base-settings)


(when (>= emacs-major-version 24)
  (require 'package)
  ;; additional repos for elpa
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (package-initialize)
  
  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages
    '(starter-kit
      starter-kit-eshell
      starter-kit-js
      starter-kit-lisp
      starter-kit-bindings
      ;; solarized-theme
      ;; zenburn-theme
      ;; anti-zenburn-theme
      expand-region
      ace-jump-mode
      js2-mode
      auctex
      yasnippet
      lua-mode
      ecb
      org
      w3m
      ipython
      iy-go-to-char
      iedit
      multiple-cursors
      mark-more-like-this
      mark-multiple
      magit
      elpy
      )
    "my default selection of packages, to be automatically
  installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(setq desktop-path '("~/.emacs.d/sessions/"))
(desktop-save-mode 1)

(require 'starter-kit)
(require 'starter-kit-lisp)

(add-hook 'js-mode-hook 'js2-minor-mode)

;; disable some unwanted stuff from the starter kit

;; general settings
(column-number-mode t)
(setq initial-scratch-message    nil
      woman-use-own-frame        nil
      make-backup-files          nil
      ps-print-color-p           'black-white
      uniquify-buffer-name-style 'post-forward
      c-default-style            "k&r" ; C indentation style
      whitespace-line-column     80
      whitespace-style           '(face trailing tabs lines-tail empty indentation))
(setq-default indent-tabs-mode nil)

;; spellchecking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; don't flyspell automatically
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; use english in commit messages
(defun use-english-dictionary ()
  (interactive)
  (ispell-change-dictionary "en_GB"))

(add-hook 'magit-log-edit-mode-hook 'use-english-dictionary)

(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode 1)))

;; browser
(setq browse-url-generic-program   "xdg-open"
      browse-url-browser-function  'w3m-browse-url
      w3m-use-cookies              t
      w3m-pop-up-windows           t
      w3m-use-tab                  nil)

;; projects
(setq ecb-source-path
      '(("~/bin" "config")
        ("~/texmf/tex/latex/olaf" "texmf")
        ("~/Projekte/ol-utils" "ol-utils")
        ("~/Projekte/math-utils" "math-utils")
        ("~/Projekte/continued-fractions" "continued-fractions")
        ("~/Projekte/web-document-gallery" "web-document")
        ("~/Studium/MasterArbeit" "master")))

;; snippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; use nxml for html editing
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . nxml-mode))

;; prettify xml code
(defun prettify-xml ()
  (interactive)
  (replace-string "><" ">
<")
  (indent-region (buffer-end -1) (buffer-end 1)))


(dolist (cnf '("functions"
               "lisp"
               "tex"
               "bindings"
               "personal"
               "email"))
  (load (concatenate 'string "~/.emacs.d/cnf-" cnf)))

(windmove-default-keybindings)

;;; if we want to show the same buffer left and right, call these
(defun same-buffers-from-active ()
  (interactive)
  (set-window-buffer (get-lru-window)
                     (window-buffer (get-mru-window))))

(defun same-buffers-from-inactive ()
  (interactive)
  (set-window-buffer (get-mru-window)
                     (window-buffer (get-lru-window))))

(defun same-buffers (&optional arg)
  (interactive "P")
  (if arg
      (same-buffers-from-inactive)
      (same-buffers-from-active)))

;;; some stuff for python programming
(elpy-enable)
(elpy-clean-modeline)

(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((Package . emacs) (Package . HUNCHENTOOT) (Syntax . COMMON-LISP) (Base . 10) (Syntax . Common-Lisp) (Package . Maxima) (ispell-dictionary . "en_GB") (ispell-dictionary . "english") (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

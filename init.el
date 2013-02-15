
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
      solarized-theme
      zenburn-theme
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

;; disable some unwanted stuff from the starter kit
(remove-hook 'prog-mode-hook 'idle-highlight-mode)

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

(server-start)

;; (load-theme 'zenburn t)
(load-theme 'anti-zenburn t)
;; (load-theme 'solarized-light t)
;; (load-theme 'tango-dark)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6b00751018da9a360ac8a7f7af8eb134921a489725735eba663700cebc12fa6f" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" "27470eddcaeb3507eca2760710cc7c43f1b53854372592a3afa008268bcf7a75" "cfde97b1d5ed1770b8e2e1b739611820c3a3e370cbda75d96e78ef2a5f359b27" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "64b7be5703b90e05f7bc1f63a9f689a7c931626462697bea9476b397da194bd9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(safe-local-variable-values (quote ((Package . TOWN-EXAMPLE) (ispell-dictionary . "en_GB") (ispell-dictionary . "english") (TeX-master . ${5:t}) (ispell-dictionary . en) (Package . Maxima) (Package . CL-WHO) (Syntax . COMMON-LISP) (Encoding . utf-8) (readtable . runes) (Package . CXML) (Package . ESA) (Lowercase . Yes) (Package . CLIM-DEMO) (ispell-dictionary . da) (ispell-dictionary . dansk) (ispell-dictionary . en_GB) (Base . 10) (Syntax . ANSI-Common-Lisp) (Package . CLIM-INTERNALS) (Package . DREI-CORE) (Syntax . Common-Lisp) (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

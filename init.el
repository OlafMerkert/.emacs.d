
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
      mark-multiple
      )
    "my default selection of packages, to be automatically
  installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

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

(add-hook 'text-mode-hook
          (lambda ()
            ;; (flyspell-mode 1)
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
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; use nxml for html editing
(add-to-list 'auto-save-list '("\\.html$" . nxml-mode))
(add-to-list 'auto-save-list '("\\.htm$" . nxml-mode))

;; (load-theme 'solarized-light t)
(load-theme 'zenburn t)

(require 'starter-kit)
(require 'starter-kit-lisp)

(dolist (cnf '("functions"
               "lisp"
               "tex"
               "bindings"
               "personal"
               "email"))
  (load (concatenate 'string "~/.emacs.d/cnf-" cnf)))

(server-start)


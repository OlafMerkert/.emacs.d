;; additional repos for elpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages
    '(starter-kit
      starter-kit-eshell
      starter-kit-js
      starter-kit-lisp
      starter-kit-bindings
      solarized-theme
      slime
      slime-repl
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
      )
    "my default selection of packages, to be automatically
  installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))  

;; (package-initialize)

;; global key settings
(global-set-key (kbd "<f6>") 'magit-status)

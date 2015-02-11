(require 'package)

;; configure repos for elpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


(defvar my-packages
  '(
    
    ;; color-theme-solarized
    ;; zenburn-theme
    ;; anti-zenburn-theme


    js2-mode
    auctex
    yasnippet
    lua-mode
    ecb
    org
    w3m
    ipython
    mark-multiple
    magit
    elpy
    bbdb
    dash
    smartparens
    htmlize
    ibuffer-vc
    helm
    )
  "my default selection of packages, to be automatically
  installed at launch.")

(provide 'cnf-package)

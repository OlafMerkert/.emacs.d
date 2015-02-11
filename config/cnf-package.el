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
    auctex
    ecb
    org
    ipython
    mark-multiple
    elpy
    bbdb
    dash
    helm
    )
  "my default selection of packages, to be automatically
  installed at launch.")

(provide 'cnf-package)

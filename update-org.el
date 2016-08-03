(when (version<= emacs-version "24")
  (error "This emacs is too old for this config."))

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(require 'cnf-package)

(package-list-packages)

(provide 'update-org)

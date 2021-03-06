;; package.el expects this; but we call it in cnf-package.el
;; (package-initialize)

(when (version<= emacs-version "24")
  (error "This emacs is too old for this config."))

(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'cl)
(require 'cnf-utils)
;; (require 'cnf-functions)
(require 'cnf-package)
(require 'cnf-clear-bindings)
(require 'cnf-base-settings)

(require 'cnf-environment)
;; only set personal stuff if the current user name matches
(when (equal "olaf" (user-login-name))
  (require 'cnf-personal))
(require 'cnf-base)

(require 'cnf-navigation)
(require 'cnf-selection)
(require 'cnf-editing)

;; helpers
(require 'cnf-spelling)
(require 'cnf-snippets)
(require 'cnf-vc)

;; writing and programming
(require 'cnf-prog)
(require 'cnf-autocomplete)
(require 'cnf-smartparens)
(require 'cnf-lisp)
(require 'cnf-python)
(require 'cnf-tex)
(require 'cnf-text)
(require 'cnf-xhtml)

;; smaller apps
(require 'cnf-browse)
(require 'cnf-eshell)

;; org and email
(require 'cnf-email)
(require 'cnf-twitter)
(require 'cnf-org)
(require 'cnf-org-export)
(require 'cnf-org-babel)
(require 'cnf-org-protocol)
(require 'cnf-mobile)

;; final tweaks
(require 'cnf-bindings)
(require 'cnf-colours)
(require 'cnf-session)


;; move customisation into separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

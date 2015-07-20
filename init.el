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
(require 'cnf-base-settings)

(require 'cnf-environment)
(require 'cnf-personal)
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
(require 'cnf-lisp)
(require 'cnf-python)
(require 'cnf-tex)
(require 'cnf-text)
(require 'cnf-xhtml)

;; smaller apps
(require 'cnf-browse)

;; org and email
(require 'cnf-email)
(require 'cnf-org)
(require 'cnf-org-export)
(require 'cnf-org-babel)
(require 'cnf-org-protocol)

;; final tweaks
(require 'cnf-colours)
(require 'cnf-session)


;; move customisation into separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

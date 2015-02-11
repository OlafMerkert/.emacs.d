(when (version<= emacs-major-version "24")
  (error "This emacs is too old for this config."))

;;(server-start)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'cnf-utils)
(require 'cnf-package)

(require 'cnf-environment)
(require 'cnf-base)
(require 'cnf-navigation)
(require 'cnf-editing)
(require 'cnf-spelling)

(require 'cnf-snippets)

(require 'cnf-prog)
(require 'cnf-lisp)
(dolist (cnf '("functions"
               "lisp"
               "python"
               "tex"
               "bindings"
               "personal"
               "email"
               "org"
               "colours"
               ;; "smartparens"
               ))
  (load (concatenate 'string "~/.emacs.d/cnf-" cnf)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Perfezionamento/thesis/continued-fractions-computations.sage.org" "~/Personal/bookmarks.org" "~/Personal/spiele.org" "~/Personal/sns.org" "~/Personal/notizen.org" "~/Personal/privat.org" "~/Perfezionamento/thesis/phd-thesis.en.org")))
 '(safe-local-variable-values
   (quote
    ((ispell-dictionary . "de_DE")
     (ispell-dictionary . "it")
     (Syntax . ANSI-Common-Lisp)
     (Syntax . COMMON-LISP)
     (Base . 10)
     (Syntax . Common-Lisp)
     (ispell-dictionary . "de")
     (ispell-dictionary . "en_GB")
     (ispell-dictionary . "english")
     (whitespace-line-column . 80)
     (lexical-binding . t)
     (org-babel-python-command . "sage -python")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



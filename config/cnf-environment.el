;; eshell
(setenv "EDITOR" "emacsclient")
(add-to-list 'exec-path (expand-file-name "~/bin"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setq-default eshell-path-env (getenv "PATH"))
(setq cd-path (list "./"
                    "../"
                    (expand-file-name "~/Projekte")
                    (expand-file-name "~/Perfezionamento/projects")))

;;; fix for using tramp::sudo
(after-load 'tramp
    (setenv "SHELL" "/bin/bash"))

;; projects
(use-package ecb :disabled t)

(setq ecb-source-path
      '(("~/bin" "config")
        ("~/texmf/tex/latex/olaf" "texmf")
        ("~/Projekte/ol-utils" "ol-utils")
        ("~/Projekte/math-utils" "math-utils")
        ("~/Projekte/continued-fractions" "continued-fractions")
        ("~/Projekte/web-document-gallery" "web-document")
        ("~/Studium/MasterArbeit" "master")))

(provide 'cnf-environment)

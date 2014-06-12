(provide 'cnf-org)

(require 'org-capture)
(require 'magit)

(defun org-path (filebase)
   (concat org-directory "/" filebase ".org"))

(eval-after-load 'org
  '(progn
    (define-key org-mode-map (kbd "<f2>") 'insert-greek-letter)))

(setq org-deadline-warning-days 5
      org-completion-use-ido t)

(setq org-directory "~/Personal"
      org-default-notes-file (org-path "notizen"))


(defun autocommit-current-org-file ())

;; syncing should have the following behaviour
;; save all files
;; look at ~/Personal
;; * if there are uncommitted changes, commit them (do not commit new
;; files)
;; * pull other changes
;; * push new changes

;; magit requires a trailing slash for the directory path!
(defpar personal-information-dir "~/Personal/")

(defun sync-personal-information (&optional pull-only)
  ;; if prefix argument is given, only pull info
  (interactive "P")
  (let ((default-directory personal-information-dir))
    (unless pull-only
      (save-some-buffers)
      (when (magit-anything-unstaged-p)
        (magit-call-git "add" "."))
      (when (magit-anything-staged-p)
        (magit-call-git "commit" "-m"
                        (concat "synced personal data from "
                                (system-name)
                                " at "
                                (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))))
        (message "Commited changes to personal data.")))
    (magit-pull)
    (unless pull-only (magit-push))))

;;; configure org capture
(setq org-capture-my-todo-template "* TODO %?%i\n  %U\n  %a"
      org-capture-templates
      `(("t" "Todo" entry
             (file+headline "" "Tasks")
             ,org-capture-my-todo-template)
        ("s" "SNS Todo" entry
             (file+headline ,(org-path "sns") "Tasks")
             ,org-capture-my-todo-template)
        ("v" "Vortrag" entry
             (file+headline ,(org-path "sns") "Vorträge")
             "* %?%i\n  %a")
        ("p" "Privat Todo" entry
             (file+headline ,(org-path "privat") "Tasks")
             ,org-capture-my-todo-template)
        ;; TODO capture to readme.org of current (git) project
        ("r" "Readme" entry
             (function find-git-project-readme-tasks)
             ,org-capture-my-todo-template)
        ))

(defun find-git-project-readme-tasks ()
  (find-git-project-readme "Tasks"))

(defun find-git-project-readme (&optional hd)
  (interactive)
  (let ((gitdir (magit-get-top-dir)))
    (if gitdir
        (progn
          (find-file (concat gitdir "readme.org"))
          (message "%s" org-complex-heading-regexp-format)
          (if hd
              (if (re-search-forward
                   (format org-complex-heading-regexp-format (regexp-quote hd))
                   nil t)
                  (goto-char (point-at-bol))
                  (goto-char (point-max))
                  (or (bolp) (insert "\n"))
                  (insert "* " hd "\n")
                  (beginning-of-line 0)
                  (message "%s" (point))
                  )))
        (error "Not in a git project."))))

(defadvice org-capture-set-target-location (after org-capture-function-remove-exact-exact-location)
  (org-capture-put :exact-position nil))

(ad-activate 'org-capture-set-target-location)

;; instruct org to open certain files always with external
;; applications
(add-to-list 'org-file-apps '("nb" . "mathematica -sl %s") t)
(add-to-list 'org-file-apps '("pdf" . "evince %s"))

;;; setup global exporting options


(eval-after-load 'ox-latex
  '(progn
    ;; make sure shell-escape is turned on (I need it for \gitversioninfo)
    (setq org-latex-pdf-process
     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    ;; enable koma script
    (add-to-list 'org-latex-classes
     '("scrartcl"
       "\\documentclass[a4paper,11pt]{scrartcl}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
     '("scrreprt"
       "\\documentclass[a4paper,11pt]{scrreprt}"
       ("\\chapter{%s}" . "\\chapter*{%s}")
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

;;; configure babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (lisp . t)))

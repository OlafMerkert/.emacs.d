(use-package org
    :ensure t
    :pin gnu)

(require 'org-capture)
(require 'org-protocol)
(require 'magit)

(defun org-path (filebase)
   (concat org-directory "/" filebase ".org"))

(after-load 'org
  (define-key org-mode-map (kbd "<f2>") 'insert-greek-letter)
  (define-key org-mode-map (kbd "C-c C-v k") 'org-babel-remove-result)
  (define-key org-mode-map (kbd "C-;") 'tex-goto-prev-dollar)
  (define-key org-mode-map (kbd "C-'") 'tex-goto-next-dollar)
  (define-key org-mode-map (kbd "M-;") 'tex-goto-prev-backslash)
  (define-key org-mode-map (kbd "M-'") 'tex-goto-next-backslash))

(setq org-deadline-warning-days 5
      org-completion-use-ido t)

(setq org-src-fontify-natively t)

(setq org-directory "~/Personal"
      org-default-notes-file (org-path "notizen"))


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
(let ((my-todo-template "* TODO %?%i\n  %a")
      (my-simple-todo-template "* TODO %?"))
  (setq  org-capture-templates
         `(("t" "Todo" entry
                (file+headline "" "Tasks")
                ,my-todo-template)
           ("T" "Todo (noref)" entry
                (file+headline "" "Tasks")
                ,my-simple-todo-template)
           ("s" "SNS Todo" entry
                (file+headline ,(org-path "sns") "Tasks")
                ,my-todo-template)
           ("v" "Vortrag" entry
                (file+headline ,(org-path "sns") "Vorträge")
                "* %?%i\n  %a")
           ("p" "Privat Todo" entry
                (file+headline ,(org-path "privat") "Tasks")
                ,my-todo-template)
           ;; TODO capture to readme.org of current (git) project
           ("r" "Readme" entry
                (function find-git-project-readme-tasks)
                ,my-todo-template)
           ("R" "Readme (noref)" entry
                (function find-git-project-readme-tasks)
                ,my-simple-todo-template)
           ("b" "Bookmark" entry
                (file ,(org-path "bookmarks"))
                "* %a%?")
           ("w" "Movie or TV Series" entry
                (file+headline ,(org-path "privat") "Filme")
                "* %a%?")
           ("m" "Music" entry
                (file+headline ,(org-path "privat") "Musik")
                "* %a%?")
           ("z" "Zimmer" entry
                (file+headline ,(org-path "privat") "Neue Wohnung")
                "* %?%i\n  %a")
           )))

(defun find-git-project-readme-tasks ()
  (find-git-project-readme "Tasks"))

(defun find-git-project-readme (&optional hd)
  (interactive)
  (let ((gitdir (magit-get-top-dir)))
    (if gitdir
        (progn
          (find-file (concat gitdir "readme.org"))
          (goto-char (point-min))
          ;; (message "%s" org-complex-heading-regexp-format)
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

;; this is required for the above location setter to work properly
(defun org-capture-remove-exact-location-fix (&optional target)
  (org-capture-put :exact-position nil))
(advice-add 'org-capture-set-target-location :after 'org-capture-remove-exact-location-fix)

;;; setup `org-refile'
(setq org-refile-targets '((nil . (:maxlevel . 2))))

;; instruct org to open certain files always with external
;; applications
(add-to-list 'org-file-apps '("nb" . "mathematica -sl %s") t)
(add-to-list 'org-file-apps '("pdf" . "evince %s"))

;; open html exports in browser instead of Emacs (we want to look at
;; them, not edit them.
(add-to-list 'org-file-apps '("html" . (browse-url-of-file file)))

(defun name-to-bbdb-link (&optional arg)
  (interactive "P")
  (let ((words (or arg 1)))
    (backward-word words)
    (insert "[[bbdb:")
    (let ((begin (point)))
      (forward-word words)
      (let ((name (buffer-substring begin (point))))
        (insert "][")
        (insert name)
        (insert "]]")))))

(defun wrap-tex-around-region ()
  (interactive)
  (save-excursion
    (let ((begin (region-beginning))
          (end (region-end)))
      (goto-char end)
      (insert " }")
      (goto-char begin)
      (insert "\\text{ "))))

(global-set-key
 (kbd "<f11>")
 (defhydra org-actions (:color blue)
   "org"
   ("SPC" org-mark-ring-goto "pop")
   ("a" org-agenda-list "agenda")
   ("t" org-todo-list "todos")
   ("c" org-capture "capture")
   ("l" org-store-link "link")
   ("u" browse-url-at-point "browse")
   ("[" org-toggle-timestamp-type "timestamp")
   ("n" name-to-bbdb-link "name-link")
   ("b" bbdb "bbdb")
   ("s" sync-personal-information "sync")
   ("A" org-agenda "agenda menu")))

;; configure latex preview
(setq org-latex-create-formula-image-program 'dvipng)

(after-load 'org
  (fullframe org-agenda org-agenda-quit)
  (fullframe org-agenda-list org-agenda-quit)
  (fullframe org-todo-list org-agenda-quit))

(provide 'cnf-org)

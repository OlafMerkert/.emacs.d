(provide 'cnf-org)

(eval-after-load 'org
  '(progn
    (define-key org-mode-map (kbd "<f2>") 'insert-greek-letter)))

(setq org-deadline-warning-days 5)
(setq org-directory "~/Personal"
      org-default-notes-file (concat org-directory "/notizen.org"))

(require 'magit)

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
                        (concat "synced personal data at "
                                (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))))
        (message "Commited changes to personal data.")))
    (magit-pull)
    (unless pull-only (magit-push))))

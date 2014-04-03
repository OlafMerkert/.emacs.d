(provide 'cnf-org)

(eval-after-load 'org
  '(progn
    (define-key org-mode-map (kbd "<f2>") 'insert-greek-letter)))

(setq org-deadline-warning-days 5)
(setq org-directory "~/Personal"
      org-default-notes-file (concat org-directory "/notizen.org"))

(defun autocommit-current-org-file ()
  ;; todo
  )

(defun sync-personal-information ()
  ;; todo
  )


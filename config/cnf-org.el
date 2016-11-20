(use-package org
    :ensure t
    :pin gnu
    :demand t
    :bind (:map org-mode-map
                ("<f2>" . insert-greek-letter)
                ("C-c C-v k" . org-babel-remove-result)
                ("C-;" . tex-goto-prev-dollar)
                ("C-'" . tex-goto-next-dollar)
                ("M-;" . tex-goto-prev-backslash)
                ("M-'" . tex-goto-next-backslash)
                ("C-_" . org-table-insert-hline)))

(require 'org-capture)
(require 'org-protocol)
(require 'magit)

(defun org-path (filebase)
   (concat org-directory "/" filebase ".org"))

(setq org-deadline-warning-days 5
      org-completion-use-ido t)

(setq org-src-fontify-natively t)

;; highlighting of latex math in org buffers
(require 'font-latex)

(org-set-emph-re 'org-emphasis-alist '(("*" bold)
                                       ("/" italic)
                                       ("_" underline)
                                       ("=" org-verbatim verbatim)
                                       ("~" org-code verbatim)
                                       ;;("+" (:strike-through t))
                                       ("$" font-latex-math-face verbatim)))

(defvar org-latex-math-envs)
(setf org-latex-math-envs '("equation" "align" "multline"))

;; (font-lock-add-keywords 'org-mode
;;                         (mapcar
;;                          (lambda (env)
;;                            `(,(format "\\(\\\\begin{%s\\*?}\\)\n*\\([^$]*\\)\n*\\(\\\\end{%s\\*?}\\)" env env)
;;                               (1 org-block-begin-line)
;;                               (2 font-latex-math-face)
;;                               (3 org-block-end-line)))
;;                          org-latex-math-envs)
;;                         t)


;; don't introduce indentation everywhere
(setq org-adapt-indentation nil
      org-startup-indented t)

(after-load 'org-indent (diminish 'org-indent-mode))

;; customise bullets and ellipsis
(setq org-ellipsis " ⚬⚬⚬")

(use-package org-bullets
    :ensure t
    :commands org-bullets-mode
    :init
    (defun turn-on-org-bullets ()
      (org-bullets-mode +1))
    (add-hook 'org-mode-hook 'turn-on-org-bullets)
    :config (setq org-bullets-bullet-list '("◉")))

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
(defpar personal-information-remote "sl2z/master")

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
    (magit-pull personal-information-remote nil)
    (unless pull-only (magit-push "master" personal-information-remote nil))))

;;; configure org capture
(let ((my-todo-template "* TODO %?%i\n%a")
      (my-simple-todo-template "* TODO %?")
      (my-note-template "* %?%i\n%a"))
  (setq  org-capture-templates
         `(("t" "Todo" entry
                (file+headline "" "Tasks")
                ,my-todo-template)
           ("T" "Todo (noref)" entry
                (file+headline "" "Tasks")
                ,my-simple-todo-template)
           ("n" "Note" entry
                (file+headline "" "Notes")
                ,my-note-template)
           ("s" "SNS Todo" entry
                (file+headline ,(org-path "sns") "Tasks")
                ,my-todo-template)
           ("v" "Vortrag" entry
                (file+headline ,(org-path "sns") "Vorträge")
                "* %?%i\n%a")
           ("p" "Privat Todo" entry
                (file+headline ,(org-path "privat") "Tasks")
                ,my-todo-template)
           ;; capture to readme.org of current (git) project
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
           ("d" "Thesis task" entry
                (file+headline "/home/olaf/Perfezionamento/thesis/phd-thesis.en.org" "Tasks")
                ,my-simple-todo-template)
           ("j" "Tagebuch" entry
                (file+datetree ,(org-path "tagebuch"))
                "* %?")
           ("f" "Firma/Stelle" entry
                (file+headline ,(org-path "jobsuche") "Arbeitgeber")
                "* %a%?\n%U")
           )))

;; make org-capture much faster by switching off clipboard lookup (who needs that?)
;; thanks to http://blog.andy.glew.ca/2013/09/org-capture-org-get-x-clipboard-very.html
(advice-add 'org-get-x-clipboard :around (lambda (fn value) nil))

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

;; capture relative filenames
(defun convert-relative-path (path)
  (save-match-data
    (if (string-match (concat "^" (regexp-quote
                                   (expand-file-name
                                    (file-name-as-directory
                                     default-directory))))
                      (expand-file-name path))
        ;; We are linking a file with relative path name.
        (substring (expand-file-name path)
                   (match-end 0))
        (abbreviate-file-name (expand-file-name path)))))

(defun org-link-description (link &optional offset)
  (unless offset (setf offset 0))
  (let ((c-begin (org-element-property :contents-begin link))
        (c-end (org-element-property :contents-end link)))
    (when (and c-begin c-end)
      (buffer-substring (+ offset c-begin) (+ offset c-end)))))

(defun org-convert-file-links-relative (&optional entire-buffer)
  (interactive "P")
  (let ((offset 0))
    (save-excursion
      (org-element-map
          (org-element-parse-buffer 'object (not entire-buffer)) 'link
        (lambda (link)
          (when (string= (org-element-property :type link) "file")
            (let* ((description (org-link-description link offset))
                   (raw-path (substring (org-element-property :raw-link link) 5))
                   (new-path (convert-relative-path raw-path))
                   (begin (+ offset (org-element-property :begin link)))
                   (end (+ offset (org-element-property :end link))))
              (unless (string= raw-path new-path)
                (goto-char begin)
                (if (looking-at "\\[")
                    (progn
                      (delete-char (- end begin))
                      (insert "[[file:" new-path)
                      (when description
                        (insert "][" description))
                      (insert "]]"))
                    (progn
                      (delete-char (- end begin))
                      (insert "file:" new-path)))
                (incf offset (- (point) end))))))))))

(defun org-capture-to-relative-file-links (&optional goto keys)
  (org-convert-file-links-relative nil))

(advice-add 'org-capture :after 'org-capture-to-relative-file-links )

;;; setup `org-refile'
(setq org-refile-targets '((nil . (:maxlevel . 3))))

;; instruct org to open certain files always with external
;; applications
(setq my-video-viewer "vlc %s")

(add-to-list 'org-file-apps '("nb" . (lambda (file link) "mathematica -sl %s")) t)
(add-to-list 'org-file-apps '("pdf" . (lambda (file link) " evince %s")))
(add-to-list 'org-file-apps `("mp4" . ,(lambda (file link) my-video-viewer)))
(add-to-list 'org-file-apps `("avi" . ,(lambda (file link) my-video-viewer)))

;; open html exports in browser instead of Emacs (we want to look at
;; them, not edit them.
(add-to-list 'org-file-apps '("html" . (lambda (file link) (browse-url-of-file file))))

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

(defun my-org-export ()
  (interactive)
  (org-export-dispatch 4))

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
   ("A" org-agenda "agenda menu")
   ("e" my-org-export "export")))

;; configure latex preview
(setq org-latex-create-formula-image-program 'dvipng)

(after-load 'org
  (fullframe org-agenda org-agenda-quit)
  (fullframe org-agenda-list org-agenda-quit)
  (fullframe org-todo-list org-agenda-quit))

(provide 'cnf-org)

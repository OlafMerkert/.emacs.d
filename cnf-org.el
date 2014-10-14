(require 'org-capture)
(require 'org-protocol)
(require 'magit)

(defun org-path (filebase)
   (concat org-directory "/" filebase ".org"))

(eval-after-load 'org
  '(progn
    (define-key org-mode-map (kbd "<f2>") 'insert-greek-letter)
    (define-key org-mode-map (kbd "<f11> n") 'name-to-bbdb-link)
    ))

(setq org-deadline-warning-days 5
      org-completion-use-ido t)

(setq org-src-fontify-natively t)

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

;; what does this do?
(defadvice org-capture-set-target-location (after org-capture-function-remove-exact-exact-location)
  (org-capture-put :exact-position nil))

(ad-activate 'org-capture-set-target-location)

;;; setup `org-refile'
(setq org-refile-targets '((nil . (:maxlevel . 2))))

;; instruct org to open certain files always with external
;; applications
(add-to-list 'org-file-apps '("nb" . "mathematica -sl %s") t)
(add-to-list 'org-file-apps '("pdf" . "evince %s"))

;;; setup global exporting options
;; allow use of #+BIND: to configure variables during export
(setq org-export-allow-bind-keywords t)

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
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)))

;; turn off indentation-highlight during htmlize
(defvar htmlize-reenable-modes nil)
(defvar htmlize-disable-modes nil)

(add-to-list 'htmlize-disable-modes 'highlight-indentation-mode)

(defun htmlize-turn-off-modes ()
  (make-local-variable 'htmlize-reenable-modes)
  (dolist (mode htmlize-disable-modes)
    (when (symbol-value mode)
      (push mode htmlize-reenable-modes)
      (funcall mode -1))))

(defun htmlize-turn-on-modes ()
  (dolist (mode htmlize-reenable-modes)
    (funcall mode +1))
  (setf htmlize-reenable-modes nil))

(eval-after-load 'htmlize
  '(progn
    (add-hook 'htmlize-before-hook 'htmlize-turn-off-modes)
    (add-hook 'htmlize-after-hook 'htmlize-turn-on-modes)))

;;; configure babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)))

(defun babel-language-p (language)
  (find language org-babel-load-languages :test 'string-equal
        :key (lambda (x) (symbol-name (car x)))))

(setq org-confirm-babel-evaluate
      (lambda (language body) (not (babel-language-p language)))
      ;; do not add leading whitespace for source-blocks after editing
      ;; (why would anybody want that?)
      org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-export-babel-evaluate nil)

(defun strip-blank-lines (str)
  "Remove all blank lines from the given string `str'."
  ; the space or tab at the beginning of is necessary, because we
  ; don't want (and need to) strip blank lines between top-level forms
  (replace-regexp-in-string "[\n]\+\\([ \t]\\)" "\n\\1" str))

(defadvice org-babel-python-evaluate-session (before ob-py-strip-blank-lines)
  (ad-set-arg 1 (strip-blank-lines (ad-get-arg 1))))

(ad-activate 'org-babel-python-evaluate-session)
;; note that this also removes blank lines in strings, where they could
;; be wanted. But for now, it is a decent workaround.

(defun beginning-of-word ()
  ;; todo not working yet
  (save-excursion
    (cond ((looking-at "[:space:]") nil)
          ((progn (backward-char)
                  (looking-at "[:space:]")) t)
          (t nil))))

(defun name-to-bbdb-link ()
  (interactive)
  (backward-word)
  (insert "[[bbdb:")
  (let ((begin (point)))
    (forward-word)
    (let ((name (buffer-substring begin (point))))
      (insert "][")
      (insert name)
      (insert "]]"))))

(defun wrap-tex-around-region ()
  (interactive)
  (save-excursion
    (let ((begin (region-beginning))
          (end (region-end)))
      (goto-char end)
      (insert " }")
      (goto-char begin)
      (insert "\\text{ "))))

;;; custom `org-protocol' handlers
(add-to-list 'org-protocol-protocol-alist
             '("Open in w3m"
               :protocol "w3m"
               :function org-protocol-open-in-w3m))

(add-to-list 'org-protocol-protocol-alist
             '("Download with youtube-dl"
               :protocol "ytdl"
               :function org-protocol-download-with-youtube-dl))

(defun org-protocol-open-in-w3m (fname)
  (let* ((splitparts (org-protocol-split-data fname t org-protocol-data-separator))
         (uri (org-protocol-sanitize-uri (car splitparts))))
    (w3m-browse-url uri)))

(defun download-with-youtube-dl (uri &rest args)
  (let ((default-directory "~/Downloads/"))
    (async-shell-command
     (concat "youtube-dl '"
             uri
             "'")
     (generate-new-buffer "*youtube-dl*"))))

(defun org-protocol-download-with-youtube-dl (fname)
  (let* ((splitparts (org-protocol-split-data fname t org-protocol-data-separator))
         (uri (org-protocol-sanitize-uri (car splitparts))))
    (download-with-youtube-dl uri)))

(defun org-link-download-with-youtube-dl ()
  (interactive)
  (let ((browse-url-browser-function 'download-with-youtube-dl))
    (org-open-at-point)))

;; setup org-ref
(let ((org-ref-file "~/.emacs.d/addons/org-ref.org"))
  (when (file-exists-p org-ref-file)
    (org-babel-load-file org-ref-file t)))

(setq reftex-default-bibliography '("~/Perfezionamento/topics/topics.bib")
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Perfezionamento/topics/")

(define-key org-mode-map (kbd "C-c C-v k") 'org-babel-remove-result)
(define-key org-mode-map (kbd "C-;") 'tex-goto-prev-dollar)
(define-key org-mode-map (kbd "C-'") 'tex-goto-next-dollar)
(define-key org-mode-map (kbd "M-;") 'tex-goto-prev-backslash)
(define-key org-mode-map (kbd "M-'") 'tex-goto-next-backslash)
(define-key org-mode-map (kbd "<f5>") 'org-mark-ring-goto)

(provide 'cnf-org)

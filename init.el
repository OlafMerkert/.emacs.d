;; setup environment variables
;; (setenv "CDPATH"
;;         (replace-regexp-in-string "~" (getenv "HOME")
;;                                   ".:..:~:~/Projekte:~/Perfezionamento/projects"))

(when (>= emacs-major-version 24)
  (require 'package)
  ;; additional repos for elpa
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (package-initialize)
  
  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages
    '(starter-kit
      starter-kit-eshell
      starter-kit-js
      starter-kit-lisp
      starter-kit-bindings
      ido-vertical-mode
      color-theme-solarized
      ;; zenburn-theme
      ;; anti-zenburn-theme
      expand-region
      ace-jump-mode
      ;; js2-mode
      auctex
      yasnippet
      lua-mode
      ecb
      org
      w3m
      ipython
      iy-go-to-char
      iedit
      multiple-cursors
      mark-more-like-this
      mark-multiple
      magit
      elpy
      bbdb
      dash
      smartparens
      )
    "my default selection of packages, to be automatically
  installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defmacro defpar (var value)
  `(progn (defvar ,var)
          (setf ,var ,value)))

(setq desktop-path '("~/.emacs.d/sessions/"))
(make-directory (car desktop-path) t)
(desktop-save-mode 1)

(load-theme 'anti-zenburn t)

(require 'starter-kit)
(require 'starter-kit-lisp)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(add-hook 'js-mode-hook 'js2-minor-mode)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; disable some unwanted stuff from the starter kit

;; general settings
(column-number-mode t)
(setq initial-scratch-message    nil
      woman-use-own-frame        nil
      make-backup-files          nil
      ps-print-color-p           'black-white
      uniquify-buffer-name-style 'post-forward
      c-default-style            "k&r" ; C indentation style
      whitespace-line-column     80
      whitespace-style           '(face trailing tabs lines-tail empty indentation))

(setq-default indent-tabs-mode nil)

;; spellchecking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; don't flyspell automatically
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; use english in commit messages
(defun use-english-dictionary ()
  (interactive)
  (ispell-change-dictionary "en_GB"))

(add-hook 'magit-log-edit-mode-hook 'use-english-dictionary)

;; use visual line instead of auto-fill
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; disable hl-line-mode
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; browser
(setq browse-url-generic-program   "xdg-open"
      browse-url-chromium-program  "google-chrome"
      browse-url-browser-function  'browse-url-chromium
      w3m-use-cookies              t
      w3m-pop-up-windows           t
      w3m-use-tab                  nil)

;; docview
(setq doc-view-continuous t)

;; projects
(setq ecb-source-path
      '(("~/bin" "config")
        ("~/texmf/tex/latex/olaf" "texmf")
        ("~/Projekte/ol-utils" "ol-utils")
        ("~/Projekte/math-utils" "math-utils")
        ("~/Projekte/continued-fractions" "continued-fractions")
        ("~/Projekte/web-document-gallery" "web-document")
        ("~/Studium/MasterArbeit" "master")))

;; eshell
(defun eshell-paths-hook ()
  (or (getenv "CDPATH")
      (setenv "CDPATH" ".:..:~:~/Projekte:~/Perfezionamento/projects"))
  (setenv "EDITOR" "emacsclient"))

(add-hook 'eshell-mode-hook 'eshell-paths-hook)

;; snippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; use nxml for html editing
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . nxml-mode))

;; prettify xml code
(defun prettify-xml ()
  (interactive)
  (replace-string "><" ">
<")
  (indent-region (point-min) (point-max)))


(dolist (cnf '("functions"
               "lisp"
               "python"
               "tex"
               "bindings"
               "personal"
               "email"
               "org"
               ;; "smartparens"
               ))
  (load (concatenate 'string "~/.emacs.d/cnf-" cnf)))

;;; fix for using tramp::sudo
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Personal/bookmarks.org" "~/Projekte/simple-image-gallery/readme.org" "~/Projekte/personal-website/readme.org" "~/Projekte/web-utils/readme.org" "~/Personal/spiele.org" "~/Personal/sns.org" "~/Personal/notizen.org" "~/Personal/privat.org" "~/Perfezionamento/thesis/phd-thesis.en.org")))
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp)
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

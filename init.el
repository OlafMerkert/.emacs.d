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
      ;; solarized-theme
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
      )
    "my default selection of packages, to be automatically
  installed at launch.")

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(setq desktop-path '("~/.emacs.d/sessions/"))
(desktop-save-mode 1)

(require 'starter-kit)
(require 'starter-kit-lisp)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(add-hook 'js-mode-hook 'js2-minor-mode)

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

;; browser
(setq browse-url-generic-program   "xdg-open"
      browse-url-browser-function  'browse-url-chromium
      w3m-use-cookies              t
      w3m-pop-up-windows           t
      w3m-use-tab                  nil)

;; projects
(setq ecb-source-path
      '(("~/bin" "config")
        ("~/texmf/tex/latex/olaf" "texmf")
        ("~/Projekte/ol-utils" "ol-utils")
        ("~/Projekte/math-utils" "math-utils")
        ("~/Projekte/continued-fractions" "continued-fractions")
        ("~/Projekte/web-document-gallery" "web-document")
        ("~/Studium/MasterArbeit" "master")))

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
  (indent-region (buffer-end -1) (buffer-end 1)))


(dolist (cnf '("functions"
               "lisp"
               "tex"
               "bindings"
               "personal"
               "email"))
  (load (concatenate 'string "~/.emacs.d/cnf-" cnf)))

(windmove-default-keybindings)

;;; if we want to show the same buffer left and right, call these
(defun same-buffers (&optional arg)
  (interactive "P")
  (if arg
      ;; copy buffer in inactive window to active window
      (set-window-buffer (get-mru-window) (window-buffer (get-lru-window)))
      ;; copy buffer in active window to inactive window
      (set-window-buffer (get-lru-window) (window-buffer (get-mru-window)))))

;;; some stuff for python programming
(elpy-enable)
(elpy-clean-modeline)

(defun search-on-line (word)
  ;; assume we are at the end of the line
  (let ((point (point)))
    (beginning-of-line)
    (prog1 (search-forward word point t)
      (goto-char point))))

(defun blank-line-p (&optional n)
  ;; again assume we are at the end of the line
  (let ((point (point)))
    (beginning-of-line (- 2 (or n 1)))
    (skip-syntax-forward (concat " "
                                 (char-to-string (char-syntax ?\n)))
                         point)
    (prog1 (<= point (point))
      (goto-char point))))

(defun py-smart-newline ()
  (interactive)
  (cond ((blank-line-p 2)
         (newline))
        ((or (blank-line-p)
             (search-on-line "pass")
             (search-on-line "return"))
         (newline-and-indent)
         (delete-char -4)) ; todo use a smarter function for this
        (t (newline-and-indent))))

;; indent function for python that knows about preceding
;; newlines, return, pass
;; figure out what to do with nested function definitions and class
;; definition: perhaps the convention is that single blank lines,
;; return and pass removes one level, while double blank lines reset
;; indentation to 0

(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Package . emacs) (Package . HUNCHENTOOT) (Syntax . COMMON-LISP) (Base . 10) (Syntax . Common-Lisp) (Package . Maxima) (ispell-dictionary . "en_GB") (ispell-dictionary . "english") (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

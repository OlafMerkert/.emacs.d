
;; don't open files with some endings
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".aux"
        ".bbl"
        ".blg"
        ".meta"
        ".out"
        ".synctex.gz"
        ".tdo"
        ".toc"
        "-pkg.el"
        "-autoloads.el"
        "auto/"
        ".fasl"
        ".pyc"))

(add-to-list 'clean-local-keybindings "C-j")

(use-package avy
    :ensure t
    :config (setf avy-keys
                  '(?a ?s ?d ?f ?j ?k ?l  ?g ?h ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m))
    :bind (("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line)
           ("C-j" . avy-goto-word-1)))

(use-package iy-go-to-char
    :ensure t
    :bind (("M-j"  . iy-go-to-char)
           ("M-J"  . iy-go-to-char-backward)))

;; org-ref requires this:
(use-package key-chord
    :ensure t
    ;; :init (key-chord-mode 1)
    )

(use-package ace-window
    :ensure t
    :bind ("C-x o" . ace-window))

(require 'windmove)

(defun split-window-automatically (&optional size)
  "split window horizontally or vertically, depending on how much
horizontal space is available."
  (interactive "P")
  (if (< 140 (window-width))
      (split-window-horizontally size)
      (split-window-vertically size)))

(defhydra window-manager (global-map "C-x")
  "window"
  ("b" ido-switch-buffer "switch buffer")
  ("B" ido-switch-buffer-other-window "switch other buffer")
  ("C-f" ido-find-file "open file")
  ("k" ido-kill-buffer "kill buffer")
  ;; ("F" ido-find-file-other-window)
  ("<left>" windmove-left "<")
  ("<down>" windmove-down "v")
  ("<up>" windmove-up "^")
  ("<right>" windmove-right ">")
  ("o" ace-window "other" :color blue)
  ;; ("f" other-frame :color blue)
  ;; ("s" split-window-automatically "split")
  ("0" delete-window "del win")
  ("1" delete-other-windows "del other")
  ("2" split-window-vertically "split vert")
  ("3" split-window-horizontally "split horiz")
  ("+" balance-windows "balance")
  ("q" nil)
  ;; ("y" bury-buffer "bury")
  )

;;; if we want to show the same buffer left and right, call these
(defun same-buffers (&optional arg)
  (interactive "P")
  (if arg
      ;; copy buffer in inactive window to active window
      (set-window-buffer (get-mru-window) (window-buffer (get-lru-window)))
      ;; copy buffer in active window to inactive window
      (set-window-buffer (get-lru-window) (window-buffer (get-mru-window)))))

;; jump to next/previous occurence of symbol at point
(defun jump-next-word-occurence (&optional count)
  (interactive "p")
  (let* ((target-symbol (symbol-at-point))
         (target (symbol-name target-symbol))
         (pos (point)))
    (when (and target-symbol
               (not (in-string-p))
               (looking-at-p "\\s_\\|\\sw") ;; Symbol characters
               )
      ;; move forward to end of symbol
      (forward-symbol 1)
      (let ((advance (- (point) pos))
            (case-fold-search nil))
        (if (minusp count) (forward-symbol -1))
        (setq regexp (concat "\\_<" (regexp-quote target) "\\_>"))
        (search-forward-regexp regexp nil t (or count 1))
        (if (minusp count) (forward-symbol 1))
        ;; move backward again
        (backward-char advance)))))

(defun jump-prev-word-occurence (&optional count)
  (interactive "p")
  (jump-next-word-occurence (if count (- count) -1)))

(global-set-key (kbd "C-S-n") 'jump-next-word-occurence)
(global-set-key (kbd "C-S-p") 'jump-prev-word-occurence)

;; adapted from http://emacs.stackexchange.com/questions/10359/delete-portion-of-isearch-string-that-does-not-match-or-last-char-if-complete-m
(defun isearch-delete-failed-or-char ()
  "Delete the failed portion of the search string, or the last char if successful."
  (interactive)
  (with-isearch-suspended
      (setq isearch-new-string
            (substring
             isearch-string 0 (or (isearch-fail-pos) (max 0 (1- (length isearch-string)))))
            isearch-new-message
            (mapconcat 'isearch-text-char-description isearch-new-string ""))
    ;; FIX if the search-string becomes empty, the suspended macro
    ;; falls back to search history, so it becomes impossible to
    ;; change the first char.
   ))

;; (define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
     (let ((case-fold-search isearch-case-fold-search))
       (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; make M-v go precisely where we started from with C-v
(setq scroll-preserve-screen-position 'always)

;; record position in files
(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)

;; find files at point
(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(use-package find-file-in-project :ensure t)

(use-package page-break-lines
    :ensure t
    :init (add-hook 'prog-mode-hook 'page-break-lines-mode)
    ;; TODO find a better replacement for this
    :config (setf page-break-lines-char ?_))

;; when moving to next page, move page start to first line
(defun recenter-page (&rest args)
  (recenter 0))

(advice-add 'forward-page :after 'recenter-page)
(advice-add 'backward-page :after 'recenter-page)

;; from http://endlessparentheses.com/improving-page-navigation.html#comment-2254443893
(defhydra hydra-page-break (global-map "C-x")
  "page breaks"
  ("[" backward-page "back")
  ("]" forward-page "forward"))

(provide 'cnf-navigation)

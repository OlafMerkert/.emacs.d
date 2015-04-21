(use-package ace-jump-mode
    :ensure t
    :bind ("C-j" . ace-jump-mode))

(add-to-list 'clean-local-keybindings "C-j")

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

(global-set-key
 (kbd "<f5>")
 (defhydra window-manager ()
   "window"
   ("b" ido-switch-buffer "sw buf")
   ("f" ido-find-file "open file")
   ("h" windmove-left "<")
   ("j" windmove-down "\/")
   ("k" windmove-up "^")
   ("l" windmove-right ">")
   ("o" ace-window "other")
   ("a" ace-window "ace" :color blue)
   ;; ("f" other-frame :color blue)
   ("s" split-window-automatically "split")
   ("0" delete-window "del win")
   ("1" delete-other-windows "del other")
   ("2" split-window-vertically "split vert")
   ("3" split-window-horizontally "split horiz")
   ("=" balance-windows "balance")))

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
             isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
            isearch-new-message
            (mapconcat 'isearch-text-char-description isearch-new-string ""))))

(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-failed-or-char)

(provide 'cnf-navigation)

(use-package ace-jump-mode
    :ensure t
    :bind ("C-j" . ace-jump-mode))

(use-package iy-go-to-char
    :ensure t
    :bind (("M-j"  . iy-go-to-char)
           ("M-J"  . iy-go-to-char-backward)))

(require 'windmove)
(global-set-key (vector '(shift left)) 'windmove-left)
(global-set-key (vector '(shift right)) 'windmove-right)
(global-set-key (vector '(shift up)) 'windmove-up)
(global-set-key (vector '(shift down)) 'windmove-down)

(use-package ace-window
    :ensure t
    :bind ("C-x o" . ace-window))

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

(provide 'cnf-navigation)

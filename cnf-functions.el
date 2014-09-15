
(defun align-regexp-all (beg end regexp)
  "Align the current region using an ad-hoc separator read from
the minibuffer.  Unless a prefix argument is given, alignment
will be repeated.  This is most useful to align tables, for
instance in TeX."
  (interactive
   (list (region-beginning) (region-end)
         (concat "\\(\\s-*\\)"
                 (read-string "Align regexp repeatedly: "))))
  (align-regexp beg end regexp
                1 align-default-spacing
                (not current-prefix-arg)))


(defun copy-line-to-other-window ()
  "Copy contents from a buffer to the other window, line by
line."
  (interactive)
  (let ((content (filter-buffer-substring (progn (forward-line 0) (point))
                                         (progn (forward-line 1) (point)))))
    (other-window 1)
    (insert content)
    (other-window -1)))

;;; if we want to show the same buffer left and right, call these
(defun same-buffers (&optional arg)
  (interactive "P")
  (if arg
      ;; copy buffer in inactive window to active window
      (set-window-buffer (get-mru-window) (window-buffer (get-lru-window)))
      ;; copy buffer in active window to inactive window
      (set-window-buffer (get-lru-window) (window-buffer (get-mru-window)))))

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

;; neat little tip from Howard Abrams
(defadvice transpose-words (before transpose-at-eol)
  "Transpose last two words when at end of line"
  (if (looking-at "$")
      (backward-word 1)))

(ad-activate 'transpose-words)

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

(provide 'cnf-functions)

(defun copy-line-to-other-window ()
  "Copy contents from a buffer to the other window, line by
line."
  (interactive)
  (let ((content (filter-buffer-substring (progn (forward-line 0) (point))
                                          (progn (forward-line 1) (point)))))
    (other-window 1)
    (insert content)
    (other-window -1)))

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


(defun beginning-of-word ()
  ;; todo not working yet
  (save-excursion
    (cond ((looking-at "[:space:]") nil)
          ((progn (backward-char)
                  (looking-at "[:space:]")) t)
          (t nil))))

(provide 'cnf-functions)

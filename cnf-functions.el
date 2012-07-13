
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

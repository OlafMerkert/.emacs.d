(use-package multiple-cursors
    :ensure t
    :bind (("C-&" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this)
           ("C-*" . mc/mark-all-like-this-dwim)))

(use-package iedit
    :commands iedit-mode
    :ensure t
    :bind (("C-;" . iedit-dwim)))

(defun iedit-dwim (arg)
  "Starts iedit but uses `narrow-to-defun' to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (narrow-to-defun)
          (if iedit-mode
              (iedit-done)
              ;; `current-word' can of course be replaced by other
              ;; functions.
              (iedit-start (current-word)))))))

;; aligning
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

(global-set-key (kbd "C-x a") 'align-regexp)
(global-set-key (kbd "C-x A") 'align-regexp-all)

;; neat little tip from Howard Abrams
(defun transpose-words--at-eol (arg)
  "Transpose last two words when at end of line"
  (if (looking-at "$")
      (backward-word 1)))

(advice-add 'transpose-words :before 'transpose-words--at-eol)

(global-set-key
 (kbd "<f12>")
 (defhydra editing-actions ()
   "editing actions"
   ("h" hl-line-mode "highlight current line")
   ("l" linum-mode "toggle line numbers")
   ("b" same-buffers "same buffers" :color blue)
   ("g" find-grep "find" :color blue)
   ("w" whitespace-cleanup "clean whitespace" :color blue)
   ("p" insert-provide "insert provide" :color blue)))

(provide 'cnf-editing)

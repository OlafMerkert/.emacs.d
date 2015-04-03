(use-package multiple-cursors
    :ensure t
    :bind (("C-&" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this)
           ("C-*" . mc/mark-all-like-this-dwim)))

(use-package iedit
    :ensure t)

(defun iedit-mode/defun (&optional arg)
  (interactive "P")
  (if iedit-mode
      (iedit-mode arg)
      (if arg
          (iedit-mode)
          (iedit-mode 0))))

(define-key prog-mode-map (kbd "C-;") 'iedit-mode)
(define-key lisp-mode-map (kbd "C-;") 'iedit-mode)

;; aligning
(defun align-regexp-all (beg end regexp)
  "Align the current region using an ad-hoc separator read from
the minibuffer. Unless a prefix argument is given, alignment will
be repeated. This is most useful to align tables, for instance in
TeX."
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

(defun insert-eqref-link ()
  (interactive)
  (let ((pos (point)))
    (org-ref-helm-insert-ref-link)
    (save-excursion
      (goto-char pos)
      (insert "eq"))))

(global-set-key
 (kbd "<f12>")
 (defhydra editing-actions ()
   "editing actions"
   ("h" hl-line-mode "highlight current line")
   ("l" linum-mode "toggle line numbers")
   ("b" same-buffers "same buffers" :color blue)
   ("g" find-grep "find" :color blue)
   ("w" whitespace-cleanup "clean whitespace" :color blue)
   ("p" insert-provide "insert provide" :color blue)
   ("r" org-ref-helm-insert-ref-link "ref" :color blue)
   ("e" insert-eqref-link "eqref" :color blue)
   ("c" org-ref-helm-insert-cite-link "cite" :color blue)
   ("u" toggle-browser "use w3m")
   ("q" nil "quit")))

;;; a variant of `query-replace' which takes current region as
;;; reference, then asks for replacement and replaces to end of buffer
(defun region-query-replace (backward)
  (interactive "P")
  (barf-if-buffer-read-only)
  (when mark-active
    (let* ((from (buffer-substring-no-properties (region-beginning) (region-end)))
           (to (query-replace-read-to from (concat "Query replace"
                                                   (if backward " backward" ""))
                                      nil)))
      (deactivate-mark)
      (query-replace from to nil
                     (if (not backward) (region-beginning) (point-min))
                     (if backward (region-end) (point-max))
                     backward))))

(global-set-key (kbd "C-%") 'region-query-replace)

(provide 'cnf-editing)

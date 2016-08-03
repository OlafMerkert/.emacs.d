;; use nxml for html editing
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . nxml-mode))

;; prettify xml code
(defun prettify-xml ()
  (interactive)
  (replace-string "><" ">
<")
  (indent-region (point-min) (point-max)))

(after-load 'nxml-mode
  (bind-keys :map nxml-mode-map
             ("<return>" . reindent-then-newline-and-indent)
             ("C-c p" . prettify-xml)))

(after-load 'css-mode
  (bind-keys :map css-mode-map ("<return>" . reindent-then-newline-and-indent)))

(provide 'cnf-xhtml)

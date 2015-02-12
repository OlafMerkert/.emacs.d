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
    (define-key nxml-mode-map (kbd "<return>") 'reindent-then-newline-and-indent)
  (define-key nxml-mode-map (kbd "C-c p") 'prettify-xml))

(after-load 'css-mode
    (define-key css-mode-map  (kbd "<return>") 'reindent-then-newline-and-indent))

(provide 'cnf-xhtml)

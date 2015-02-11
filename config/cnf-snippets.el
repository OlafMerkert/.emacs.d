;; snippets
(use-package yasnippet
    :ensure t
    :mode ("\\.yasnippet$" . snippet-mode)
    :init (progn
            (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-global-mode 1)))

;; prompt for name and create appropriate new yasnippet
(defun yas-create-snippet (snippet-name snippet-abbrev)
  (interactive
   (list (read-string "Snippet name: ")
         (read-string "Snippet abbrev: ")))
  (let ((snippet-filename (concat (first yas-snippet-dirs)
                                  "/"
                                  (downcase (symbol-name major-mode))
                                  "/"
                                  snippet-name
                                  ".yasnippet")))
    (find-file snippet-filename)
    (insert "# -*- mode: snippet -*-
# name: " snippet-name "
# key: " snippet-abbrev "
# --
")
    (not-modified)))

(global-set-key (kbd "C-x M-s") 'yas-create-snippet)

(provide 'cnf-snippets)

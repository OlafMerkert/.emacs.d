(setq c-default-style            "k&r"  ; C indentation style
      whitespace-line-column     80
      whitespace-style           '(face trailing tabs lines-tail empty indentation))

(setq-default indent-tabs-mode nil)

(use-package js2-mode
    :ensure t
    :commands js2-minor-mode
    :init (add-hook 'js-mode-hook 'js2-minor-mode))

(setq js-indent 4)

(defun esk-pretty-function ()
  (font-lock-add-keywords
   nil `(("\\(function *\\) *("
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "\u0192"
                                    'decompose-region)))))))

(add-hook 'js-mode-hook 'esk-pretty-function)

(use-package ac-js2
    :ensure t
    :config
    (add-hook 'js2-mode-hook 'ac-js2-mode)
    (setq ac-js2-evaluate-calls t))

;; (add-to-list 'ac-js2-external-libraries "path/to/lib/library.js")

(use-package js-comint
    :ensure t
    :config
    (setenv "NODE_NO_READLINE" "1")
    (setq inferior-js-program-command "/usr/bin/node")
    (defun install-js-comint-bindings ()
      (local-set-key "\C-x\C-e" 'js-send-last-sexp)
      (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
      (local-set-key "\C-cb" 'js-send-buffer)
      (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
      (local-set-key "\C-cl" 'js-load-file-and-go))
    (add-hook 'js-mode-hook 'install-js-comint-bindings))

(use-package json-mode :ensure t)

(use-package lua-mode :ensure t :commands lua-mode)

(after-load 'c-mode
  (define-key c-mode-map  (kbd "<return>") 'reindent-then-newline-and-indent))

;; auto fill comments
(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

;; (add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-add-watchwords)

(provide 'cnf-prog)

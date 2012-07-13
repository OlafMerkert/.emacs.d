(setf slime-load-hook nil)
(require 'slime-autoloads)
(slime-setup '(slime-repl
               slime-fuzzy
               slime-c-p-c
               slime-autodoc-mode
               slime-banner
               slime-editing-commands
               slime-presentations
               slime-typeout-frame
               slime-fancy-inspector
               slime-tramp
               slime-highlight-edits))

(setq slime-lisp-implementations
      '(;;(sbcl ("/usr/bin/sbcl"))
        ;;(sbcl ("/usr/bin/sbcl" "--core"
        ;;            "/home/olaf/sbcl-gtk2.core"))
        (sbcl ("/usr/bin/sbcl"
               "--dynamic-heap-size" "4096"
               "--control-stack-size" "16"))
        (clisp ("/usr/bin/clisp"))
        (ccl ("/usr/local/bin/ccl"))))

(defun extract-last-sexp ()
  (let ((opoint (point)))
    (backward-sexp)
    (prog1 (buffer-substring (point) opoint)
      (goto-char opoint))))

(defun extract-next-sexp ()
  (let ((opoint (point)))
    (forward-sexp)
    (prog1 (buffer-substring (point) opoint)
      (goto-char opoint))))

(defun extract-this-sexp ()
  (let ((opoint (point)))
    (backward-sexp)
    (let ((bpoint (point)))
      (forward-sexp)
      (prog1 (buffer-substring bpoint (point))
        (goto-char opoint)))))

(defun multiply-last-sexp (&optional arg)
  (interactive (list (if current-prefix-arg current-prefix-arg 2)))
  (let ((last-sexp (extract-last-sexp)))
   (dotimes (i (- arg 1))
     (just-one-space 1)
     (insert last-sexp))))

(defun multiply-last-sexp-reader (&optional arg)
  (interactive (list (if current-prefix-arg current-prefix-arg 1)))
  (let ((opoint (point))
        (reader-assign (format "#%d=" arg)))
    (backward-sexp)
    (insert reader-assign)
    (goto-char (+ opoint
                  (length reader-assign)))) 
  (just-one-space 1)
  (insert (format "#%d#" arg)))

(defun defun-this-symbol ()
  (interactive)
  (let ((symbol (extract-this-sexp)))
    (end-of-buffer)
    (newline)
    (newline-and-indent)
    (insert "(defun " symbol " ())")
    (backward-char 2)))

(defun defgeneric-next ()
  (interactive)
  (let ((opoint (point)))
    (newline-and-indent)
    (search-forward "defmethod")
    (let ((name (extract-next-sexp)))
      (goto-char opoint)
      (insert "(defgeneric" name " ())")
      (backward-char 2))))

(setq lisp-indent-function 'common-lisp-indent-function
      ;; slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

(eval-after-load 'slime
  '(progn
    (define-key slime-mode-map (kbd "TAB")   'slime-indent-and-complete-symbol)
    ;; (define-key slime-mode-map (kbd "C-;")   'slime-insert-balanced-comments)
    ;; (define-key slime-mode-map (kbd "C-'")   'slime-remove-balanced-comments)
    ;; (define-key slime-mode-map (kbd "C-c s") 'clos-insert-accessor-and-initarg)

    (define-key slime-mode-map (kbd "C-c g") 'defgeneric-next)

    (define-key slime-mode-map (kbd "C-!")   (lambda () (interactive) (multiply-last-sexp-reader 1)))
    (define-key slime-mode-map (kbd "C-@")   (lambda () (interactive) (multiply-last-sexp-reader 2)))
    (define-key slime-mode-map (kbd "C-#")   (lambda () (interactive) (multiply-last-sexp-reader 3)))))

(define-key lisp-mode-map (kbd "C-2")   (lambda () (interactive) (multiply-last-sexp 2)))
(define-key lisp-mode-map (kbd "C-3")   (lambda () (interactive) (multiply-last-sexp 3)))
(define-key lisp-mode-map (kbd "C-4")   (lambda () (interactive) (multiply-last-sexp 4)))
(define-key lisp-mode-map (kbd "C-c f") 'defun-this-symbol)
(define-key emacs-lisp-mode-map (kbd "C-2")   (lambda () (interactive) (multiply-last-sexp 2)))
(define-key emacs-lisp-mode-map (kbd "C-3")   (lambda () (interactive) (multiply-last-sexp 3)))
(define-key emacs-lisp-mode-map (kbd "C-4")   (lambda () (interactive) (multiply-last-sexp 4)))
(define-key emacs-lisp-mode-map (kbd "C-c f") 'defun-this-symbol)

(dolist (mode '(lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook mode (lambda () (paredit-mode 1))))

(provide 'cnf-lisp)

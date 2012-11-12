;; get slime from quicklisp
(defvar ql-slime-helper "~/.quicklisp/slime-helper.el")

(when (file-exists-p ql-slime-helper)
  (load ql-slime-helper)
  (slime-setup '(slime-fancy
                 slime-banner 
                 slime-tramp
                 slime-highlight-edits)))

(setq slime-lisp-implementations
      '(;;(sbcl ("/usr/bin/sbcl"))
        (sbcl ("/usr/bin/sbcl" "--core"
                   "/home/olaf/sbcl-gtk2.core"))
        ;; (sbcl ("/usr/bin/sbcl"
        ;;        "--dynamic-heap-size" "4096"
        ;;        "--control-stack-size" "16"))
        (clisp ("/usr/bin/clisp"))
        (ccl ("/usr/local/bin/ccl"))))

(defun slime-sl2z ()
  (interactive)
  ;; setup translators
  (setq slime-to-lisp-filename-function
        (lambda (file-name)
          (subseq file-name (length "/ssh:olaf@sl2z.de:")))
        slime-from-lisp-filename-function
        (lambda (file-name)
          (concat "/ssh:olaf@sl2z.de:" file-name)))
  ;; connect to slime on server
  (slime-connect "127.0.0.1" 4005))

(defun slime-local ()
  (interactive)
  ;; setup translators
  (setq slime-to-lisp-filename-function   #'convert-standard-filename
        slime-from-lisp-filename-function #'identity)
  ;; start slime
  (slime))

(defun slime-selector-or-start (arg)
  (interactive "P")
  (if (and (fboundp 'slime-connected-p)
           (slime-connected-p))
      (slime-selector)
      (if arg ; connect to remote swank on server
          (slime-sl2z)
          (slime-local))))

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

;; customisations of indenting
(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

(setq lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-alignment t
      lisp-indent-maximum-backtracking 7)

(eval-after-load 'slime
  '(progn
    (define-key slime-mode-map (kbd "TAB")   'slime-indent-and-complete-symbol)
    (define-key slime-repl-mode-map (kbd "<backspace>") 'paredit-backward-delete)
    (define-key slime-repl-mode-map (kbd "<delete>") 'paredit-forward-delete)
    (define-key slime-repl-mode-map (kbd "C-d") 'paredit-forward-delete)
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

;; adjustments to indentation
(setf (get 'ew 'common-lisp-indent-function)
      '(&rest 0))

(provide 'cnf-lisp)

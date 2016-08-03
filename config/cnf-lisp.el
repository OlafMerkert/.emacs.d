(use-package elisp-slime-nav
    :ensure t
    :init (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

;; enable some hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))))

;; turn everything for prog-mode on as well
(defun esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(add-hook 'emacs-lisp-mode-hook 'esk-prog-mode-hook)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)


(defun insert-provide ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "(provide '" (file-name-base (buffer-file-name)) ")\n")))

;; pretty lambda and function
(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)

(defun esk-pretty-fn ()
    (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                   (0 (progn (compose-region (match-beginning 1)
                                                             (match-end 1)
                                                             "\u0192"
                                                             'decompose-region)))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)
(add-hook 'clojurescript-mode-hook 'esk-pretty-fn)

;; get slime from quicklisp
(defvar ql-slime-helper "~/.quicklisp/slime-helper.el")

(when (file-exists-p ql-slime-helper)
  (load ql-slime-helper)
  (slime-setup '(slime-fancy
                 slime-fuzzy
                 slime-banner
                 slime-tramp
                 slime-highlight-edits)))

;; completion setup
(setf slime-complete-symbol*-fancy t)

;; on some machines, sbcl or ccl might have been compiled from source
(defun try-local-path (exec-name)
  (let ((local-path (concat "/usr/local/bin/" exec-name))
        (path (concat "/usr/bin/" exec-name)))
    (if (file-exists-p local-path)
        local-path path)))

(setq slime-lisp-implementations
      `((sbcl (,(try-local-path "sbcl")))
        (ccl (,(try-local-path "ccl")))
        (clisp ("/usr/bin/clisp"))
        ))


(defun slime-setup-forwarding (server-login start-lisp)
  (let ((buffer "*remote inferior lisp*"))
    (unless (get-buffer buffer)
      (shell buffer)
      (switch-to-buffer buffer)
      (comint-simple-send buffer (concat "ssh -L4005:127.0.0.1:4005 " server-login "\n"))
      (when start-lisp
        (comint-simple-send buffer "sbcl\n")
        (comint-simple-send buffer "(ql:quickload 'swank)\n")
        (comint-simple-send buffer
                            "(progn

(swank:create-server :port 4005 :dont-close nil)
                              (setf swank:*use-dedicated-output-stream* nil))\n")
        (comint-simple-send buffer "\"forwarding ready\"\n"))
      (unless start-lisp
        (comint-simple-send buffer "echo \"forwarding ready\"\n"))
      (sit-for 20)
      t)))

(defvar server-prefix-length 0)
(defvar server-prefix "")

(defun slime-remote (host &optional username start-lisp)
  (interactive)
  (unless username
    (setf username "olaf"))
  (let ((server-login (concat username "@" host)))
    (setf server-prefix (concat "/ssh:" server-login ":")
          server-prefix-length (length server-prefix))
    ;; open ssh connection if none is open
    (save-window-excursion
      (slime-setup-forwarding server-login start-lisp))
    ;; setup translators
    (setq slime-to-lisp-filename-function
          (lambda (file-name) (subseq file-name server-prefix-length))
          slime-from-lisp-filename-function
          (lambda (file-name) (concat server-prefix file-name)))
    ;; connect to slime on server
    (slime-connect "127.0.0.1" 4005)))

(defun slime-sl2z ()
  (interactive)
  (slime-remote "sl2z.de" "olaf" nil))

(defun slime-maglor ()
  (interactive)
  (slime-remote "maglor" "olaf" t))

(defun cleanup-remote-slime-buffers ()
  (interactive)
  (kill-buffer "*remote inferior lisp*")
  (kill-buffer "*slime-repl sbcl*"))

(defun slime-local (&optional command)
  (interactive)
  ;; setup translators
  (setq slime-to-lisp-filename-function   #'convert-standard-filename
        slime-from-lisp-filename-function #'identity)
  ;; start slime
  (slime command))

;; change default package to ol-user
;; (add-hook 'slime-connected-hook
;;           (lambda () (slime-repl-set-package "OL-USER"))
;;           t)

(defun slime-sbcl ()
  (interactive)
  (slime-local 'sbcl))

(defun slime-ccl ()
  (interactive)
  (slime-local 'ccl)
  (slime-repl-set-package "OL-USER"))

(defun slime-clisp ()
  (interactive)
  (slime-local 'clisp))

(defhydra slime-start (:color blue)
  "slime"
  ("r" slime-local "default")
  ("s" slime-sbcl "sbcl")
  ("R" slime-sl2z "sl2z")
  ("M" slime-maglor "maglor")
  ("z" slime-ccl "ccl")
  ("c" slime-clisp "clisp")
  ("q" cleanup-remote-slime-buffers "cleanup"))

(defun slime-selector-or-start ()
  (interactive)
  (if (and (fboundp 'slime-connected-p)
           (slime-connected-p))
      (slime-selector)
      (slime-start/body)))

(global-set-key (kbd "<f9>") 'slime-selector-or-start)

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

(defun multiply-last-sexp-2 () (interactive) (multiply-last-sexp 2))
(defun multiply-last-sexp-3 () (interactive) (multiply-last-sexp 3))
(defun multiply-last-sexp-4 () (interactive) (multiply-last-sexp 4))

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
    (beginning-of-defun)
    (newline 2)
    (previous-line 2)
    (indent-according-to-mode)
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

;; completion
;; slime-complete-symbol-function

;; customisations of indenting
(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(let ((local-hyperspec-paths (list "/usr/share/doc/hyperspec/"
                                   "/usr/share/doc/HyperSpec/")))
  (setq common-lisp-hyperspec-root nil)
  (dolist (path local-hyperspec-paths)
    (when (file-exists-p path)
      (setf common-lisp-hyperspec-root (concat "file://" path))))
  (unless common-lisp-hyperspec-root
    (setf common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/")))

(setq lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-alignment t
      lisp-indent-maximum-backtracking 7)

(use-package paredit
    :ensure t
    :config (diminish 'paredit-mode))

(after-load 'paredit
  (define-key paredit-mode-map (kbd "C-M-<backspace>") 'backward-kill-sexp)
  (define-key paredit-mode-map (kbd "C-j") nil))

(after-load 'slime
  (define-key slime-mode-map      (kbd "TAB")             'slime-indent-and-complete-symbol)
  (define-key slime-repl-mode-map (kbd "<backspace>")     'paredit-backward-delete)
  (define-key slime-repl-mode-map (kbd "<delete>")        'paredit-forward-delete)
  (define-key slime-mode-map      (kbd "C-M-<backspace>") 'backward-kill-sexp)
  (define-key slime-repl-mode-map (kbd "C-d")             'paredit-forward-delete)
  (define-key slime-mode-map      (kbd "C-x C")           'slime-insert-balanced-comments)
  (define-key slime-mode-map      (kbd "C-c g")           'defgeneric-next)
  (define-key slime-mode-map      (kbd "C-c #")           'multiply-last-sexp-reader)
  (define-key slime-mode-map      (kbd "C-c C-<return>")  'slime-macroexpand-1-inplace)
  (define-key slime-scratch-mode-map (kbd "C-j") nil)
  (define-key slime-repl-mode-map (kbd "C-j") nil)
  )

(define-key emacs-lisp-mode-map (kbd "C-c RET")
  (lambda (arg)
    (interactive "P")
    (save-excursion
      (forward-sexp)
      (pp-macroexpand-last-sexp arg))))

(define-key emacs-lisp-mode-map (kbd "C-c C-t") 'trace-function)

(dolist (mode-map (list lisp-mode-map
                        emacs-lisp-mode-map))
  (define-key mode-map       (kbd "C-2")   'multiply-last-sexp-2)
  (define-key mode-map       (kbd "C-3")   'multiply-last-sexp-3)
  (define-key mode-map       (kbd "C-4")   'multiply-last-sexp-4)
  (define-key mode-map       (kbd "C-c f") 'defun-this-symbol))

(defun turn-on-paredit ()
  (paredit-mode 1)) 

(dolist (mode '(lisp-mode-hook
                emacs-lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook mode 'turn-on-paredit))

;; adjustments to indentation
(defmacro copy-cl-indentation (&rest mapping)
  `(setf ,@(mapcar (lambda (x) `(get ',x 'common-lisp-indent-function))
                   (flatten mapping))))

(setf (get 'ew 'common-lisp-indent-function) '(&rest 1)
      (get 'eval-when 'common-lisp-indent-function) '(2 &rest 0))

(copy-cl-indentation (defmethod* defmethod)
                     (defgeneric* defgeneric)
                     (mvbind multiple-value-bind)
                     (dbind destructuring-bind)
                     )

(put 'defpar 'common-lisp-indent-function '(&rest))

;;; TODO improve highlighting of important (custom) macros
(font-lock-add-keywords 'lisp-mode
                        '(("(\\(defmacros?!\\|defpar\\|defalias\\)[ \n]+\\([^ ()\n]+\\)"
                           (1 font-lock-keyword-face)
                           (2 font-lock-function-name-face)))
                        t)

(font-lock-add-keywords 'lisp-mode
                        '(("(\\(awhen\\|aif\\|aprog1\\|alambda\\|acond\\)[ \n]+"
                           (1 font-lock-keyword-face)))
                        t)

;; for hu.dwim.def
(let ((file "~/.quicklisp/dists/quicklisp/software/hu.dwim.def-20140713-darcs/emacs/hu.dwim.def.el"))
  (if (file-exists-p file) (load-file file)))

;;; nicer indentation for cl-who
;; for common html tags
(defun repeated (n item &optional tail)
  (if (<= n 0) tail
      (repeated (- n 1) item (cons item tail))))

(defun common-lisp-hyperspec--use-w3m (f lookup-term)
  "Use w3m as for `browse-url' for the CL hyperspec."
  (let ((browse-url-browser-function 'w3m-browse-url))
    (funcall f lookup-term)))

(advice-add 'common-lisp-hyperspec :around 'common-lisp-hyperspec--use-w3m)

(advice-add 'common-lisp-hyperspec-format :around 'common-lisp-hyperspec--use-w3m)

(advice-add 'common-lisp-hyperspec-lookup-reader-macro :around 'common-lisp-hyperspec--use-w3m)

;; open .sexp files with common-lisp-mode
(add-to-list 'auto-mode-alist '("\\.sexp$" . common-lisp-mode))

(defun clim-command-name (string)
  (substitute ?\s ?- (capitalize string)))

(defun slime-search-buffer-package-custom ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t'#:]*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

(setf slime-find-buffer-package-function 'slime-search-buffer-package-custom)

(defcustom clim-application 'app
  "Class Name of the current CLIM application, we also expect a
  function of the same name which starts the CLIM application."
  :type 'symbol
  :risky nil
  :safe (lambda (val) t))

(defun start-clim-application ()
  (interactive)
  (let ((form (concat "(" (slime-current-package) "::" (prin1-to-string clim-application) ")")))
    (message "Starting CLIM application  %s  ..." form)
    (slime-interactive-eval form)))

(define-key slime-mode-map (kbd "S-<f9>") 'start-clim-application)

(provide 'cnf-lisp)

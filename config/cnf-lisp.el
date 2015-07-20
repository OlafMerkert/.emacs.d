(defun insert-provide ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "(provide '" (file-name-base (buffer-file-name)) ")\n")))

;; pretty lambda
(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'prog-mode-hook 'esk-pretty-lambdas)

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

(defun slime-sl2z ()
  (interactive)
  ;; setup translators
  (setq slime-to-lisp-filename-function   (lambda (file-name) (subseq file-name 18))
        slime-from-lisp-filename-function (lambda (file-name) (concat "/ssh:olaf@sl2z.de:" file-name)))
  ;; connect to slime on server
  (slime-connect "127.0.0.1" 4005))

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
  ("d" slime-local "default")
  ("s" (slime-sbcl) "sbcl")
  ("r" slime-sl2z "remote")
  ("z" (slime-ccl) "ccl")
  ("c" (slime-clisp) "clisp")
  )

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

(dolist (mode '(lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook mode (lambda () (paredit-mode 1))))

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

;; for hu.dwim.def
(let ((file "~/.quicklisp/dists/quicklisp/software/hu.dwim.def-20140713-darcs/emacs/hu.dwim.def.el"))
  (if (file-exists-p file) (load-file file)))

;;; nicer indentation for cl-who
;; for common html tags
(defun repeated (n item &optional tail)
  (if (<= n 0) tail
      (repeated (- n 1) item (cons item tail))))

(let ((rep2 (repeated 10 3))
      (rep0 (repeated 10 1)))
  (mapc (lambda (tag) (put tag 'common-lisp-indent-function rep2))
        '(:a :abbr :acronym :address :applet :area :article :aside :audio :b :base :basefont :bdi :bdo :big :blockquote :body :br :button :canvas :caption :center :cite :code :col :colgroup :command :datalist :dd :del :details :dfn :dir :div :dl :dt :em :embed :fieldset :figcaption :figure :font :footer :form :frame :frameset :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html :i :iframe :img :input :ins :keygen :kbd :label :legend :li :link :map :mark :menu :meta :meter :nav :noframes :noscript :object :ol :optgroup :option :output :p :param :pre :progress :q :rp :rt :ruby :s :samp :script :section :select :small :source :span :strike :strong :style :sub :summary :sup :table :tbody :td :textarea :tfoot :th :thead :time :title :tr :track :tt :u :ul :var :video :wbr))
  ;; for common css properties
  (mapc (lambda (prop) (put prop 'common-lisp-indent-function rep0))
        '(:color :opacity :background :background-attachment :background-color :background-image :background-position :background-repeat :background-clip :background-origin :background-size :border :border-bottom :border-bottom-color :border-bottom-left-radius :border-bottom-right-radius :border-bottom-style :border-bottom-width :border-color :border-image :border-image-outset :border-image-repeat :border-image-slice :border-image-source :border-image-width :border-left :border-left-color :border-left-style :border-left-width :border-radius :border-right :border-right-color :border-right-style :border-right-width :border-style :border-top :border-top-color :border-top-left-radius :border-top-right-radius :border-top-style :border-top-width :border-width :box-decoration-break :box-shadow :bottom :clear :clip :display :float :height :left :overflow :overflow-x :overflow-y :padding :padding-bottom :padding-left :padding-right :padding-top :position :right :top :visibility :width :vertical-align :z-index :align-content :align-items :align-self :display :flex :flex-basis :flex-direction :flex-flow :flex-grow :flex-shrink :flex-wrap :justify-content :margin :margin-bottom :margin-left :margin-right :margin-top :max-height :max-width :min-height :min-width :order :hanging-punctuation :hyphens :letter-spacing :line-break :line-height :overflow-wrap :tab-size :text-align :text-align-last :text-indent :text-justify :text-transform :white-space :word-break :word-spacing :word-wrap :text-decoration :text-decoration-color :text-decoration-line :text-decoration-style :text-shadow :text-underline-position :font :font-family :font-feature-setting :@font-feature-values :font-kerning :font-language-override :font-synthesis :font-variant-alternates :font-variant-caps :font-variant-east-asian :font-variant-ligatures :font-variant-numeric :font-variant-position :font-size :font-style :font-variant :font-weight :@font-face :font-size-adjust :font-stretch :direction :text-orientation :text-combine-horizontal :unicode-bidi :writing-mode :border-collapse :border-spacing :caption-side :empty-cells :table-layout :counter-increment :counter-reset :list-style :list-style-image :list-style-position :list-style-type :@keyframes :animation :animation-delay :animation-direction :animation-duration :animation-fill-mode :animation-iteration-count :animation-name :animation-timing-function :animation-play-state :backface-visibility :perspective :perspective-origin :transform :transform-origin :transform-style :transition :transition-property :transition-duration :transition-timing-function :transition-delay :box-sizing :content :cursor :icon :ime-mode :nav-down :nav-index :nav-left :nav-right :nav-up :outline :outline-color :outline-offset :outline-style :outline-width :resize :text-overflow :break-after :break-before :break-inside :column-count :column-fill :column-gap :column-rule :column-rule-color :column-rule-style :column-rule-width :column-span :column-width :columns :widows :orphans :page-break-after :page-break-before :page-break-inside :marks :quotes :filter :image-orientation :image-rendering :image-resolution :object-fit :object-position :mask :mask-type :mark :mark-after :mark-before :phonemes :rest :rest-after :rest-before :voice-balance :voice-duration :voice-pitch :voice-pitch-range :voice-rate :voice-stress :voice-volume :marquee-direction :marquee-play-count :marquee-speed
          :marquee-style)))

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

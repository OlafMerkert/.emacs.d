;; get rid of white background
(set-background-color "lavender")
(add-to-list 'default-frame-alist '(background-color . "lavender"))

(defun font-select (name &optional size)
  (interactive "sFont name: ")
  (unless size
    (setf size 12))
  (set-frame-font (format "%s-%s" name size) nil t))

;; (font-select "Times New Roman" 13)

(defmacro def-font-selector (&rest fonts)
  `(defhydra font-family-selector (global-map "<f1>")
     "font"
     ("q" nil "quit")
     ,@(mapcar (lambda (font)
                 (destructuring-bind (key name &optional size) font
                   `(,key (lambda () (interactive) (font-select ,name ,size)) ,name)))
               fonts)))

(def-font-selector
  ("a" "Hack" 10)
  ("h" "Hermit" 10)
  ("p" "PT Mono" 10)
  ("d" "Deja Vu Sans Mono" 10)
  ("s" "Source Code Pro Light" 11)
  ("S" "Source Sans Pro" 11)
  ("o" "Droid Sans Mono")
  ("i" "Inconsolata" 12)
  ("t" "Terminus" 12)
  ("l" "Liberation Mono")
  ("u" "LucidaTypewriter")
  ("m" "Monaco" 11)
  ("n" "Nimbus Mono")
  ("g" "GentiumAlt" 12)
  ("e" "Envy Code R" 12)
  ("c" "Comic Sans MS" 12)
  ("P" "Palladio URW" 12)
  ("T" "Times New Roman" 13))

;; use more prominent colours for the modeline of the active buffer
(set-face-attribute 'mode-line
                    nil 
                    :foreground "black"
                    :background "pale green" 
                    :box '(:line-width 1 :style released-button))

(set-face-attribute 'mode-line-inactive
                    nil 
                    :foreground "gray30"
                    :background "gray90"
                    :box '(:line-width 1 :style released-button))

(set-face-attribute 'region nil :background "antiquewhite2")

(set-face-attribute 'magit-diff-file-heading
                    nil
                    :weight 'normal)

(global-set-key (kbd "C-x f") (lambda () (interactive)
                                 (let ((face (face-at-point)))
                                   ;; (kill-new face)
                                   (message "%s" face))))

(defmacro set-fg-colors (pre post &rest conses)
  `(progn
     ,@(mapcar
        (lambda (c)
          `(set-face-attribute ',(symb pre (symbol-name (first c)) post)
                               nil
                               :foreground ,(second c)
                               ,@(cddr c)))
        conses)))

;; programming colours
(set-fg-colors "font-lock-" "-face"
               (builtin        "royalblue3")
               (constant       "midnightblue")
               (keyword        "dodgerblue")
               (string         "royalblue2")
               (type           "blue3")
               (function-name  "blue" :underline nil :weight 'semi-bold :slant 'normal)
               (variable-name  "darkblue" :underline nil :weight 'normal :slant 'normal)
               (comment        "steelblue4")
               (warning        "tomato"))

;; org colours
(set-fg-colors "org-" ""
               (level-1  "blue2" :weight 'bold)
               (level-2  "orange3" :weight 'normal)
               (level-3  "blue4" :weight 'normal)
               (level-4  "goldenrod3" :weight 'normal)
               (level-5  "dodgerblue3" :weight 'normal)
               (level-6  "gold3" :weight 'normal))

(after-load 'font-latex
  (set-fg-colors "font-" "-face"
                 (latex-math "maroon")))

(after-load 'git-commit
  (set-fg-colors "git-commit-" ""
                 (summary "blue" :weight 'semi-bold)))

;; highlighting of parenthesis in a subdued colour
(defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (when (> (display-color-cells) 8)
      (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                              '(("(\\|)" . 'esk-paren-face)))))

(use-package highlight-numbers
    :ensure t
    :diminish highlight-numbers-mode
    :init (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    :config (set-face-attribute 'highlight-numbers-number nil :foreground "maroon"))

(provide 'cnf-colours)

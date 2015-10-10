;; get rid of white background
(set-background-color "ghost white")
(add-to-list 'default-frame-alist '(background-color . "ghost white"))

(defmacro def-font-selector (&rest fonts)
  `(defhydra font-family-selector (global-map "<f1>")
     "font"
     ("q" nil "quit")
     ,@(mapcar (lambda (font)
                 (destructuring-bind (key name &optional size) font
                     (unless size
                       (setf size 10))
                   `(,key (lambda () (interactive) (set-frame-font ,(format "%s-%s" name size) nil t)) ,name)))
               fonts)))

(def-font-selector
  ("a" "Hack" 10)
  ("h" "Hermit" 10)
  ("p" "PT Mono" 11)
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
  ("e" "Envy Code R" 12))

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

(set-face-attribute 'region
                    nil
                    :background "LightSteelBlue1")

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
          `(set-face-attribute ',(symb pre (symbol-name (car c)) post)
                               nil
                               :foreground ,(cdr c)))
        conses)))

;; programming colours
(set-fg-colors "font-lock-" "-face"
               (builtin . "midnight blue")
               (constant . "midnight blue")
               (keyword . "blue")
               (function-name . "DodgerBlue2")
               (variable-name . "DodgerBlue3")
               (string . "midnight blue")
               (comment . "LightSteelBlue4")
               (type . "DodgerBlue4"))

(set-face-attribute 'font-lock-function-name-face
                    nil
                    :weight 'bold)

(set-face-attribute 'font-lock-variable-name-face
                    nil
                    :weight 'bold)

(set-face-attribute 'font-lock-type-face
                    nil
                    :weight 'bold)

;; org colours
(set-fg-colors "org-" ""
               (level-1 . "royal blue")
               (level-2 . "steel blue")
               (level-3 . "dodger blue")
               (level-4 . "sky blue")
               (level-5 . "blue3"))

(set-face-attribute 'org-level-2 nil :weight 'normal)

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



(provide 'cnf-colours)

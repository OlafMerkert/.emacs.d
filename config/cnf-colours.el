;; get rid of white background
(set-background-color "oldlace")
(add-to-list 'default-frame-alist '(background-color . "oldlace"))

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
                    :background "azure2")

(set-face-attribute 'magit-diff-file-heading
                    nil
                    :weight 'normal)

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

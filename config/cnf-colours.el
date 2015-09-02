;; get rid of white background
(set-background-color "oldlace")
(add-to-list 'default-frame-alist '(background-color . "oldlace"))

(defhydra font-family-selector (global-map "<f1>")
  "font"
  ("q" nil "quit")
  ("a" (lambda () (interactive) (set-frame-font "Hack-10" nil t)) "Hack")
  ("h" (lambda () (interactive) (set-frame-font "Hermit-10" nil t)) "Hermit")
  ("p" (lambda () (interactive) (set-frame-font "PT Mono-11" nil t)) "PT Mono")
  ("d" (lambda () (interactive) (set-frame-font "Deja Vu Sans Mono-10" nil t)) "Deja Vu Sans Mono")
  ("s" (lambda () (interactive) (set-frame-font "Source Code Pro-11" nil t)) "Source Code Pro"))

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

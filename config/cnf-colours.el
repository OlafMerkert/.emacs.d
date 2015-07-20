;; get rid of white background
(set-background-color "oldlace")
(add-to-list 'default-frame-alist '(background-color . "oldlace"))

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

(provide 'cnf-colours)

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

(provide 'cnf-colours)

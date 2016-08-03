;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-r") 'revert-buffer)

(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") 'shell)

(global-set-key (kbd "C-x c") 'comment-dwim)

;; rebound movement keys
(defvar my-keys-minor-mode-map (make-sparse-keymap)
  "Personal customisation of keybindings.")
(define-minor-mode my-keys-minor-mode
    "A minor mode with adjusted keybindings."
  nil " K" 'my-keys-minor-mode-map)

;; disable in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (my-keys-minor-mode 0)))

(defvar minor-mode-map-precedence
  '((my-keys-minor-mode . 100)
    (yas/minor-mode     . 80)
    (paredit-mode       . 60)))

(defun load--give-my-keybindings-priority (&rest args)
  "Try to ensure that my keybindings always have priority."
  (setf minor-mode-map-alist
        (sort minor-mode-map-alist
              (lambda (a b)
                (let ((prec-a (cdr (assq (car a) minor-mode-map-precedence)))
                      (prec-b (cdr (assq (car b) minor-mode-map-precedence))))
                  (cond ((not (or prec-a prec-b)) t)
                        ((not prec-a) nil)
                        ((not prec-b) t)
                        (t (> prec-a prec-b))))))))

;;(advice-add 'load :after 'load--give-my-keybindings-priority)

;; (define-key my-keys-minor-mode-map (kbd "C-<return>") 'copy-line-to-other-window)

;; Move more quickly
;; (global-set-key (kbd "C-S-f") (lambdai (forward-char 5)))
;; (global-set-key (kbd "C-S-b") (lambdai (backward-char 5)))

(global-set-key (kbd "M-=") 'count-words)

(global-set-key (kbd "C-x M-f") 'find-file-in-project)

(global-set-key (kbd "C-x TAB") 'indent-rigidly)

(define-key 'help-command "a" 'apropos) ; don't just search for commands

(use-package 2048-game
    :commands '2048-game
    :config
    (bind-keys :map  2048-mode-map
               ("h" . 2048-left)
               ("j" . 2048-down)
               ("k" . 2048-up)
               ("l" . 2048-right)))

(provide 'cnf-bindings)

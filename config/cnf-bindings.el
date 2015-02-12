;; rebound movement keys
(defvar my-keys-minor-mode-map (make-sparse-keymap)
  "Personal customisation of keybindings.")
(define-minor-mode my-keys-minor-mode
    "A minor mode with adjusted keybindings."
  t " K" 'my-keys-minor-mode-map)

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

(advice-add 'load :after 'load--give-my-keybindings-priority)

(define-key my-keys-minor-mode-map (kbd "C-<return>") 'copy-line-to-other-window)

;; Move more quickly
(global-set-key (kbd "C-S-f") (lambda () (interactive) (forward-char 5)))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (backward-char 5)))

(provide 'cnf-bindings)

;; enable some commands
(put 'downcase-region   'disabled nil)
(put 'upcase-region     'disabled nil)
(put 'capitalize-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; global key settings
(global-set-key (kbd "<f6>")       'magit-status)
(global-set-key (kbd "<f9>")       'slime-selector)
(global-set-key (kbd "S-<return>") 'split-line)
(global-set-key (kbd "C-x M-o")    'other-frame)
(global-set-key (kbd "C-x c")      'comment-dwim)

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

(defadvice load (after give-my-keybindings-priority)
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

(ad-activate 'load)

(require 'mark-more-like-this)(require 'expand-region)
(define-key my-keys-minor-mode-map (kbd "C-j")        'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "M-j")        'iy-go-to-char)
(define-key my-keys-minor-mode-map (kbd "C-x a")      'align-regexp)
(define-key my-keys-minor-mode-map (kbd "C-x A")      'align-regexp-all)
(define-key my-keys-minor-mode-map (kbd "C-<return>") 'copy-line-to-other-window)
(define-key my-keys-minor-mode-map (kbd "C-<")        'mark-previous-like-this)
(define-key my-keys-minor-mode-map (kbd "C->")        'mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "C-M-m")      'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(define-key my-keys-minor-mode-map (kbd "C-*")        'mark-all-like-this)
(define-key my-keys-minor-mode-map (kbd "C-o")        'er/expand-region)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (forward-char 5)))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (backward-char 5)))


(eval-after-load 'nxml-mode
  '(define-key nxml-mode-map (kbd "<return>") 'reindent-then-newline-and-indent))
(eval-after-load 'css-mode
  '(define-key css-mode-map  (kbd "<return>") 'reindent-then-newline-and-indent))

(eval-after-load 'gnus-art
  '(define-key gnus-article-mode-map (kbd "C-c C-s") 'gnus-article-save-part))

(provide 'cnf-bindings)

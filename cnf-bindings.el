;; global key settings
(global-set-key (kbd "<f12> h")    'hl-line-mode)
(global-set-key (kbd "<f12> l")    'linum-mode)
(global-set-key (kbd "<f6>")       'magit-status)
(global-set-key (kbd "C-c g")      nil)
(global-set-key (kbd "<f8>")       'gnus)
(global-set-key (kbd "<f9>")       'slime-selector-or-start)
(global-set-key (kbd "S-<f9>")     'slime-local-alt)
(global-set-key (kbd "S-<return>") 'split-line)
(global-set-key (kbd "C-x M-o")    'other-frame)
(global-set-key (kbd "C-x c")      'comment-dwim)
(global-set-key (kbd "<f12> b")    'same-buffers)
(global-set-key (kbd "<f12> g")    'find-grep)
(global-set-key (kbd "<f11>")      nil)
(global-set-key (kbd "<f11> a")    'org-agenda)
(global-set-key (kbd "<f11> c")    'org-capture)
(global-set-key (kbd "<f11> l")    'org-store-link)
(global-set-key (kbd "<f11> b")    'org-iswitchb)
(global-set-key (kbd "<f11> B")    'bbdb)
(global-set-key (kbd "<f11> s")    'sync-personal-information)
(global-set-key (kbd "<f11> u")    'browse-url-at-point)
(global-set-key (kbd "C-c w")      'whitespace-cleanup)

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

(after-load 'css-mode
    (define-key css-mode-map  (kbd "<return>") 'reindent-then-newline-and-indent))



(after-load 'gnus-art
  (define-key gnus-article-mode-map (kbd "C-c C-s") 'gnus-article-save-part))

(after-load 'bibtex
  (define-key bibtex-mode-map (kbd "C-j") 'bibtex-next-field))

;; in tex, regexp search can be distracting (we often want to search
;; for `$'
(after-load 'tex
    (define-key TeX-mode-map (kbd "C-s") 'isearch-forward)
  (define-key TeX-mode-map (kbd "C-r") 'isearch-backward)
  (define-key TeX-mode-map (kbd "M-%") 'query-replace)
  (define-key TeX-mode-map (kbd "C-M-s") 'isearch-forward-regexp)
  (define-key TeX-mode-map (kbd "C-M-r") 'isearch-backward-regexp)
  (define-key TeX-mode-map (kbd "C-M-%") 'query-replace-regexp))


(provide 'cnf-bindings)

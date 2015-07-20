;; enable some commands
(put 'downcase-region   'disabled nil)
(put 'upcase-region     'disabled nil)
(put 'capitalize-region 'disabled nil)
(put 'narrow-to-region  'disabled nil)

;; repeat last command
(global-set-key (kbd "C-z") 'repeat)

(use-package dash :ensure t)
(use-package fullframe :ensure t)

;; disable hl-line-mode
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
;; remove filename completion
(after-load 'hippie-exp
  (dolist (f '(try-expand-line
               try-expand-list
               try-complete-file-name-partially
               try-complete-file-name))
    (setq hippie-expand-try-functions-list
          (delete f hippie-expand-try-functions-list))))

(column-number-mode t) ; show line and column in modeline
(setq initial-scratch-message    nil
      woman-use-own-frame        nil
      make-backup-files          nil
      ps-print-color-p           'black-white)

;; present files with same names in a nice way
(require 'uniquify)
(setq uniquify-buffer-name-style) 'post-forward

(use-package ido
    :ensure t
    :config (setq ido-enable-prefix nil
                  ido-enable-flex-matching t
                  ido-auto-merge-work-directories-length nil
                  ido-create-new-buffer 'always
                  ido-use-filename-at-point 'guess
                  ido-use-virtual-buffers t)
    :init (ido-mode t))

(use-package ido-ubiquitous
    :ensure t
    :init (ido-ubiquitous-mode))

(use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode 1))

(use-package helm :ensure t)

(use-package hydra :ensure t)

(use-package ibuffer
    :init (setq ibuffer-use-other-window t
                ibuffer-default-shrink-to-minimum-size nil
                ibuffer-default-directory "~")
    (defun ibuffer-buffer-list ()
      (interactive)
      (ibuffer t "*Buffer List*"))
    :commands (ibuffer)
    :bind ("C-x C-b" . ibuffer-buffer-list)
    :config
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h (:name "Size" :inline t)
      (cond ((> (buffer-size) 1000000000) (format "%7.1fG" (/ (buffer-size) 1000000000.0)))
            ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
            ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
            (t (format "%8d" (buffer-size)))))
    (setq ibuffer-formats
          '((mark modified read-only " " (name 42 42 :left :elide) " " (mode 16 16 :left :elide) " " (size-h 9 -1 :right) " " filename-and-process)
            (mark modified read-only " " (name 42 42 :left :elide) " " (size-h 9 -1 :right) " " filename-and-process)
            (mark modified read-only vc-status-mini " " (name 20 20 :left :elide) " " (size-h 9 -1 :right) " " (mode 16 16 :left :elide) " "
             filename-and-process)
            (mark modified read-only vc-status-mini " " (name 20 20 :left :elide) " " (size-h 9 -1 :right) "  " (vc-status 16 16 :left) " " filename-and-process)
            (mark modified read-only " " (name 30 -1 :left) " " filename-and-process))))

;; if we call `ibuffer' from itself, then `ibuffer-quit' does not work
;; anymore. So just update instead
(after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-x C-b") 'ibuffer-update)
  (define-key ibuffer-mode-map (kbd "g") 'ibuffer-update))

;; make sure *scratch* returns nil under `ibuffer-vc-root'
;; (defun ibuffer-vc-root--exclude-scratch (next buf)
;;   (unless (string= "*scratch*" (buffer-name buf))
;;     (funcall next buf)))

;; (advice-add 'ibuffer-vc-root :around 'ibuffer-vc-root--exclude-scratch)

;;; make sure point never goes onto prompt of minibuffer
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;;; some keybindings
(global-set-key (kbd "S-<return>") 'split-line)
(global-set-key (kbd "C-x M-o")    'other-frame)
(global-set-key (kbd "C-x c")      'comment-dwim)

(after-load 'comint
  ;; removing all output from comint buffers
  (defun comint-flush-buffer ()
    "Clear the current comint-buffer."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer )))
  (define-key comint-mode-map (kbd "C-c M-o") 'comint-flush-buffer))

(provide 'cnf-base)

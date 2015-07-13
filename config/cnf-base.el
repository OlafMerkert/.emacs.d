;; enable some commands
(put 'downcase-region   'disabled nil)
(put 'upcase-region     'disabled nil)
(put 'capitalize-region 'disabled nil)
(put 'narrow-to-region  'disabled nil)

;; repeat last command
(global-set-key (kbd "C-z") 'repeat)

(use-package dash :ensure t)
(use-package fullframe :ensure t)

;; TODO replace starter-kit with own customisations
(use-package starter-kit :ensure t)
(use-package starter-kit-lisp :ensure t)
(use-package starter-kit-eshell :ensure t)
(use-package starter-kit-js :ensure t)
(use-package starter-kit-bindings :ensure t)
;; disable hl-line-mode
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
;; remove filename completion
(after-load 'hippie-exp
  (dolist (f '(try-complete-file-name-partially
               try-complete-file-name))
    (setq hippie-expand-try-functions-list
          (delete f hippie-expand-try-functions-list))))

(column-number-mode t) ; show line and column in modeline
(setq initial-scratch-message    nil
      woman-use-own-frame        nil
      make-backup-files          nil
      ps-print-color-p           'black-white
      uniquify-buffer-name-style 'post-forward)

(use-package ido :ensure t)

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

(use-package ibuffer-vc
    :init (progn (require 'vc)
                 (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :config
    (defun ibuffer-vc-set-filter-groups-by-vc-root ()
      "Set the current filter groups to filter by vc root dir."
      (interactive)
      (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
      (message "ibuffer-vc: groups set")
      (let ((ibuf (get-buffer "*Buffer List*")))
        (when ibuf
          (with-current-buffer ibuf
            (pop-to-buffer ibuf)
            (ibuffer-update nil t))))))

;; if we call `ibuffer' from itself, then `ibuffer-quit' does not work
;; anymore. So just update instead
(after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-x C-b") 'ibuffer-vc-set-filter-groups-by-vc-root)
  (define-key ibuffer-mode-map (kbd "g") 'ibuffer-vc-set-filter-groups-by-vc-root))


;;; make sure point never goes onto prompt of minibuffer
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;;; some keybindings
(global-set-key (kbd "S-<return>") 'split-line)
(global-set-key (kbd "C-x M-o")    'other-frame)
(global-set-key (kbd "C-x c")      'comment-dwim)

;; removing all output from comint buffers
(defun comint-flush-buffer ()
  "Clear the current comint-buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer )))

(provide 'cnf-base)

(use-package smartparens
    :ensure t
    :config (require 'smartparens-config))

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-M-d" . sp-down-sexp)
 ("C-M-u"   . sp-backward-up-sexp)
 ("C-M-n" . sp-up-sexp)
 ("C-M-p" . sp-backward-down-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ;; ("C-S-f" . sp-forward-symbol)
 ;; ("C-S-b" . sp-backward-symbol)

 ("C-("  . sp-backward-slurp-sexp)
 ("C-)" . sp-forward-slurp-sexp)
 ("C-{"  . sp-backward-barf-sexp)
 ("C-}" . sp-forward-barf-sexp)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("C-M-DEL"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)

 ;;("C-M-d" . delete-sexp)

 ("DEL" . sp-backward-delete-char)
 ("C-d" . sp-delete-char)
 ("M-DEL" . sp-backward-kill-word)
 ("M-d" . sp-kill-word)
 

 ("M-[" . sp-split-sexp)
 ("M-]" . sp-join-sexp)

 ;; ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("M-<up>" . sp-splice-sexp-killing-backward)
 ("M-<down>" . sp-splice-sexp-killing-forward)
 ("M-s" . sp-splice-sexp)
 ("M-r" . sp-raise-sexp)
 ;; ("M-?" . convolute)

 ;; TODO commenting
 )

;; (add-to-list 'sp-navigate-consider-stringlike-sexp 'emacs-lisp-mode)

;; (sp-with-modes sp--lisp-modes
;;   (sp-local-pair "(" ")" :wrap ))

;; smartparens-strict-mode
(setq sp-autoinsert-if-followed-by-same 3
      sp-autoinsert-if-followed-by-word t)

(setq-default sp-autoskip-opening-pair nil
              sp-autoskip-closing-pair 'always-end)


;; I do not want to use smartparens for lisp modes, because it simply
;; does not handle double quotes as nicely

;; but for other programming modes it should be useful:
(add-hook 'python-mode-hook 'smartparens-mode)
(add-hook 'js-mode-hook 'smartparens-mode)

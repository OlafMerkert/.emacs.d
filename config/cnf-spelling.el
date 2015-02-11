(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(remove-hook 'text-mode-hook 'turn-on-flyspell)

(global-set-key (kbd "C-x M-l") 'ispell-change-dictionary)

;; use english in commit messages
(defun use-english-dictionary ()
  (interactive)
  (ispell-change-dictionary "en_GB"))

(add-hook 'magit-log-edit-mode-hook 'use-english-dictionary)


(provide 'cnf-spelling)

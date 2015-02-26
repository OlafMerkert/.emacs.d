(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq-default ispell-dictionary "de_DE")

(remove-hook 'text-mode-hook 'turn-on-flyspell)

(global-set-key (kbd "C-x M-l") 'ispell-change-dictionary)

;; use english in commit messages
(defun use-english-dictionary ()
  (interactive)
  (ispell-change-dictionary "en_GB"))

(add-hook 'magit-log-edit-mode-hook 'use-english-dictionary)

;; input methods should respect ispell-dictionary
(defpar dict-input-methods
    '(("de" . "german-postfix")
      ("fr" . "french-postfix")
      ("it" . "italian-postfix")))

(make-variable-buffer-local 'default-input-method)

(defun match-input-method-dictionary (dict &optional arg)
  (aif (assoc (subseq dict 0 2) dict-input-methods)
       (setf default-input-method (cdr it))))

(advice-add 'ispell-change-dictionary :after 'match-input-method-dictionary)

(provide 'cnf-spelling)

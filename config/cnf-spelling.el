(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq-default ispell-dictionary "de_DE")

(remove-hook 'text-mode-hook 'turn-on-flyspell)

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

(global-set-key
 (kbd "C-x M-l")
 (defhydra change-dictionary ()
   "change dictionary"
   ("d" (lambda () (interactive) (ispell-change-dictionary "de_DE")) "deutsch")
   ("e" (lambda () (interactive) (ispell-change-dictionary "en_GB")) "english")
   ("f" (lambda () (interactive) (ispell-change-dictionary "fr_FR")) "français")
   ("i" (lambda () (interactive) (ispell-change-dictionary "it")) "italiano")
   ("q" nil "quit")))

(provide 'cnf-spelling)

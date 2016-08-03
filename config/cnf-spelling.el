(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq-default ispell-dictionary "de_DE")

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
 (defhydra change-dictionary (:color blue)
   "change dictionary"
   ("d" (ispell-change-dictionary "de_DE") "deutsch")
   ("e" (ispell-change-dictionary "en_GB") "english")
   ("f" (ispell-change-dictionary "fr_FR") "français")
   ("i" (ispell-change-dictionary "it") "italiano")
   ("q" nil "quit")))

(defhydra change-input-method (:color blue)
  "select input method"
  ("d" (activate-input-method (match-input-method-dictionary "de_DE")) "deutsch")
  ("e" (activate-input-method (match-input-method-dictionary "en_GB")) "english")
  ("f" (activate-input-method (match-input-method-dictionary "fr_FR")) "français")
  ("i" (activate-input-method (match-input-method-dictionary "it")) "italiano")
  ("o" (activate-input-method
        (read-input-method-name
         (if default-input-method "Input method (default %s): " "Input method: ")
         default-input-method t))
       "other")
  ("q" nil "quit"))

(defun my-toggle-input-method (&optional arg)
  (interactive "P")
  (if (and current-input-method (not arg))
      (deactivate-input-method)
      (if (or arg (not default-input-method))
          (change-input-method/body)
          (activate-input-method default-input-method))))

(global-set-key (kbd "C-\\") 'my-toggle-input-method)

;; enable flyspell for various modes, and program default languages
(dolist (mode-hook '(message-mode-hook git-commit-mode-hook))
  (add-hook mode-hook 'flyspell-mode))

(defun use-en-dictionary ()
  (ispell-change-dictionary "en_GB"))

(dolist (mode-hook '(git-commit-mode-hook))
  (add-hook mode-hook 'use-en-dictionary))

(provide 'cnf-spelling)

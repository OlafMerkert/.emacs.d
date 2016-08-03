(use-package twittering-mode
    :ensure t
    :config (setf twittering-use-master-password t
                  twittering-icon-mode t)
    :commands twit)

(provide 'cnf-twitter)

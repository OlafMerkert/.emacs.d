(setq desktop-path '("~/.emacs.d/sessions/")
      desktop-restore-frames nil)

(make-directory (car desktop-path) t)

(desktop-save-mode 1)

(provide 'cnf-session)

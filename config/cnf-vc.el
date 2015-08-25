(use-package magit
    :ensure t
    :bind ("<f6>" . magit-status)
    :config
    ;; do not ask if commit summary is too long
    (setf git-commit-finish-query-functions nil
          magit-push-always-verify nil))

(use-package ibuffer-vc
    :init
    (progn
      (require 'vc)
      (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :config
    (progn
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
    :bind (:map ibuffer-mode-map
             ("C-x C-b" . ibuffer-vc-set-filter-groups-by-vc-root)
             ("g" . ibuffer-vc-set-filter-groups-by-vc-root)))

(provide 'cnf-vc)

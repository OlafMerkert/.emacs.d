;;; custom `org-protocol' handlers
(defun org-protocol-open-in-w3m (fname)
  (let* ((splitparts (org-protocol-split-data fname t org-protocol-data-separator))
         (uri (org-protocol-sanitize-uri (car splitparts))))
    (w3m-browse-url uri)))

(add-to-list 'org-protocol-protocol-alist
             '("Open in w3m"
               :protocol "w3m"
               :function org-protocol-open-in-w3m))

(defun download-with-youtube-dl (uri &rest args)
  (let ((default-directory "~/Downloads/"))
    (async-shell-command
     (concat "youtube-dl '"
             uri
             "'")
     (generate-new-buffer "*youtube-dl*"))))

(defun org-protocol-download-with-youtube-dl (fname)
  (let* ((splitparts (org-protocol-split-data fname t org-protocol-data-separator))
         (uri (org-protocol-sanitize-uri (car splitparts))))
    (download-with-youtube-dl uri)))

(defun org-link-download-with-youtube-dl ()
  (interactive)
  (let ((browse-url-browser-function 'download-with-youtube-dl))
    (org-open-at-point)))


(add-to-list 'org-protocol-protocol-alist
             '("Download with youtube-dl"
               :protocol "ytdl"
               :function org-protocol-download-with-youtube-dl))

;; workaround missing mode check for `org-label-store-link'
(defun org-label-store-link--test-mode (f)
  (when (derived-mode-p 'org-mode)
    (funcall f)))

(advice-add 'org-label-store-link :around 'org-label-store-link--test-mode)

(defun org-copy-selection (fname)
  (let ((selection-string (car (org-protocol-split-data fname t org-protocol-data-separator))))
    (kill-new selection-string)
    (message "Pushed to kill-ring: %s" selection-string)
    nil))

(add-to-list 'org-protocol-protocol-alist
             '("Copy Selection"
               :protocol "cpsel"
               :function org-copy-selection))

;; sending urls or files by e-mail
(defun last1 (list)
  (car (last list)))

(defun org-protocol-send-email (fname)
  (let* ((splitparts (org-protocol-split-data fname t org-protocol-data-separator))
         (uri (org-protocol-sanitize-uri (car splitparts)))
         (title (cadr splitparts))
         (local (string= "file:" (substring uri 0 5))))
    (when local
      (setf uri (substring uri 5)))
    ;; generate a title for local files
    (unless (and title (< 0 (length title)) local)
      (setf title (last1 (split-string uri "/+"))))
    (compose-mail "" title)
    ;; write link or add attachment
    (save-excursion
      (end-of-buffer)
      (insert "\n")
      (if local
         (mail-attach-file/automatic uri)
         (insert uri "\n")))))

(add-to-list 'org-protocol-protocol-alist
             '("Send link/file by email"
               :protocol "email"
               :function org-protocol-send-email))

(provide 'cnf-org-protocol)

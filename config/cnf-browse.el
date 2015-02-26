(use-package w3m
    :ensure t
    :commands (w3m w3m-browse-url))

;; browser
(setq browse-url-generic-program   "xdg-open"
      browse-url-chromium-program  "google-chrome"
      browse-url-browser-function  'browse-url-generic ;browse-url-firefox
      browse-url-generic-program   "firefox"
      w3m-use-cookies              t
      w3m-pop-up-windows           t
      w3m-use-tab                  nil)

(defun toggle-browser ()
  (interactive)
  (setf browse-url-browser-function
        (if (eq browse-url-browser-function 'w3m-browse-url)
            'browse-url-generic
            'w3m-browse-url))
  (message "Browse with %s" browse-url-browser-function))

;; docview
(setq doc-view-continuous t)

;; searching and downloading torrents
(defun search-torrent (query-string)
  (interactive "sTorrent search string: ")
  (w3m-browse-url (format "https://www.torrentz.com/search?q=%s"
                          (url-encode-url query-string))))

(defun extract-magnet-uri ()
  (interactive)
  (unless (string-match "\\`about://source/" w3m-current-url)
    (w3m-view-source))
  (beginning-of-buffer)
  (search-forward-regexp "\"\\(magnet:[^\"]*\\)\"")
  (let ((magnet (match-string 1)))
    (w3m-view-source)
    (message "MAGNET: %s" magnet)
    magnet))

(defun open-magnet-uri/transmission ()
  (interactive)
  (start-process "transmission" nil
                 "/usr/bin/transmission-gtk"
                 (extract-magnet-uri)))

(provide 'cnf-browse)

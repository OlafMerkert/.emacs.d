;; gnus configuration
(require 'gnus)
(require 'gnus-win)
(require 'message)
(require 'smtpmail)
(use-package bbdb :ensure t)
(require 'bbdb-gnus)

;; gnus window configuration
(setq gnus-use-full-window t)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-subject
        (not gnus-thread-sort-by-most-recent-date)))

(defvar vertical-gnus-buffer-configuration)
(defvar horizontal-gnus-buffer-configuration)

(after-load 'gnus-win
  (setq vertical-gnus-buffer-configuration gnus-buffer-configuration
        horizontal-gnus-buffer-configuration
        (sublis '((vertical . horizontal)
                  (0.25 . 0.5))
                vertical-gnus-buffer-configuration)))

(defun open-gnus ()
  (interactive)
  (setf gnus-buffer-configuration
        (if (< 150 (frame-width))
            ;; wide-screen layout
            horizontal-gnus-buffer-configuration
            vertical-gnus-buffer-configuration))
  (aif (gnus-buffer-exists-p "*Group*")
       (switch-to-buffer it)
       (gnus)))

;; (fullframe gnus gnus-group-exit)

(global-set-key (kbd "<f8>") 'open-gnus)

;; use the local exim server to send mail
;; (setq send-mail-function 'sendmail-send-it
;; message-send-mail-function 'sendmail-send-it)

;; setup smtpmail
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it/detect-server)

(defun smtpmail-send-it/detect-server ()
  (interactive)
  ;; assume we are in a message buffer
  (let* ((from (message-fetch-field "From"))
         (server-settings (or (find-if (lambda (ssm) (search (first ssm) from)) smtp-servers-map)
                              (first smtp-servers-map))))
    (let ((smtpmail-smtp-server (second server-settings))
          (smtpmail-smtp-service (third server-settings))
          (smtpmail-stream-type (fourth server-settings)))
      (smtpmail-send-it))))

;; default mail settings
(setq smtpmail-smtp-server "smtp.1und1.de"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;; different smtp settings based on from adress
(setq smtp-servers-map '(("m-merkert.de" "smtp.1und1.de" 465 ssl)
                         ("sns.it" "smtp.gmail.com" 465 ssl)))

;; mail queue
(setq smtpmail-queue-mail nil)

;; draft folder
(setq nndraft-directory "~/Mail/Gnus/")

;; store sent email on the imap server as well
(setq gnus-message-archive-method nil
       ;; '(nnfolder "archive"
       ;;   (nnfolder-inhibit-expiry t)
       ;;   (nnfolder-directory "~/Mail/archive")
       ;;   (nnfolder-active-file "~/Mail/archive/active"))
       )

(setq message-alternative-emails (regexp-opt (rest user-mail-addresses))
      gnus-ignored-from-addresses (regexp-opt user-mail-addresses))

(setq message-dont-reply-to-names (regexp-opt active-user-mail-addresses))

(defun gnus-sent-messages-folder (&optional narrowed-p)
  "1und1")

(setq gnus-message-archive-group
      '((cond
          ((message-news-p)
           ;; News
           "sent-news")
          ;; Mail
          ((message-mail-p) "1und1"))))

(setq gnus-gcc-mark-as-read t)

(defun message-toggle-alternate ()
  "toggle between the available alternative e-mails available"
  (interactive)
  (let* ((from (message-field-value "From"))
         (pos (position-if (lambda (uma) (search uma from)) active-user-mail-addresses))
         (user-mail-address (if (not pos) user-mail-address
                                (elt active-user-mail-addresses (mod (+ 1 pos) (length active-user-mail-addresses))))))
    (message-replace-header "From" (message-make-from) "Subject")
    (message-replace-header "Gcc" (gnus-sent-messages-folder) "From")))

(define-key message-mode-map (kbd "C-c a") 'message-toggle-alternate)

;; TODO perhaps we do no longer need the following functionality? 
(defun message-use-alternative-email-as-from--adjust-gcc-for-alternative ()
  (message-remove-header "Gcc")
  (insert "Gcc: " (gnus-sent-messages-folder t) "\n"))

(advice-add 'message-use-alternative-email-as-from :after 'message-use-alternative-email-as-from--adjust-gcc-for-alternative)

;; display date in the summary buffer
(defvar gnus-gwene-summary-line-format)
(setq gnus-thread-indent-level 1
      gnus-summary-line-format "%*%U%R%I %(%f%) :%-30=  %s%80=  [ %&user-date; ]\n"
      gnus-gwene-summary-line-format "%*%U%R%I %s%80=  [ %&user-date; ]\n")

;; and use different format for selected gwene feeds
(defvar gwene-no-author-feeds)
(setf gwene-no-author-feeds
      '("heise"
        "tagesschau"
        "questionableco"
        "xkcd"
        "lisp.planet"
        "emacsen.planet"
        "python.planet"
        "debian.planet"
        "fedoraproject.planet"
        "appleoutsider"
        "notebookcheck"
        "endlessparens"
        "kitchin"))

(defun adjust-summary-line-format ()
  (when (and (search "nntp+news.gwene.org" gnus-newsgroup-name)
             (let ((g-n-name-rest (substring gnus-newsgroup-name 20)))
               (some (lambda (x) (search x g-n-name-rest)) gwene-no-author-feeds)))
    (setq gnus-summary-line-format gnus-gwene-summary-line-format)))

(add-hook 'gnus-summary-mode-hook 'adjust-summary-line-format)

;; todo use gnus for compose-mail
(setq mail-user-agent 'gnus-user-agent)

;; enable topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-ignored-newsgroups ""
      mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-select-method '(nnimap "local"
                           (nnimap-stream shell)
                           (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Mail:LAYOUT=fs") 
                           (nnir-search-engine imap)))

(setq gnus-secondary-select-methods
      '(;; (nntp "akk4-dmz.akk.uni-karlsruhe.de")
        ;; use gwene as RSS/Atom -> nntp gateway
        (nntp "news.gwene.org"
         (nntp-open-connection-function nntp-open-tls-stream)
         (nntp-port-number 563)
         (nntp-address "news.gwene.org"))
        ;; (nntp "news.gmane.org"
        ;;  (nntp-open-connection-function nntp-open-tls-stream)
        ;;  (nntp-port-number 563)
        ;;  (nntp-address "news.gmane.org"))
        ))

(defun kill-gnutls-processes ()
  (interactive)
  (call-process "/usr/bin/killall" nil nil nil "gnutls-cli"))


(use-package offlineimap
    :ensure t
    :commands offlineimap
    :init ;;(add-hook 'gnus-before-startup-hook 'offlineimap)
    (defhydra offlineimap-from-gnus (gnus-group-mode-map "I")
      "offlineimap"
      ("f" (lambda ()
             (interactive)
             (if (and (get-buffer "*OfflineIMAP*") (get-buffer-process "*OfflineIMAP*"))
                 (offlineimap-resync)
                 (offlineimap)))
           "sync")
      ("g" (lambda ()
             (interactive)
             (gnus-group-get-new-news))
           "read")
      ("b" (lambda ()
             (interactive)
             (aif (get-buffer "*OfflineIMAP*") (switch-to-buffer it)))
           "to buffer")
      ("G" (lambda ()
             (interactive)
             (aif (gnus-buffer-exists-p "*Group*") (switch-to-buffer it)))
           "groups")
      ("c" offlineimap-quit "close" :color blue)
      ("k" offlineimap-kill "kill")
      ("K" kill-gnutls-processes "kill gnutls")
      ("q" nil "quit")))

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; (setq nnmail-split-methods)

;; blacklist posts by certain people

;; from http://www.emacswiki.org/emacs/BlacklistSpammers on Di 3. Sep 19:09:17 CEST 2013
(defun gnus-summary-blacklist-poster ()
   "Put sender on current line in blacklist."
   (interactive)
   (let ((spammer (mail-header-from (gnus-summary-article-header)))
         (current-score-file gnus-current-score-file))
     (when (gnus-news-group-p gnus-newsgroup-name)
       (gnus-score-change-score-file "all.BLACKLIST")
       (gnus-summary-score-entry "From" spammer 'S' -1001 nil)
       (gnus-score-change-score-file current-score-file)
       (gnus-score-save))))

;; where to move stuff by default
(setq gnus-move-split-methods nil
      gnus-move-group-prefix-function (lambda (group-name) "1und1/Archiv/"))

;;; bbdb configuration
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-mua-update-interactive-p '(query . create)
      bbdb-message-all-addresses t)

(define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)
(define-key bbdb-mode-map (kbd "C-o") 'bbdb-insert-field)

(setq bbdb-file "~/Personal/kontakte.bbdb")

(setq bbdb-send-mail-style 'gnus
      bbdb-mail-user-agent 'gnus-user-agent
      bbdb-complete-name-full-completion t
      bbdb-completion-type 'primary-or-name
      bbdb-complete-name-allow-cycling t)

(setq bbdb-offer-save 1
      bbdb-use-pop-up t
      bbdb-electric-p t
      bddb-popup-target-lines 1)

(setq bbdb-north-american-phone-numbers-p nil
      bbdb-phone-style nil)

;; setup bbdb layouts
(setf bbdb-layout-alist
      '((one-line
         (order . (mail phone notes))
         (name-end  . 24)
         (toggle    . t))
        (multi-line
         (order . (mail mail-alias phone t))
         (omit . (creation-date timestamp
                  name-format name-face
                  bbdb-id
                  asynk:bbdbgoogleprivate:gc
                  asynk:bbdbgoogleprivate:bb
                  ))
         (toggle . t)
         (indentation . 21))
        (pop-up-multi-line  (omit . (creation-date timestamp
                                     name-format name-face))
         (indentation . 21))
        (full-multi-line (indentation . 21))))


(after-load 'bbdb-com
  (defun bbdb-search/wrap (f &rest args)
   (let ((bbdb-xfield-label-list
          (remove 'asynk:bbdbgoogleprivate:gc bbdb-xfield-label-list)))
     (message "calling bbdb-search")
     (apply f args)))

  (dolist (bbdb-fun '(bbdb bbdb-search-xfields))
    (advice-add bbdb-fun :around 'bbdb-search/wrap)))


;;; rss feed commands
(defun gwene-article-open-full ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (widget-backward 1)
    ;; (let ((browse-url-browser-function 'w3m-browse-url)
    ;;       (w3m-pop-up-windows nil)))
    (shr-browse-url)))

(defun gwene-summary-open-full ()
  (interactive)
  (gnus-summary-select-article)
  (gnus-summary-select-article-buffer)
  (gwene-article-open-full)
  (gnus-article-show-only-summary)
  (next-line))

(defun gnus-article-show-only-summary ()
  (interactive)
  (gnus-article-show-summary)
  (gnus-summary-show-only-summary))

(defun gnus-summary-show-only-summary ()
  (interactive)
  (delete-other-windows))

(after-load 'gnus-sum
  (bind-keys :map gnus-summary-mode-map
             ("v" . gnus-summary-show-only-summary)
             ("o" . gwene-summary-open-full)
             ("r" . gnus-summary-wide-reply)
             ("R" . gnus-summary-wide-reply-with-original)
             ("u" . gnus-summary-put-mark-as-unread)))

(after-load 'gnus-art
  (bind-keys :map gnus-article-mode-map
             ("v" . gnus-article-show-only-summary)
             ("o" . gwene-article-open-full)
             ("C-c C-s" . gnus-article-save-part)
             ("r" . gnus-article-wide-reply-with-original)
             ("R" . gnus-article-wide-reply-with-original)))


;; TODO keybindings seem to be overwritten
;; TODO show date in gnus summary

;; insert all email addresses for one alias
(defun bbdb-goto-next-record (n)
  "Move point to the beginning of the next BBDB record.
With prefix N move forward N records. If there is no next record,
return nil."
  (let ((npoint (bbdb-scan-property 'bbdb-record-number 'integerp n)))
    (if npoint (goto-char npoint))))

(defun bbdb-mail-dwim-for-alias (regexp)
  "Produce a list of formatted E-Mail adresses for all records
with `regexp' matching `mail-alias'."
  (bbdb-search-xfields 'mail-alias regexp)
  (let (addresses)
    (with-current-buffer (get-buffer "*BBDB*")
      (do ((pos (goto-char (point-min))
                (bbdb-goto-next-record 1)))
          ((not pos))
        (push (bbdb-dwim-mail (bbdb-current-record)) addresses)))
    addresses))

(defun message-insert-group ()
  "Prompt for a `mail-alias' using ido, then append all mail
addresses with that alias to the recipient list."
  (interactive)
  (save-excursion
    (let* ((alias (ido-completing-read "Mail alias: " (bbdb-get-mail-aliases)))
           (addresses (bbdb-mail-dwim-for-alias alias)))
      ;; go to end of To record
      (message-goto-to)
      (let ((to  (message-field-value "To")))
        (unless (= 0 (length to))
          (unless (save-excursion (backward-char)
                                  (looking-at ","))
            (insert ","))
          (newline)
          (insert "    "))
        (insert (first addresses)))
      (dolist (addr (rest addresses))
        (insert ",")
        (newline)
        (insert "    " addr)))))

(global-set-key (kbd "C-c g") nil)
(define-key message-mode-map (kbd "C-c g") 'message-insert-group)

;; make `mml-attach-file' ask fewer questions
(defun mail-attach-file/automatic (file &optional inline)
  (interactive
   (list (mml-minibuffer-read-file "Attach file: ")
         current-prefix-arg))
  (let* ((type (or (mm-default-file-encoding file)
                   "application/octet-stream"))
         (description nil)
         (disposition (if inline "inline" "attachment")))
    (mml-attach-file file type description disposition)))

(define-key message-mode-map (kbd "C-c C-a") 'mail-attach-file/automatic)

(provide 'cnf-email)

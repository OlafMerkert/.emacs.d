(require 'gnus)

;; use the local exim server to send mail
(setq send-mail-function 'sendmail-send-it)

;; use a remote imap server
(setq gnus-select-method
      '(nnimap "1und1"
        (nnimap-address "imap.1und1.de")
        (nnimap-server-port 993)
        (nnimap-stream tls)
        (nnir-search-engine imap)))

;; store sent email on the imap server as well
 (setq gnus-message-archive-method
       '(nnfolder "archive"
         (nnfolder-inhibit-expiry t)
         (nnfolder-directory "~/Mail/archive")
         (nnfolder-active-file "~/Mail/archive/active")))

(setq gnus-message-archive-group
      '((cond
          ((message-news-p)
           ;; News
           "sent-messages")
          ((message-mail-p)
           ;; Mail
           "nnimap:1und1:INBOX"))))

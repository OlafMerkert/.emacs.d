(require 'gnus)

;; use the local exim server to send mail
(setq send-mail-function 'sendmail-send-it)

;; use the remote 1und1 imap server
;; (setq gnus-select-method
;;       '(nnimap "1und1"
;;         (nnimap-address "imap.1und1.de")
;;         (nnimap-server-port 993)
;;         (nnimap-stream tls)
;;         (nnir-search-engine imap)))

;; use 1und1 news server
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nntp "news.online.de"))

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
           "nnimap+1und1:INBOX"))))

(setq gnus-gcc-mark-as-read t)

;; enable topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)


(setq gnus-select-method '(nntp "news.tin.it"))

(setq gnus-secondary-select-methods
            '((nnimap "1und1"
               (nnimap-address "imap.1und1.de")
               (nnimap-server-port 993)
               (nnimap-stream tls)
               (nnir-search-engine imap))
              (nnimap "sns"
               (nnimap-address "mail.sns.it")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap))))

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

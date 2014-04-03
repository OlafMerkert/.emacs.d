;; gnus configuration
(require 'gnus)
(require 'message)
(require 'smtpmail)
(require 'bbdb)
(require 'bbdb-gnus)

;; use the local exim server to send mail
;; (setq send-mail-function 'sendmail-send-it
;; message-send-mail-function 'sendmail-send-it)

;; setup smtpmail
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(setq smtpmail-smtp-server "smtp.1und1.de"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(setq smtp-servers-map '(("m-merkert.de" "smtp.1und1.de" 465)
                         ("sns.it" "mail.sns.it" 465)))

;; TODO setup automatic usage of different adresses depending on account

;; mail queue
(setq smtpmail-queue-mail nil)

;; store sent email on the imap server as well
(setq gnus-message-archive-method
       '(nnfolder "archive"
         (nnfolder-inhibit-expiry t)
         (nnfolder-directory "~/Mail/archive")
         (nnfolder-active-file "~/Mail/archive/active")))

(setq message-alternative-emails (regexp-opt (rest user-mail-addresses))
      gnus-ignored-from-addresses (regexp-opt user-mail-addresses))

(defun gnus-sent-messages-folder ()
  (if (search "sns.it" (message-field-value "From"))
      "nnimap+sns:INBOX"
      "nnimap+1und1:INBOX"))

(setq gnus-message-archive-group
      '((cond
          ((message-news-p)
           ;; News
           "sent-news")
          ;; Mail
          ((message-mail-p)
           (gnus-sent-messages-folder)))))

(setq gnus-gcc-mark-as-read t)

(defun message-toggle-alternate ()
  "toggle between the available alternative e-mails available"
  (interactive)
  (let* ((from (message-field-value "From"))
         (pos (position-if (lambda (uma) (search uma from)) user-mail-addresses))
         (user-mail-address (if (not pos) user-mail-address
                                (elt user-mail-addresses (mod (+ 1 pos) (length user-mail-addresses))))))
    (message-replace-header "From" (message-make-from) "Subject")
    (message-replace-header "Gcc" (gnus-sent-messages-folder) "From")))

(define-key message-mode-map (kbd "C-c a") 'message-toggle-alternate)


;; todo use gnus for compose-mail

;; enable topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-ignored-newsgroups ""
      mm-discouraged-alternatives '("text/html" "text/richtext"))

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
               (nnir-search-engine imap))
              ;; use gwene as RSS/Atom -> nntp gateway
              (nntp "news.gwene.org")))

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
      gnus-move-group-prefix-function (lambda (group-name) "nnimap+1und1:Archiv/"))

;;; bbdb configuration
;; (require 'message)

;; (require 'bbdb-com)
;; (require 'bbdb-hooks)

(bbdb-initialize 'gnus 'message 'w3)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-mua-update-interactive-p '(query . create)
      bbdb-message-all-addresses t)

(define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)
(define-key bbdb-mode-map (kbd "C-o") 'bbdb-insert-field)

(setq bbdb-file "~/Personal/kontakte.bbdb")

(setq bbdb-send-mail-style 'gnus
      bbdb-complete-name-full-completion t
      bbdb-completion-type 'primary-or-name
      bbdb-complete-name-allow-cycling t)

(setq bbdb-offer-save 1
      bbdb-use-pop-up t
      bbdb-electric-p t
      bddb-popup-target-lines 1)

(setq bbdb-north-american-phone-numbers-p nil)

;;; rss feed commands
(defun gwene-article-open-full ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (widget-backward 1)
    (shr-browse-url)))

(defun gwene-summary-open-full ()
  (interactive)
  (gnus-summary-select-article)
  (gnus-summary-select-article-buffer)
  (gwene-article-open-full)
  (gnus-article-show-summary))


(defun gnus-article-show-only-summary ()
  (interactive)
  (gnus-article-show-summary)
  (gnus-summary-show-only-summary))

(defun gnus-summary-show-only-summary ()
  (interactive)
  (delete-other-windows))

(eval-after-load 'gnus-sum
  '(progn
    (define-key gnus-summary-mode-map (kbd "v") 'gnus-summary-show-only-summary)
    (define-key gnus-summary-mode-map (kbd "o") 'gwene-summary-open-full)))

(eval-after-load 'gnus-art
  '(progn
    (define-key gnus-article-mode-map (kbd "v") 'gnus-article-show-only-summary)
    (define-key gnus-article-mode-map (kbd "o") 'gwene-article-open-full)))

;; todo keybindings seem to be overwritten
;; todo show date in gnus summary

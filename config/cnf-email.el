;; gnus configuration
(require 'gnus)
(require 'message)
(require 'smtpmail)
(use-package bbdb :ensure t)
(require 'bbdb-gnus)

(global-set-key (kbd "<f8>") 'gnus)

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

;; store sent email on the imap server as well
(setq gnus-message-archive-method
       '(nnfolder "archive"
         (nnfolder-inhibit-expiry t)
         (nnfolder-directory "~/Mail/archive")
         (nnfolder-active-file "~/Mail/archive/active")))

(setq message-alternative-emails (regexp-opt (rest user-mail-addresses))
      gnus-ignored-from-addresses (regexp-opt user-mail-addresses))

(defun gnus-sent-messages-folder (&optional narrowed-p)
  (if (search "sns.it" (if narrowed-p
                           (message-fetch-field "From")
                           (message-field-value "From")))
      "nnimap+sns:INBOX"
      "nnimap+1und1:INBOX"))

(setq gnus-message-archive-group
      '((cond
          ((message-news-p)
           ;; News
           "sent-news")
          ;; Mail
          ((message-mail-p)
           nil ; (gnus-sent-messages-folder)
           ))))

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

;; enable topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-ignored-newsgroups ""
      mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-select-method '(nntp "akk4-dmz.akk.uni-karlsruhe.de"))

(setq gnus-secondary-select-methods
            '((nnimap "1und1"
               (nnimap-address "imap.1und1.de")
               (nnimap-server-port 993)
               (nnimap-stream tls)
               (nnir-search-engine imap))
              (nnimap "sns"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap))
              ;; use gwene as RSS/Atom -> nntp gateway
              (nntp "news.gwene.org")))

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
      gnus-move-group-prefix-function (lambda (group-name) "nnimap+1und1:Archiv/"))

;;; bbdb configuration

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

(setq bbdb-north-american-phone-numbers-p nil
      bbdb-phone-style nil)

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
  (define-key gnus-summary-mode-map (kbd "v") 'gnus-summary-show-only-summary)
  (define-key gnus-summary-mode-map (kbd "o") 'gwene-summary-open-full))

(after-load 'gnus-art
  (define-key gnus-article-mode-map (kbd "v") 'gnus-article-show-only-summary)
  (define-key gnus-article-mode-map (kbd "o") 'gwene-article-open-full)
  (define-key gnus-article-mode-map (kbd "C-c C-s") 'gnus-article-save-part))

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

(provide 'cnf-email)

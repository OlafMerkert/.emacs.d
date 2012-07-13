(require 'gnus)

(setq send-mail-function 'sendmail-send-it)

(setq gnus-select-method
      '(nnimap "1und1"
        (nnimap-address "imap.1und1.de")
        (nnimap-server-port 993)
        (nnimap-stream tls)
        (nnir-search-engine imap)))

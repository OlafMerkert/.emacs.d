(setq active-user-mail-addresses '("olaf@m-merkert.de" "olaf.merkert@sns.it")
      user-mail-addresses (append active-user-mail-addresses
                                  '("olaf.merkert@stud.unibas.ch"
                                    "merkol01@mbx.unibas.ch"))
      user-mail-address (first active-user-mail-addresses)
      user-full-name "Olaf Merkert")

(setq tramp-default-method "ssh"
      tramp-default-user "root"
      tramp-default-host "localhost")

(provide 'cnf-personal)

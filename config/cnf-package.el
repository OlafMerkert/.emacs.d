(require 'package)

;; disable use of the gnutls library (broken)?
(if (fboundp 'gnutls-available-p) (fmakunbound 'gnutls-available-p))

;; make sure we check certificates. fall back to trust on first use
(setq tls-checktrust 'ask
      tls-program '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"
                    "gnutls-cli --tofu -p %p %h"))

;; (certificate-check-enabled-p)

(defun certificate-check-enabled-p ()
  (if (condition-case e
          (progn
            (url-retrieve "https://wrong-host.badssl.com/"
                          (lambda (retrieved) t))
            (url-retrieve "https://self-signed.badssl.com/"
                          (lambda (retrieved) t))
            t)
        ('error nil))
      (error "tls misconfigured")
      (url-retrieve "https://badssl.com"
                    (lambda (retrieved) t))))

;; configure repos for elpa
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(provide 'cnf-package)

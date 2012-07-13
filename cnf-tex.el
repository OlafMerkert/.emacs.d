;; Emacs TeX configuration

(setq LaTeX-item-indent 0
      TeX-auto-save t    ; this creates auto dirs
      TeX-parse-self t
      TeX-save-query nil   ; autosave before compiling
      LaTeX-babel-hyphen "--"
      LaTeX-babel-hyphen-after-hyphen t
      LaTeX-command "latex -shell-escape") ; enable shell escapes

(setq-default TeX-master t)

(setq TeX-view-program-list
      '(("Okular" ("okular --unique"
                   (mode-io-correlate " -p %(outpage)") " %o"))
        ("dvips and Okular" ("%(o?)dvips %d -o && okular --unique"
                             (mode-io-correlate " -p %(outpage)") " %f"))
        ("Evince" ("evince"
                   (mode-io-correlate " -i %(outpage)") " %o"))
        ("dvips and Evince" ("%(o?)dvips %d -o && evince"
                             (mode-io-correlate " -i %(outpage)") " %f"))))

(setq TeX-view-program-selection
      (cond
        ((file-exists-p "/usr/bin/okular")
         '(((output-dvi style-pstricks) "dvips and Okular")
           (output-dvi "Okular")
           (output-pdf "Okular")
           (output-html "xdg-open")))
        ((file-exists-p "/usr/bin/evince")
         '(((output-dvi style-pstricks) "dvips and Evince")
           (output-dvi "Evince")
           (output-pdf "Evince")
           (output-html "xdg-open")))))

(add-hook 'TeX-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (visual-line-mode 1)
            (TeX-PDF-mode 1)))

;; TODO $$ zu \( \)
;; TODO () zu \left( \right)
;; TODO \[ \] oder \( \) zu \begin{equation*} \end{equation*}
;; TODO aendere Umgebung \begin{x} \end{x} zu \begin{y} \end{y}


(defun insert-greek-letter (char)
  (interactive "c")
  (let ((expansion (assoc char greek-letter-table)))
    (if expansion
        (insert (cdr expansion))
      (insert char))))

(setq greek-letter-table
      (mapcar (lambda (x)
                (cons (get-byte 0 (car x)) (cdr x)))
              '(("a" . "\\alpha")
                ("b" . "\\beta")
                ("c" . "\\gamma")
                ("d" . "\\delta")
                ("e" . "\\varepsilon")
                ("z" . "\\zeta")
                ("M" . "\\eta")
                ("C" . "\\Gamma")
                ("D" . "\\Delta")
                ("Q" . "\\Theta")
                ("q" . "\\vartheta")
                ("i" . "\\iota")
                ("k" . "\\kappa")
                ("l" . "\\lambda")
                ("m" . "\\mu")
                ("n" . "\\nu")
                ("x" . "\\xi")
                ("L" . "\\Lambda")
                ("X" . "\\Xi")
                ("P" . "\\Pi")
                ("p" . "\\pi")
                ("r" . "\\varrho")
                ("s" . "\\sigma")
                ("t" . "\\tau")
                ("u" . "\\upsilon")
                ("f" . "\\phi")
                ("G" . "\\varphi")
                ("g" . "\\chi")
                ("y" . "\\psi")
                ("w" . "\\omega")
                ("S" . "\\Sigma")
                ("U" . "\\Upsilon")
                ("F" . "\\Phi")
                ("Y" . "\\Psi")
                ("W" . "\\Omega"))))

;; keybindings for tex stuff
(eval-after-load 'tex
    '(define-key TeX-mode-map  (kbd "<f2>") 'insert-greek-letter))

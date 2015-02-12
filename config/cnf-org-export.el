(require 'cnf-org)

;;; setup global exporting options
;; allow use of #+BIND: to configure variables during export
(setq org-export-allow-bind-keywords t)

(after-load 'ox-latex
    ;; make sure shell-escape is turned on (I need it for \gitversioninfo)
    (setq org-latex-pdf-process
     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; enable koma script
  (add-to-list 'org-latex-classes
               '("scrartcl"
                 "\\documentclass[a4paper,11pt]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("scrreprt"
                 "\\documentclass[a4paper,11pt]{scrreprt}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;;(add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings nil) ;; 'minted
  )

;; turn emacs highlighting into html code
(use-package htmlize
    :ensure t
    :defer t)

;; turn off indentation-highlight during htmlize
(defvar htmlize-reenable-modes nil)

(defvar htmlize-disable-modes nil)

(add-to-list 'htmlize-disable-modes 'highlight-indentation-mode)

(defun htmlize-turn-off-modes ()
  (make-local-variable 'htmlize-reenable-modes)
  (dolist (mode htmlize-disable-modes)
    (when (symbol-value mode)
      (push mode htmlize-reenable-modes)
      (funcall mode -1))))

(defun htmlize-turn-on-modes ()
  (dolist (mode htmlize-reenable-modes)
    (funcall mode +1))
  (setf htmlize-reenable-modes nil))

(after-load 'htmlize
  (add-hook 'htmlize-before-hook 'htmlize-turn-off-modes)
  (add-hook 'htmlize-after-hook 'htmlize-turn-on-modes))

;; setup org-ref
;; (use-package helm-bibtex :ensure t)

;; (let ((org-ref-file "~/.emacs.d/addons/org-ref/org-ref.org"))
;;   (when (file-exists-p org-ref-file)
;;     (org-babel-load-file org-ref-file t)))

(setq reftex-default-bibliography '("~/Perfezionamento/topics/topics.bib")
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Perfezionamento/topics/")

(provide 'cnf-org-export)
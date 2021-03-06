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
  (add-to-list 'org-latex-classes
               '("scrbook"
                 "\\documentclass[a4paper,11pt]{scrbook}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; without this, ref and eqref links as provided by org-ref are
  ;; useless. Moreover, we want labels to appear at the top.
  (setq org-latex-prefer-user-labels t
        org-latex-caption-above '(table special-block)))

;; turn emacs highlighting into html code
(use-package htmlize
    :ensure t
    :defer t)

;; turn off indentation-highlight during htmlize
(defvar htmlize-reenable-modes nil)

(defvar htmlize-disable-modes nil)

(add-to-list 'htmlize-disable-modes 'highlight-indentation-mode)
(add-to-list 'htmlize-disable-modes 'slime-highlight-edits-mode)

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
(defun easy-menu-disable-add (function map path item &optional before))
(advice-add 'easy-menu-add-item :around 'easy-menu-disable-add)

(use-package helm-bibtex :ensure t)

;; it is more convenient to tangle the files manually for now
(use-package org-ref
    :ensure t
    :config (progn
              (require 'doi-utils)
              (require 'org-ref-bibtex)
              (setq reftex-default-bibliography '("~/Perfezionamento/topics/topics.bib")
                    org-ref-default-bibliography reftex-default-bibliography
                    org-ref-pdf-directory "~/.cache/bibtex-manager/links/")))

;; highlighting of source blocks in LaTeX with listings
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "olbase"))

(setq org-latex-listings t) ; maybe enable only per file?
(setq org-latex-listings-options '(("basicstyle" "\\ttfamily\\color{almost-black}")
                                   ("keywordstyle" "\\bfseries\\color{black}")
                                   ("identifierstyle" "{}")
                                   ("commentstyle" "\\color{gray}")
                                   ("stringstyle" "{}")
                                   ("breaklines" "true")
                                   ("columns" "fullflexible")
                                   ("frame" "single")))

;; fix export of heading references with spaces
(defun hex-char-p (c)
  (cond ((<= ?0 c ?9) (- c ?0))
        ((<= ?A c ?E) (+ (- c ?A) 10))))

(defun url-decode-string (string)
  (let ((p (position ?% string)))
    (cond ((or (not p) (< (length string) 3)) string)
          ((and (hex-char-p (aref string (+ p 1)))
                (hex-char-p (aref string (+ p 2))))
           (concat (substring string 0 p)
                   (char-to-string (+ (* 16 (hex-char-p (aref string (+ p 1))))
                                      (hex-char-p (aref string (+ p 2)))))
                   (url-decode-string (substring string (+ p 3)))))
          (t (concat (substring string 0 (+ p 1))
                     (url-decode-string (substring string (+ p 1))))))))

(defun org-export-resolve-fuzzy-link--decode (next link info)
  (let ((raw-path (org-element-property :path link)))
    (org-element-put-property link :path (url-decode-string raw-path))
    (funcall next link info)))

(advice-add 'org-export-resolve-fuzzy-link :around 'org-export-resolve-fuzzy-link--decode)

;; downcase names of special blocks in LaTeX
(defpar LaTeX-downcase-special-blocks-list
        '("thm" "rem" "proof" "prop" "lemma" "defi" "cor"))

(defun org-latex-special-block-downcase (special-block contents info)
  (let* ((type (downcase (org-element-property :type special-block)))
         (dc-type (find type LaTeX-downcase-special-blocks-list :test 'string=)))
    (when dc-type
      (org-element-put-property special-block :type type))))

(advice-add 'org-latex-special-block :before 'org-latex-special-block-downcase)

;; HTML export with bootstrap stylesheet
(use-package ox-twbs :ensure t)

(provide 'cnf-org-export)

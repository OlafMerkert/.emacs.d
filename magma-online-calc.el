;;; send regions or buffers to the magma online calculator for
;;; evaluation, and paste the results into buffers

(defun magma-send-request (calculator-code)
 (let ((url-request-method        "POST")
       (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
       (url-request-data          (format "input=%s" (browse-url-encode-url calculator-code))))
   (url-retrieve-synchronously "http://magma.maths.usyd.edu.au/calc/")))


(defun magma-process-input (input)
  ;; (kill-buffer "*magma-response*")
  (with-current-buffer (magma-send-request input)
    ;; (rename-buffer "*magma-response*")
    (search-forward-regexp "<textarea name=\"output\"[^/]*>\\([^<]*\\)")
    (match-string 1)))

(defun magma-process-region (&optional arg)
  (interactive "P")
  (let* ((b (region-beginning))
         (e (region-end))
         (input (buffer-substring b e)))
    (if (= 0 (length input))
        (error "empty region")
        (let ((output (magma-process-input input)))
          (if arg output
              ;; by default, insert the result after region
              (progn
                (goto-char e)
                (insert "\n")
                (insert output)))))))

(defun beginning-of-line-p ()
  (= (point)
     (save-excursion (beginning-of-line) (point))))

(defun magma-process-buffer (&optional arg)
  (interactive "P")
  (let ((output (magma-process-input (buffer-string))))
    (if arg output
        (progn
          (pop-to-buffer "*magma-output*" 'display-buffer-pop-up-window)
          (end-of-buffer)
          (if (beginning-of-line-p) nil (insert "\n"))
          (insert output)))))

;;; TODO check for transient mark mode or stuff.

(provide 'magma-online-calc)

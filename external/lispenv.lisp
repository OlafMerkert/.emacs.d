;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; setup local projects
#+quicklisp
(push (merge-pathnames "Projekte/"
                       (user-homedir-pathname))
      quicklisp:*local-project-directories*)

;;; always load my favourite utilities, but don't panic if it can't be
;;; found. Also change to ol-user directly we are using SBCL
#-clisp(handler-case
    (progn
      (ql:quickload :ol-utils)
      #+sbcl (in-package :ol-user))
  (quicklisp:system-not-found ()
    (abort)))

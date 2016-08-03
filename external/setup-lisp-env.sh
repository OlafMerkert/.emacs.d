#!/bin/sh

cd $HOME

# install quicklisp and slime
curl -O "http://beta.quicklisp.org/quicklisp.lisp"
sbcl --no-userinit --load quicklisp.lisp <<EOF
(quicklisp-quickstart:install :path "~/.quicklisp/")
EOF
sbcl <<EOF
(ql:quickload 'quicklisp-slime-helper)
EOF

# setup the lisp init file
ln -sf .emacs.d/external/lispenv.lisp .sbclrc
ln -sf .emacs.d/external/lispenv.lisp .ccl-init.lisp
ln -sf .emacs.d/external/lispenv.lisp .clisprc.lisp

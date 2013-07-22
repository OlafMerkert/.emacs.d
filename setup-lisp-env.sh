#!/bin/sh

cd $HOME

# install quicklisp and slime
curl -O "http://beta.quicklisp.org/quicklisp.lisp"
sbcl --load quicklisp.lisp <<EOF
(quicklisp-quickstart:install :path "~/.quicklisp/")
(ql:quickload 'quicklisp-slime-helper)
EOF

# setup the lisp init file
ln -s .emacs.d/lispenv.lisp .sbclrc

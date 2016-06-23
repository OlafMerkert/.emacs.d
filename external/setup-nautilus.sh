#!/bin/sh

mkdir -p $HOME/.local/share/nautilus-python/extensions/
ln -sf $HOME/.emacs.d/external/nautilus_org_integration.py $HOME/.local/share/nautilus-python/extensions/org_integration.py

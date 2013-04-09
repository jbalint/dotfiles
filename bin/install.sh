#!/bin/bash

if [ ! -e bin/install.sh ] ; then
    echo "install.sh should be run from 'dotfiles' directory"
    exit 1
fi

for C in configs/* ; do
    ln -s `pwd`/$C $HOME/.${C##*/}
done
ln -s `pwd`/configs/emacs.d/.emacs $HOME/.emacs


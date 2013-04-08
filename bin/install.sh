#!/bin/bash

if [ ! -e bin/install.sh ] ; then
    echo "install.sh should be run from 'dotfiles' directory"
    exit 1
fi

for C in configs/* ; do
    ln -si `pwd`/$C $HOME/.${C##*/}
done

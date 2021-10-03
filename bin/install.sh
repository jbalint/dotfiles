#!/bin/bash

if [ ! -e bin/install.sh ] ; then
    echo "install.sh should be run from 'dotfiles' directory"
    exit 1
fi

mkdir -p ~/tmp/vim
mkdir -p ~/tmp/emacs

for C in configs/* ; do
	DEST=$HOME/.${C##*/}
	if [ ! -e "$DEST" ] ; then
		ln -s `pwd`/$C $HOME/.${C##*/}
	else
		echo "$DEST already exists"
	fi
done


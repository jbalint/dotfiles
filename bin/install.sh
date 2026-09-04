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

mkdir -p $HOME/.config/systemd/user
for C in systemd/* ; do
	DEST=$HOME/.config/systemd/user/${C##*/}
	if [ ! -e "$DEST" ] ; then
		ln -s `pwd`/$C $HOME/.config/systemd/user/${C##*/}
	else
		echo "$DEST already exists"
	fi
done

mkdir -p $HOME/.config/environment.d
for C in environment.d/* ; do
	DEST=$HOME/.config/environment.d/${C##*/}
	if [ ! -e "$DEST" ] ; then
		ln -s `pwd`/$C $HOME/.config/environment.d/${C##*/}
	else
		echo "$DEST already exists"
	fi
done


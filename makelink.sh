#!/bin/sh

linkfiles=".emacs .bash_profile .sbclrc .bash_logout .gnus.el
 .ssh .subversion/config .sawfishrc .screenrc .hgrc .hgignore 
.gitconfig .gitignore .inputrc .bashrc .stumpwmrc .Xresources"
#linkfiles="test test1 test2/test2"
#echo $linkfiles
filehome=~/linux-initial
cd ~
for linkfile in $linkfiles; do
    if ([ -d $linkfile ] || [ -f $linkfile ]) && [ ! -h $linkfile ]; then
	mv $linkfile $linkfile.old
	ln -s $filehome/$linkfile $linkfile
	echo "make a backup and link with "$linkfile.old
    elif [ ! -h $linkfile ]; then
        if [ "$linkfile" == ".emacs" ]; then
            ln -s $filehome/emacs/.emacs .emacs
        else
	    ln -s $filehome/$linkfile $linkfile
        fi
	echo "make a link with", $linkfile
    else
        echo "do nothing with", $linkfile	
    fi
done


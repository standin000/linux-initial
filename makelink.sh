#!/bin/sh

linkfiles="emacs/.emacs .bash_profile .sbclrc .bash_logout .gnus.el
 .ssh .subversion/config .sawfishrc .screenrc .hgrc .hgignore 
.gitconfig .gitignore .inputrc .bashrc .stumpwmrc .Xresources"
#linkfiles="test test1 test2/test2"
#echo $linkfiles
cd ~
for linkfile in $linkfiles; do
    if ([ -d $linkfile ] || [ -f $linkfile ]) && [ ! -h $linkfile ]; then
	mv $linkfile $linkfile.old
	ln -s $OLDPWD/$linkfile $linkfile
	echo "make a backup and link with "$linkfile.old
    elif [ ! -h $linkfile ]; then
	ln -s $OLDPWD/$linkfile $linkfile
	echo "make a link with", $linkfile
    else
        echo "do nothing with", $linkfile	
    fi
done


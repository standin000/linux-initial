#!/bin/bash
# Plato Wu,2011/06/11: TODO use dirname of link file to decide if link is OK instead of
# link file judgement

linkfiles=".emacs .bash_profile .sbclrc .bash_logout .gnus.el .vimrc
.ssh .subversion/config .sawfishrc .screenrc .hgrc .hgignore 
.gitconfig .gitignore .inputrc .bashrc .Xresources .stumpwmrc .minttyrc emacs texmf"
#linkfiles="test test1 test2/test2"
#echo $linkfiles
filehome=~/linux-initial

linkfile(){
    if [ $2 ]; then
        mv $linkfile $linkfile.old
        echo "make a copy with", $linkfile
    fi

    if [ "$1" == ".emacs" ]; then
        ln -s $filehome/emacs/.emacs .emacs
    else
	ln -s $filehome/$linkfile $linkfile
    fi

    echo "make a link with", $linkfile
}

cd ~

for linkfile in $linkfiles; do
    if ([ -d $linkfile ] || [ -f $linkfile ]) && [ ! -h $linkfile ]; then
        linkfile $linkfile t
    elif [ ! -h $linkfile ]; then
        linkfile $linkfile
    else
        source=`readlink $linkfile`
        # Plato Wu,2011/06/16: == =~ require [[]]
        # Plato Wu,2011/06/16: or =~ linux-initial.*
        if [[ ${source} == ${filehome}* ]]; then
            echo "do nothing with", $linkfile	
        else
            linkfile $linkfile t
        fi
    fi
done


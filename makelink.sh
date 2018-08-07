#!/bin/bash

linkfiles=".emacs .bash_profile .sbclrc .bash_logout .gnus.el .vimrc
.ssh .subversion/config .subversion/servers .sawfishrc .screenrc .hgrc .hgignore .tmux.conf
.gitconfig .gitignore .inputrc .bashrc .Xresources .stumpwmrc .minttyrc emacs texmf .config/youtube-dl/config"

filehome=~/linux-initial
cd ~

linkfile(){
    if [ $2 ]; then
        mv $1 $1.old
        echo "make a copy with", $linkfile
    fi

    file=$(basename $1)
    dir=$(dirname $1)

    if [ ! -d $dir ]; then
        echo "not exist $dir "
        mkdir -p $dir
    fi

    ln -s $filehome/$1 $1

    echo "make a link with", $linkfile
}

for linkfile in $linkfiles; do
    # Plato Wu,2014/02/16: it there is a regular file
    if ([ -d $linkfile ] || [ -f $linkfile ]) && [ ! -h $linkfile ]; then
        linkfile $linkfile t
    elif [ ! -h $linkfile ]; then
        linkfile $linkfile
        # Plato Wu,2014/02/16: if there is a link file
    else
        # Plato Wu,2014/02/16: check its origin path
        source=`readlink $linkfile`
        # Plato Wu,2011/06/16: == =~ require [[]]
        # Plato Wu,2011/06/16: or =~ linux-initial.*
        # Plato Wu,2014/02/16: if this link file is generated by makelink.sh
        if [[ ${source} == ${filehome}* ]]; then
            echo "do nothing with", $linkfile	
        else
            linkfile $linkfile t
        fi
    fi
done


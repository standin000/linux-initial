#!/bin/sh
filehome=~/linux-initial/auth
# Plato Wu,2017/05/04: .hgauth no need now, for code.google.com is broken.
authfiles=".authinfo id_rsa id_dsa standin000.el .mailrc"

if [ ! -f "$HOME/gnupg.asc" ]; then
    gpg -o "$HOME/gnupg.asc" -d gnupg.asc.asc
    gpg --import ~/gnupg.asc
fi

for authfile in $authfiles; do
    if ([ "$authfile" = "id_rsa" ] || [ "$authfile" = "id_dsa" ]); then
        mkdir ~/.ssh
        cd ~/.ssh
    elif [ "$authfile" = "standin000.el" ]; then
        mkdir -p ~/.emacs.d/.trello
        cd ~/.emacs.d/.trello
    else
        cd ~
    fi    

    if ([ -d $authfile ] || [ -f $authfile ]); then
        mv $authfile $authfile.old
        echo "make a backup and link with "$authfile.old
    fi
    # Plato Wu,2017/05/04: subkeys.pgp.net is NG
    # for send/receive public key
    # gpg --send-keys --keyserver subkeys.pgp.net gtalk000@gmail.com
    # gpg --keyserver subkeys.pgp.net --recv-keys E87C1128
    # for export, copy  and import private key
    # gpg --export-secret-keys -a -o gnupg.asc
    ##############################
    # gpg --import gnupg.asc
    # gpg -e -a -r "Plato Wu" $authfile will encrypt file with public key
    # Plato Wu,2017/05/04: it need input password one by one
    gpg -o $authfile -d $filehome/$authfile.asc
    if ([ -f $authfile.old ]); then
        diff $authfile.old $authfile
        error=$?
        if [ $error = 0 ]; then
            echo "remove $authfile.old"
            rm $authfile.old
        fi
    fi    
    chmod 600  $authfile
done

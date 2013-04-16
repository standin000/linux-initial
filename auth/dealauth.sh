#!/bin/bash
filehome=~/linux-initial/auth
authfiles=".authinfo .hgauth id_rsa"
for authfile in $authfiles; do
    if [ "$authfile" == "id_rsa" ]; then
        cd ~/.ssh
    else
        cd ~
    fi    

    if ([ -d $authfile ] || [ -f $authfile ]); then
        mv $authfile $authfile.old
        echo "make a backup and link with "$authfile.old
    fi
    # for send/receive public key
    # gpg --send-keys --keyserver subkeys.pgp.net gtalk000@gmail.com
    # gpg --keyserver subkeys.pgp.net --recv-keys E87C1128
    # for export, copy  and import private key
    # gpg --export-secret-keys -a -o private.asc
    # scp -P 80 plato@netawater.blogdns.org://home/plato/backup/private.asc .
    # gpg --import private.asc
    #gpg -e -a -r "Plato Wu" $authfile will encrypt file with public key
    gpg -o $authfile -d $filehome/$authfile.asc
    if ([ -f $authfile.old ]); then
        diff $authfile.old $authfile
        error=$?
        if [ $error == 0 ]; then
            echo "remove $authfile.old"
            rm $authfile.old
        fi
    fi    
        chmod 600  $authfile
done

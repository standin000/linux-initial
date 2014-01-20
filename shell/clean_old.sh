#!/bin/sh
# # update itself for avoiding deleted
# touch cleanold.sh

# # The -delete action atomatically turns on -depth, but -prune does nothing when
# # -depth is in effect.
# # find . -path './.svn'  -prune -o -empty -type d -print -delete

# # delete files's access time older than 90 day, ignore .svn directory.
# # add -print for testing.
# # ls -l -u will show file access time.
# # find -o mean OR: find will execute consequent expression OR alternative expression.
# find . -path './.svn'  -prune -o ! -type d -atime +90 -print -exec rm {} \;

# # delete empty directory.
# # use exec rmdir directly cause find can not search deleted directory afterwards
# # find . -path './.svn'  -prune -o -empty -type d -print -exec rmdir {} \;
# # use -print0 and -0 to avoiding that filenames containing blanks and/or newlines
# # are incorrectly processed by xargs.
# # use -t for testing, use -r for no runing if empty input
# # sometimes it need run severity times for directories which has empty sub
# # directories.
# if [ ${BASH_VERSINFO[5]} = "i386-apple-darwin9.0" ]; then
# # xargs in mac os x does not has -r:--not-run-if-empty and it also not need it.
# # it has not -t:--verbose too, but its own -t has the same effctive
# 	find . -path './.svn'  -prune -o -empty -type d -print0 | xargs -0 -t rmdir
# else
# 	find . -path './.svn'  -prune -o -empty -type d -print0 | xargs -0 -r -t rmdir
# fi



clean_old(){
    # Plato Wu,2009/05/18: -delete can not remove not empty directory.
    # maybe I can treat diretory as file in the future after all using
    # movetotrash.sh
    # Plato Wu,2009/05/31: all trash item must be level 1
    # Plato Wu,2009/09/26: use mtime is more accurate in Cygwin.
    # Plato Wu,2010/08/28: print and confirm before delete
    if [  $2 ]; then
        find $1 -maxdepth 1 ! -type d -mtime +90 -exec ls -l {} \; > trash_items.txt
        # Plato Wu,2009/07/27: use mindepth avoid delete itself
        find $1 -mindepth 1 -maxdepth 1 -type d -mtime +90 -exec ls -l -d {} \; >> trash_items.txt
        if [ -s trash_items.txt ]; then
          cat trash_items.txt  
          doContinue=n
          echo -n "Are you sure clean $1 them all (y/n)"
          read doContinue
          if [ "$doContinue" != "y" ]; then
              echo "Quitting..."
              exit
          fi
          clean_old $1
       fi
    else
        find $1 -maxdepth 1 ! -type d -mtime +90 -print -delete
        # Plato Wu,2009/07/27: use mindepth avoid delete itself
        find $1 -mindepth 1 -maxdepth 1 -type d -mtime +90 -print -exec /bin/rm -rf {} \;
    fi    
}

clean_old ~/Trash/ try

if [  $1 ]; then
    # doContinue=n
    # echo -n "Are you sure clean $1 (y/n)"
    # read doContinue
    # if [ "$doContinue" != "y" ]; then
    #   echo "Quitting..."
    #   exit
    # fi

    clean_old $1 try
else
    if [ "$OSTYPE" = "cygwin" ]; then
	mount_point=/cygdrive
    else
	mount_point=/mnt
    fi

    for dir in $mount_point/*; do
	trash=$dir/Trash/
   # Plato Wu,2009/05/18: Wuala has a hidden Trash directory
	if ([ -d $trash ] && [ "$trash" != "/cygdrive/w/Trash/" ])
	    then
#       cd $trash
	    clean_old $trash try
	fi
    done
fi

rm trash_items.txt

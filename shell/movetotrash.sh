#/bin/sh
# obsolete method
# Plato Wu,2009/05/19: desert batch file version
# REM goto C:\ to avoid woking in the current directory.
# C:
# cd \
# move %~fs1 %~d1%\Trash\
# pause
# mv --target-directory ~/Trash $1 $2

pause_print_error(){
  read -n1 -p "Press any key to continue..."
  exit $1
}
for p in "$@"; do 
# Plato Wu,2009/04/23: use "" to escape space symbol
# Plato Wu,2009/12/30: Only convert DOS path which is "?:*" or ?:*
# Plato Wu,2010/01/06: I don't know why $p contained "" in one system and do not in another system.
# ${1##..} only support ? and * wildcards.
if ([ "$OSTYPE" = "cygwin" ] && [ "${p##*:}" != "$p" ]); then
    # Plato Wu,2009/12/26: cygpath in latest cygwin can not produce correct unix path
    # for path which contains Chinese charcter in Dos, so I use this script
    # Plato Wu,2010/01/10: I don't know why /cygdrive/D's mount point is / and  
    # /cygdrive/d's mount point is D:,but /cygdrive/E and /cygdrive/e's mount point
    # are the same
    path=`echo $p | sed -e 's/D:/d:/g' -e 's/"//g' -e 's/\\\\/\\//g' -e 's/://g' -e 's/^/\/cygdrive\//g'`
#    path=`cygpath "$p"`
else
    path=$p
fi

touch "$path"

error=$?
if [ $error != 0 ]; then
    pause_print_error $error
fi

# Plato Wu,2009/12/26: use awk for compatibility
rootpath=`df "$path" | awk 'FNR == 2 {print $6}'`

error=$?

if [ $error != 0 ]; then
    pause_print_error $error
fi

if ([ "$rootpath" = "/" ] || [ "$rootpath" = "/home" ])
then
  rootpath=~
fi

if [ ! -d $rootpath/Trash/ ]
then
    mkdir $rootpath/Trash/
fi

error=$?
if [ $error != 0 ]; then
    pause_print_error $error
fi

newpath=`basename "$path"`
while [ -e $rootpath/Trash/"$newpath" ]
do
    newpath="1${newpath}"
done

mv -i "$path" $rootpath/Trash/"${newpath}"

error=$?
if [ $error != 0 ]; then
    pause_print_error $error
fi

done
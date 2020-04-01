#!/bin/bash

# mirror_test.sh
# This script tests a list of ubuntu mirrors for speed
#
# I never could find anything in  Ubuntu to automatically test and setup  mirrors.
# There might  be something  already there,  but I  decided to  write this  little
# script for myself.  Hope you enjoy.
#
# @TODO Automatically pull mirror list from https://wiki.ubuntu.com/Archive
# @TODO Write resuts to /etc/apt/sources.list
# @TODO Add command line parsing for above options
#
# @AUTHOR Lance Rushing <lance_rushing@hotmail.com>
# @SINCE 9/1/2006
# This script is covered under the GNU Public License: http://www.gnu.org/licenses/gpl.txt
# source http://ubuntuforums.org/showthread.php?t=251398

FILE="mirror_list.txt"

#!/bin/bash
# ffmirror.sh - http://grulos.blogspot.com
# find fastest mirror (?!?)
# give it a list of hosts and it will sort them
# according to their response time
# no switches (ATM) just ./ffmirror<mirrors.txt
# www.foo.org or http://www.foo.org or
# http://foo.org/page or ...

# trap 'printf "\ncaught signal\nGot only ${#slist[@]}\n" &&
#   printf "%s\n" "${slist[@]}" && exit 1' 2

while read -r line; do
#while read ${MIRRORS}; do    

  # b-a to get time difference.
  # could have used date +%s%N
  # I guess this won't work on a BSD
  a=$(</proc/uptime)
  a=${a%%\ *}
  a=${a/./}

  # wget stuff
  wget --spider -q -O /dev/null $line &&
     b=$(</proc/uptime) &&
       b=${b%%\ *} &&
         b=${b/./}

  printf "%06s%04s%s\n" "$((b-a))0ms ${response[1]} $host"

  # try next if not connected within 2s
  [ $((b-a)) -le 0 ] && continue

  # this is my lazy sort algorithm (c)
  c=$(((b-a)*100))
  until [ "${slist[$c]}" == "" ]; do
    ((c++))
  done
  # slist[$c]="$((b-a))0ms $line"
  slist[$c]="$line\n"
done < "$FILE"

printf "%s\n" "${slist[@]}"
#printf "${slist[@]}"

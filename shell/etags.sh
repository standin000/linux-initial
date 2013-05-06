#!/bin/sh
# Plato Wu,2009/11/18: iname let the match being case insensitive
# -print0 and -0 let find and xargs handle file name which contain space.
# Plato Wu,2010/01/03: * must be escaped for it is used by Shell first.
# Plato Wu,2011/06/28: archlinux provides etags.emacs, don't create link
# Plato Wu,2011/08/31: emacs 23.3.a-2 use etags intead etags.emacs
find . -iname \*.[chs] -print0 -or -iname \*.cpp -print0 | xargs -0 etags -a
# Plato Wu,2013/01/21: count line
find . -iname \*.[chs] -print0 -or -iname \*.cpp -print0 | xargs -0 wc -l

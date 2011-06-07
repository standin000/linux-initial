#!/bin/sh
# Plato Wu,2009/11/18: iname let the match being case insensitive
# -print0 and -0 let find and xargs handle file name which contain space.
# Plato Wu,2010/01/03: * must be escaped for it is used by Shell first.
find . -iname \*.[chs] -print0 | xargs -0 etags

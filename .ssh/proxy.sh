#!/bin/sh
# Plato Wu,2009/07/03: run make first.
# for socket proxy
~/limited_network -S 192.168.1.1:9050 $@
# for http proxy
#~/limited_network -H 192.168.1.1:8118 "$@"
ssh -o ProxyCommand="~/proxy.sh %h %p" "$@"

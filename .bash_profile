# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# Plato Wu,2009/06/07: -e mean file -f mean regular file
# Plato Wu,2010/05/28: urxvt use .bashrc which will source .bash_profile
# if [ -e "${HOME}/.bashrc" ] ; then
#   source "${HOME}/.bashrc"
# fi

# Put your fun stuff here.
# source the users bashrc if it exists


# User specific aliases and functions
# alias rm='mv --target-directory ~/Trash'
 alias rm='~/linux-initial/shell/movetotrash.sh'
 alias del='/bin/rm -i -f'
 alias cp='cp -p -i'
 alias mv='mv -i'
 
# # Don't put continuous duplicate lines in the history.
#  export HISTCONTROL=ignoredups

# Don't put duplicate lines in the whole history.
 export HISTCONTROL=erasedups

 export HISTSIZE=1000 # .bash_history size.
 export HISTFILESIZE=1000 # history command size.

# Default to human readable figures
 alias df='df -h'
 alias du='du -h'

# Misc :)
 alias less='less -r'                          # raw control characters
 alias whence='type -a'                        # where, of a sort
 alias grep='grep --color'                     # show differences in colour

# Some shortcuts for different directory listings
 alias ls='ls -hF --color=auto'                 # classify files in colour #tty
 alias dir='/bin/ls --format=vertical'              # for --color=auto will cause
# problem in emacs, now I donot use --color=tty for ls, beacuse emacs use
#color to solve color problem in shell mode.

 alias vdir='ls --format=long'
 alias ll='ls -l'                              # long list
 alias la='ls -A'                              # all but . and ..
 alias l='ls -CF'                              #
 alias free='free -m'                          # show output in MB
 # Plato Wu,2011/11/21: curl -O 127.0.0.1:8888 will return Remote file name has no length!
 alias curl='curl -C - -O'                     # resume download and use remote file name
 alias aria2c='aria2c -c --file-allocation=none'

 # Plato Wu,2009/06/07: use ssh's X11 forwarding
# if [ -z "$DISPLAY" ]; then
#  export DISPLAY=10.0.2.2:0.0
# fi

# let microwindow be used by multi user.
#export NXDISPLAY=~/.nano-X
# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export EDITOR=vi

# for MIT 6001 course
#export MITSCHEME_6001_DIRECTORY=~/MIT_6001

# Do NOT put any commond in .bashrc for executing them after logining.

# use ssh instead of CVS's default shell rsh, since ssh can use proxy
# another method is create a symblic rsh to ssh at /usr/bin
export CVS_RSH=ssh

# Plato Wu,2009/05/19: proxy for git
if [ -f ~/limited_network ]; then
    export GIT_PROXY_COMMAND=~/.ssh/proxy.sh
fi

# Plato Wu,2009/06/06: pick them from cygwin's .bashrc
# Don't wait for job termination notification
# set -o notify

# Don't use ^D to exit
set -o ignoreeof

# Use case-insensitive filename globbing
shopt -s nocaseglob

# Make bash append rather than overwrite the history on disk
shopt -s histappend

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdspell

# Completion options
# ##################

# These completion tuning parameters change the default behavior of bash_completion:

# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1

# Define to avoid stripping description in --option=description of './configure --help'
COMP_CONFIGURE_HINTS=1

# Define to avoid flattening internal contents of tar files
COMP_TAR_INTERNAL_PATHS=1


# Plato Wu,2009/06/06: It need bash-completion (ArchLinux package name) installed.
# If this shell is interactive, turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
case $- in
  *i*) [[ -e /etc/bash_completion ]] && . /etc/bash_completion ;;
esac

# Ignore some controlling instructions
export HISTIGNORE="[   ]*:&:bg:fg:exit"

# Whenever displaying the prompt, write the previous line to disk
#export PROMPT_COMMAND="history -a"


# Plato Wu,2009/04/22: Do NOT put any commond in .bashrc for executing them after logining.

# # Plato Wu,2009/06/07: If exist .bash_profile, bash does not execute .bash_login
# $STY exist mean shell run from screen

if ([ -e ~/.bash_login ] && [ -z "$STY" ]); then
     source ~/.bash_login
fi

if [ "$EMACS" != '' ]; then
  export PROMPT_COMMAND=
elif [ "$SSH_CONNECTION" != '' ]; then
## Plato Wu,2009/06/22: For putty windows title, it is identified for login by ssh
  export HOSTIP=`echo $SSH_CONNECTION |awk '{print $3}' |awk -F: '{if ($1 == "") print $4; else print $1}'`
    # Plato Wu,2012/04/22: \007 for BEL \033 for ESC
  export PROMPT_COMMAND='echo -ne "\033]0;${USER}@'$HOSTIP':[${HOSTNAME%%.*}]:${PWD/#$HOME/~}"'
else

  export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}"'
fi

# Plato Wu,2010/03/14: For command prompt.
export PS1='\n\[\e[32m\]\t \u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '

# Plato Wu,2009/06/22: Add for cause Cygwin's less and ls show Chinese
export LESSCHARSET=latin1
alias ls='ls -hF --show-control-chars --color=auto'

export PATH=$PATH:/opt/java/jre/bin/:/usr/local/bin/
 
# Plato Wu,2010/01/26: let emacsclient -t automatically start an emacs in deamon mode and
# connect to it if one is not found running
export ALTERNATE_EDITOR=

if ([ "$HOSTNAME" = "myserver" ] || [ "$HOSTNAME" = "myhost" ]); then
 alias emacs='emacsclient -t'
fi
# Plato Wu,2010/02/21: proxy setting for chromium
# Plato Wu,2012/04/11: swtichy sharp instead
# Plato Wu,2012/04/21: chromium 18.0 don't work for pac
#export auto_proxy="https://users.ninthfloor.org/~plato/localautoproxy.pac" or
# Plato Wu,2013/01/02: certificate common name ‘*.ninthfloor.org’ doesn't 
# match requested host name ‘plato.ninth.su, so don't use https for ninth.su
#export auto_proxy="http://plato.ninth.su/localautoproxy.pac"
export GIT_EDITOR=emacs

# Plato Wu,2010/10/24: xterm-256color can't support End key in emacs of myhost, so define it in .emacs
export TERM=xterm-256color

# Plato Wu,2011/07/08: cd z: is OK in cygwin
# Plato Wu,2011/10/31: special for cygwin environment
if [ "$OSTYPE" = "cygwin" ] ; then
    export LANG=zh_CN.GBK
# Plato Wu,2013/06/27: so "cd d" is "cd /cygdrive/d"
    export CDPATH=./:/cygdrive/    
fi

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec startx
fi

# Plato Wu,2013/06/27: spell checking for cd command
shopt -s cdspell

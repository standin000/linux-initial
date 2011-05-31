#~/.bashrc is sourced by non-interactive non-login shells.
# Plato Wu,2010/01/17: It is NG for ssh command
# export PATH=$PATH:/usr/local/bin/

# Check for an interactive session
[ -z "$PS1" ] && return

# Plato Wu,2010/05/28: it is used by urxvt, so source .bash_profile here.
source ~/.bash_profile

# alias ls='ls --color=auto'
# PS1='[\u@\h \W]\$ '


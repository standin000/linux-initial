# /etc/skel/.bash_logout

# This file is sourced when a login shell terminates.

# Clear the screen for security's sake.
# Plato Wu,2009/05/13: cygwin does not has clear commond and it also not need
# do this task
if [ "$OSTYPE" = "linux-gnu" ]; then
    clear
fi

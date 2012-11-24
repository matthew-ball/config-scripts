## FILE: /home/chu/.conf-scripts/bash-dir/profile.sh
## AUTHOR: Matthew Ball (copyleft 2012)
## TIME: Wed 16 May 2012 15:07:17 EST

### COMMENT:
# the point of this `bash_profile' configuration is to start any programs which will be needed at run-time
## TODO: rename all bash-related stuff (removing the word "bash" from their filenames)

emacs --daemon # NOTE: start the emacs daemon
# emacsclient -t # NOTE: start an emacsclient terminal session
# screen # NOTE: start a screen session
# startx & # NOTE: start the X server
# TODO: 1. start screen server (in screen start some stuff?)
#       2. dettach screen session
#       3. startx &
#       4. reconnect screen session

## COMMENT: if ~/.bashrc exists load it
if [ -f ~/.bashrc ]; then
. ~/.bashrc
fi

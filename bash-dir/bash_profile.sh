## FILE: /home/chu/.bash_profile
## AUTHOR: Matthew Ball (copyleft 2012)

## TODO: move this to `~/.conf-scripts/bash-dir/bash_profile'

emacs --daemon # NOTE: start the emacs daemon
startx & # NOTE: start the X server
# screen

## COMMENT: if ~/.bashrc exists  we'll load it
if [ -f ~/.bashrc ]; then
. ~/.bashrc
fi

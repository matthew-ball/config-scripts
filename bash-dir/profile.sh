## FILE: /home/chu/.conf-scripts/bash-dir/profile.sh
## AUTHOR: Matthew Ball (copyleft 2012, 2013)

## COMMENT: if ~/.bashrc exists load it
if [ -f ~/.bashrc ]; then
. ~/.bashrc
fi

## COMMENT: start the X server
startx

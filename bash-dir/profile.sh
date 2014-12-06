## FILE: /home/chu/.conf-scripts/bash-dir/profile.sh
## AUTHOR: Matthew Ball (copyleft 2012, 2013)

# WARNING: I don't know if this works
if [ -z "$CONFIG_SCRIPTS_DIR" ]; then
    # NOTE: ... source ~/.xsessionrc if it exists
    if [ -f "$HOME/.xsessionrc" ]; then
	# . "$HOME/.xsessionrc"
	. "$HOME/.bashrc"
    fi
fi

## IMPORTANT: start the X server
#startx &

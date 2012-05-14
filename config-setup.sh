#!/bin/bash

# IMPORTANT: to give this script execute permissions run the command: 'chmod +x config-setup'
# TODO: doesn't like the "~" variable (i.e. doesn't expand file names), replace with expanded address: "/home/$USER/"

# TODO: task list
# 1. Execute =clone= on the =configuration scripts= project directory from [[http://www.github.com][github]].
# 2. Add the [[http://emacs.naquadah.org][emacs-snapshot]] repository to the package manager (NOTE: maybe Debian only?).
# 3. Run =quicklisp= configuration (available from [[http://www.google.com][here]]).
# 4. Execute =symlink= on the appropriate configuration files (from =~/.conf-scripts/=).
# 5. Disable display manager.
# 6. Restart system.

### COMMENT: application variables
# NOTE: emacs configuration files
EMACS_DIR_SRC="~/.conf-scripts/emacs-dir/"
EMACS_DIR_DEST="~/.emacs.d/"

# NOTE: conkeror configuration files
CONKEROR_DIR_SRC="~/.conf-scripts/conkeror-dir/"
CONKEROR_DIR_DEST="~/.conkerorrc/"

# NOTE: stumpwm configuration files
STUMPWM_DIR_SRC="~/.conf-scripts/stumpwm-dir/"
STUMPWM_DIR_DEST="~/.stumpwm.d/"

STUMPWM_RC_SRC="~/.conf-scripts/stumpwm-dir/init.lisp"
STUMPWM_RC_DEST="~/.stumpwmrc"

# NOTE: bash configuration files
BASH_DIR_SRC="~/.conf-scripts/bash-dir/"
BASH_DIR_DEST="~/.bash.d/"

BASH_RC_SRC="~/.conf-scripts/bash-dir/init.sh"
BASH_RC_DEST="~/.bashrc"

# NOTE: mpd configuration files # TODO: clean up mpd stuff
MPD_RC_SRC="~/.conf-scripts/mpd-dir/mpdrc"
MPD_RC_DEST="~/.mpdconf"

# NOTE: ncmpcpp configuration files # TODO: populate ncmpcpp stuff
NCMPCPP_DIR_SRC="~/.conf-scripts/ncmpcpp-dir/"
NCMPCPP_DIR_DEST="~/.ncmpcpp/"

# NOTE: aptitude configuration files # TODO: populate aptitude stuff
APTITUDE_DIR_SRC="~/.conf-scripts/aptitude-dir/"
APTITUDE_DIR_DEST="~/.aptitude/"

### COMMENT: general variables
VERSION_NUMBER=0.01
GITHUB_SRC="https://github.com/matthew-ball/config-scripts" # NOTE: path to my github repository

### COMMENT: user functions
function sources-list-configuration { # COMMENT: modify sources.list # WARNING: need to be root
    echo "modify sources.list"
}

function disable-display-manager { # COMMENT: disable display manager # WARNING: need to be root
    echo "disable display manager"
}

function clone-git-project { # COMMENT: clone project directory # WARNING: system requires `git' to be installed
    echo "clone git project"
}

function quicklisp-configuration { # COMMENT: quicklisp configuration # WARNING: requires `quicklisp' file to run
    echo "quicklisp configuration"
}

function symlink-configuration { # COMMENT: symlink configuration
    echo "symlink"
    FILE_SRC=$1
    FILE_DEST=$2
    # symlink-file $FILE_SRC $FILE_DEST
}

# NOTE: example: symlink-file bashrc .bashrc
function symlink-file {
    # NOTE: the first argument is the file source
    # NOTE: the second argument is the file destination

    if [ -e $2 ]; then
	echo "- destination file $2 exists"
	return
    elif [ -d $2 ]; then
	echo "- destination folder $2 exists"
	return
    else
	echo "- creating $2"
    fi

    echo "  + ln -s $1 $2" # TODO: run shell command
}

# DEBUG: test function
function echo-source-and-destination {
    echo "- creating symlink $1 from $2 to $3 ..."
    #symlink-file $2 $3
}

function minimal-configuration-setup { # TODO: add others ...
    setup_choice=4
    echo "- minimal setup initiated ..."
    while [ $setup_choice -ne 3 ]; do
	echo "- Welcome to the \"configuration script\" project menu:"
	echo "0. Set =BASH= configuration."
	echo "1. Set =GNU Emacs= configuration."
	echo "2. Set =StumpWM= configuration."
	echo "3. Set =Conkeror= configuration."
	echo "4. Set all of the above."
	echo -e " + make selection: \c "
	read setup_choice
	echo ""
	if [ $setup_choice -eq 0 ]; then # NOTE: =BASH= configuration
	    echo-source-and-destination "bash-dir" $BASH_DIR_SRC $BASH_DIR_DEST
	    echo-source-and-destination "bash-rc" $BASH_RC_SRC $BASH_RC_DEST
	    echo ""
	else
	    if [ $setup_choice -eq 1 ]; then # NOTE: =GNU Emacs= configuration
		echo-source-and-destination "emacs-dir" $EMACS_DIR_SRC $EMACS_DIR_DEST
		echo ""
	    else
		if [ $setup_choice -eq 2 ]; then # NOTE: =StumpWM= configuration
			echo-source-and-destination "stumpwm-dir" $STUMPWM_DIR_SRC $STUMPWM_DIR_DEST
			echo-source-and-destination "stumpwm-rc" $STUMPWM_RC_SRC $STUMPWM_RC_DEST
			echo ""
		else
		    if [ $setup_choice -eq 3 ]; then # NOTE: =Conkeror= configuration
			echo-source-and-destination "conkeror-dir" $CONKEROR_DIR_SRC $CONKEROR_DIR_DEST
			echo ""
		    else
			if [ $setup_choice -eq 4 ]; then # NOTE: All of the above
			    echo-source-and-destination "bash-dir" $BASH_DIR_SRC $BASH_DIR_DEST
			    echo-source-and-destination "bash-rc" $BASH_RC_SRC $BASH_RC_DEST
			    echo ""
			else
			    unknown-input # ERROR: unknown input
			fi
		    fi
		fi
	    fi
	fi
    done
}

### COMMENT: maximal install setup process
function maximal-configuration-setup {
    echo "- maximal configuration setup initiated ..."
    echo "  + downloading config files from $GITHUB_SRC ..."
    echo "  + config files downloaded ..."
    minimal-setup # NOTE: run minimal-setup
    # do-something-else # NOTE: install extra functionality
}

# DEBUG: ...
function unknown-input {
    echo "- unknown input ..."
    exit
}

### COMMENT: main process
function main-program {
    menu_option=5
    while [ $menu_option -ne 4 ]; do
	echo "- configuration script functionality menu:"
	echo "  0. custom config install"
	echo "  1. custom config backup"
	echo "  2. custom config restore"
	echo "  3. update configuration script"
	echo "  4. quit"
	echo -e "  + make selection: \c "
	read menu_option
	echo ""

	if [ $menu_option -eq 0 ]; then # NOTE: configuration script install
	    echo "- configuration script install"
	    echo -e "  + run maximal setup process? [0/1] \c "
	    read confirm_process
	    if [ $confirm_process -eq 0 ]; then
		maximal-setup # NOTE: run maximal configuration setup
	    else
		if [ $confirm_process -eq 1 ]; then
		    minimal-configuration-setup # NOTE: run minimal configuration setup
		else
		    unknown-input
		fi
	    fi
	else
	    if [ $menu_option -eq 1 ]; then # NOTE: configuration script backup
		echo "- configuration script backup"
		echo "  + TODO write backup script ..."
		echo ""
	    else
		if [ $menu_option -eq 2 ]; then # NOTE: configuration script restore
		    echo "- configuration script restore"
		    echo "  + TODO write restore script ..."
		    echo ""
		else
		    if [ $menu_option -eq 3 ]; then # NOTE: update configuration script
			echo "- configuration script update"
		    else
			if [ $menu_option -eq 4 ]; then # NOTE: quit
			    exit
			else
			    unknown-input # ERROR: unknown input
			fi
		    fi
		fi
	    fi
	fi
    done
}

### COMMENT: print a welcome message to the screen
function welcome-message {
    echo "Welcome to the \"configuration scripts\" project:"
    echo " + Version: $VERSION_NUMBER"
    echo " + Authors: chu, syrinx_"
}

### COMMENT: start main program
welcome-message # NOTE: print the welcome message
main-program # NOTE: run main-program

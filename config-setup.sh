#!/bin/bash

# IMPORTANT: to give this script execute permissions run the command: 'chmod +x config-setup'

# TODO: task list
# 1. Execute =clone= on the =configuration scripts= project directory from [[http://www.github.com][github]].
# 2. Add the [[http://emacs.naquadah.org][emacs-snapshot]] repository to the package manager (NOTE: maybe Debian only?).
# 3. Run =quicklisp= configuration (available from [[http://www.google.com][here]]).
# 4. Execute =symlink= on the appropriate configuration files (from =~/.conf-scripts/=).
# 5. Disable display manager.
# 6. Restart system.

### COMMENT: general variables
VERSION_NUMBER=0.01
GITHUB_SRC="https://github.com/matthew-ball/config-scripts" # NOTE: path to github repository

### COMMENT: application variables
# NOTE: emacs configuration files
EMACS_DIR_SRC="/home/$USER/.conf-scripts/emacs-dir/"
EMACS_DIR_DEST="/home/$USER/.emacs.d/"

# NOTE: conkeror configuration files
CONKEROR_DIR_SRC="/home/$USER/.conf-scripts/conkeror-dir/"
CONKEROR_DIR_DEST="/home/$USER/.conkerorrc/"

# NOTE: stumpwm configuration files
STUMPWM_DIR_SRC="/home/$USER/.conf-scripts/stumpwm-dir/"
STUMPWM_DIR_DEST="/home/$USER/.stumpwm.d/"

STUMPWM_RC_SRC="/home/$USER/.conf-scripts/stumpwm-dir/init.lisp"
STUMPWM_RC_DEST="/home/$USER/.stumpwmrc"

# NOTE: bash configuration files
BASH_DIR_SRC="/home/$USER/.conf-scripts/bash-dir/"
BASH_DIR_DEST="/home/$USER/.bash.d/"

BASH_RC_SRC="/home/$USER/.conf-scripts/bash-dir/init.sh"
BASH_RC_DEST="/home/$USER/.bashrc"

BASH_PR_SRC="/home/$USER/.conf-scripts/bash-dir/profile.sh"
BASH_PR_DEST="/home/$USER/.bash_profile"

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
    FILE_DST=$2
    # symlink-file $FILE_SRC $FILE_DST
}

# NOTE: example: symlink-file bashrc .bashrc
function symlink-file {
    # NOTE: the first argument is the file source
    # NOTE: the second argument is the file destination

    FILE_SRC=$1
    FILE_DST=$2

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


### COMMENT: minimal set up
function minimal-configuration-setup { # TODO: add others ...
    setup_choice=5
    echo "- minimal setup initiated ..."
    while [ $setup_choice -ne 4 ]; do
	echo "- Welcome to the \"configuration script\" project menu:"
	echo "0. Set =BASH= configuration."
	echo "1. Set =GNU Emacs= configuration."
	echo "2. Set =StumpWM= configuration."
	echo "3. Set =Conkeror= configuration."
	echo "4. Set all of the above."
	echo "5. Back."
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
			    if [ $setup_choice -eq 5 ]; then # NOTE: Back
				main-program
			    else
				unknown-input # ERROR: unknown input
			    fi
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
    minimal-configuration-setup # NOTE: run minimal configuration setup
    # do-something-else # NOTE: install extra functionality
}

### DEBUG: unknown input
function unknown-input {
    echo "- unknown input ..."
    exit
}

### COMMENT: main process
function main-program { # TODO: reorder processes
    menu_option=5
    while [ $menu_option -ne 4 ]; do
	echo "- configuration script functionality menu:"
	echo "  0. install configuration script"
	echo "  1. backup configuration script"
	echo "  2. restore configuration script"
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
		maximal-configuration-setup # NOTE: run maximal configuration setup
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

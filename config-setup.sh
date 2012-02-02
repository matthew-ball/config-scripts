#!/bin/bash

# to give this script execute permissions run the command: 'chmod +x config-setup'

# TODO: doesn't like "~" variable, replace with something like: "/home/$USER/"

# TODO: finish this script
EMACS_DIR_SRC="~/.conf-scripts/emacs-dir/"
EMACS_DIR_DEST="~/.emacs.d/"

CONKEROR_DIR_SRC="~/.conf-scripts/conkeror-dir/"
CONKEROR_DIR_DEST="~/.conkerorrc/"

STUMPWM_DIR_SRC="~/.conf-scripts/stumpwm-dir/"
STUMPWM_DIR_DEST="~/.stumpwm.d/"

# TODO: move stumpwmrc to stumpwm-dir/init.lisp
STUMPWM_RC_SRC="~/.conf-scripts/stumpwm-dir/init.lisp"
STUMPWM_RC_DEST="~/.stumpwmrc"

BASH_DIR_SRC="~/.conf-scripts/bash-dir/"
BASH_DIR_DEST="~/.bash.d/"

BASH_RC_SRC="~/.conf-scripts/bash-dir/bashrc"
BASH_RC_DEST="~/.bashrc"

# TODO: clean up mpd stuff ...
MPD_RC_SRC="~/.conf-scripts/mpd-dir/mpdrc"
MPD_RC_DEST="~/.mpdconf"

# TODO: populate ncmpcpp stuff ...
NCMPCPP_DIR_SRC="~/.conf-scripts/ncmpcpp-dir/"
NCMPCPP_DIR_DEST="~/.ncmpcpp/"

# TODO: populate aptitude stuff ...
APTITUDE_DIR_SRC="~/.conf-scripts/aptitude-dir/"
APTITUDE_DIR_DEST="~/.aptitude/"

VERSION_NUMBER=0.01
GITHUB_SRC="https://github.com/matthew-ball/config-scripts" # path to my github repository

# i don't know if I will need these two check functions ...
# function check-dir-exists {
#     if [ -d $1]; then
# 	echo "  + $1 exists ..."
#     else
# 	echo "  + creating $1 ..."
#     fi
# }

# function check-file-exists {
#     if [-e $1 ]; then
# 	echo "  + $1 exists ..."
#     else
# 	echo "  + creating $1 ..."
#     fi
# }

# example: symlink-file bashrc .bashrc
function symlink-file {
    # the first argument is the file source
    # the second argument is the file destination
    # TODO: run shell command ...

    if [ -e $2 ]; then
	echo "- destination file $2 exists"
	return
    elif [ -d $2 ]; then
	echo "- destination folder $2 exists"
	return
    else
	echo "- creating $2"
    fi

    echo "  + ln -s $1 $2"
}

function echo-source-and-destination {
    # echo "- $1::source: $2 ..."
    # echo "- $1::destination: $3 ..."
    echo "- creating symlink $1 from $2 to $3 ..."
    symlink-file $2 $3
}

# TODO: add conkeror
function minimal-setup {
    setup_choice=8
    echo "- minimal setup initiated ..."
    while [ $setup_choice -ne 7 ]; do
	echo "- symlink configuration ..."
	echo "  0. symlink everything [a]"
	echo "  1. symlink emacs directory [e]"
	echo "  2. symlink conkeror directory [c]"
	echo "  3. symlink stumpwm directory [s]"
	echo "  4. symlink bash directory [b]"
	echo "  5. symlink mpd directory [m]"
	echo "  6. symlink aptitude directory [p]"
	echo "  7. symlink ncmpcpp directory [n]"
	echo "  8. back [q]"
	echo -e "  + make selection: \c "
	read setup_choice
	echo ""
	if [ $setup_choice -eq 0 ]; then # everything
	    echo-source-and-destination "emacs-dir" $EMACS_DIR_SRC $EMACS_DIR_DEST
	    echo-source-and-destination "conkeror-dir" $CONKEROR_DIR_SRC $CONKEROR_DIR_DEST
	    echo-source-and-destination "stumpwm-dir" $STUMPWM_DIR_SRC $STUMPWM_DIR_DEST
	    echo-source-and-destination "stumpwm-rc" $STUMPWM_RC_SRC $STUMPWM_RC_DEST
	    echo-source-and-destination "bash-dir" $BASH_DIR_SRC $BASH_DIR_DEST
	    echo-source-and-destination "bash-rc" $BASH_RC_SRC $BASH_RC_DEST
	    echo-source-and-destination "mpd-rc" $MPD_RC_SRC $MPD_RC_DEST
	    echo-source-and-destination "aptitude-dir" $APTITUDE_DIR_SRC $APTITUDE_DIR_DEST
	    echo-source-and-destination "ncmpcpp-dir" $NCMPCPP_DIR_SRC $NCMPCPP_DIR_DEST
	    echo ""
	else
	    if [ $setup_choice -eq 1 ]; then # emacs
		echo-source-and-destination "emacs-dir" $EMACS_DIR_SRC $EMACS_DIR_DEST
		echo ""
	    else
		if [ $setup_choice -eq 2 ]; then # conkeror
		    echo-source-and-destination "conkeror-dir" $CONKEROR_DIR_SRC $CONKEROR_DIR_DEST
		    echo ""
		else
		    if [ $setup_choice -eq 3 ]; then # stumpwm
			echo-source-and-destination "stumpwm-dir" $STUMPWM_DIR_SRC $STUMPWM_DIR_DEST
			echo-source-and-destination "stumpwm-rc" $STUMPWM_RC_SRC $STUMPWM_RC_DEST
			echo ""
		    else
			if [ $setup_choice -eq 4 ]; then # bash
			    echo-source-and-destination "bash-dir" $BASH_DIR_SRC $BASH_DIR_DEST
			    echo-source-and-destination "bash-rc" $BASH_RC_SRC $BASH_RC_DEST
			    echo ""
			else
			    if [ $setup_choice -eq 5 ]; then # mpd
				echo-source-and-destination "mpd-rc" $MPD_RC_SRC $MPD_RC_DEST
				echo ""
			    else
				if [ $setup_choice -eq 6 ]; then # aptitude
				    echo-source-and-destination "aptitude-dir" $APTITUDE_DIR_SRC $APTITUDE_DIR_DEST
				    echo ""
				else
				    if [ $setup_choice -eq 7 ]; then # ncmpcpp
					echo-source-and-destination "ncmpcpp-dir" $NCMPCPP_DIR_SRC $NCMPCPP_DIR_DEST
					echo ""
				    else
					if [ $setup_choice -eq 8 ]; then # back
					    main-program
					else
					    unknown-input
					# echo "- unknown input ..."
					# exit
					fi
				    fi
				fi
			    fi
			fi
		    fi
		fi
	    fi
	fi
    done
}

function maximal-setup {
    echo "- maximal setup initiated ..."
    echo "  + downloading config files from $GITHUB_SRC ..."
    echo "  + config files downloaded ..."
    minimal-setup # run minimal-setup ...
}

function unknown-input {
    echo "- unknown input ..."
    exit
}

function main-program {
    menu_option=4
    while [ $menu_option -ne 3 ]; do
	echo "- custom config functionality menu:"
	echo "  0. custom config install"
	echo "  1. custom config backup"
	echo "  2. custom config restore"
	echo "  3. quit"
	echo -e "  + make selection: \c "
	read menu_option
	echo ""

	if [ $menu_option -eq 0 ]; then # custom config install
	    echo "- custom config install"
	    echo -e "  + run maximal setup process? [0/1] \c "
	    read confirm_process
	    if [ $confirm_process -eq 0 ]; then
		maximal-setup # run maximal setup ...
	    else
		if [ $confirm_process -eq 1 ]; then
		    minimal-setup # run minimal setup ...
		else
		    unknown-input
		fi
	    fi
	else
	    if [ $menu_option -eq 1 ]; then # custom config backup
		echo "- custom config backup"
		echo "  + TODO write backup script ..."
		echo ""
	    else
		if [ $menu_option -eq 2 ]; then # custom config restore
		    echo "- custom config restore"
		    echo "  + TODO write restore script ..."
		    echo ""
		else
		    if [ $menu_option -eq 3 ]; then # quit
			exit
		    else
			unknown-input
			# echo "- unknown input ..."
			# exit
		    fi
		fi
	    fi
	fi
    done
}

# start main program ...
echo "==================="
echo "custom config" v$VERSION_NUMBER
echo "==================="
main-program # run main-program

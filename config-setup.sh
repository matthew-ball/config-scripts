#!/bin/bash
# FILE: /home/chu/.conf-scripts/config-setup.sh
# AUTHOR: Matthew Ball (copyleft 2012, 2013)

# IMPORTANT: to give this script execute permissions run the command: 'chmod +x config-setup.sh'

# TODO: task list
# 1. Execute =clone= on the =configuration scripts= project directory from [[http://www.github.com][github]].
# 2. Add the [[http://emacs.naquadah.org][emacs-snapshot]] repository to the package manager (NOTE: maybe Debian only?).
# 3. Run =quicklisp= configuration (available from [[http://www.google.com][here]]).
# 4. Execute =symlink= on the appropriate configuration files (from =~/.conf-scripts/=).
# 5. Disable display manager.
# 6. Restart system.

# COMMENT: general variables
VERSION_NUMBER=0.09
DEST_DIR=".config-scripts"
GITHUB_SRC="git://github.com/matthew-ball/config-scripts" # NOTE: path to github repository
SOURCES_DEST="/etc/apt/sources.list"                      # NOTE: path to sources.list file
QUICKLISP_SRC="http://beta.quicklisp.org/quicklisp.lisp"  # NOTE: path to quicklisp file

# COMMENT: application variables
# NOTE: emacs configuration files
EMACS_SRC_DIR="/home/$USER/$DEST_DIR/emacs-dir/"
EMACS_DEST_DIR="/home/$USER/.emacs.d/"

# NOTE: conkeror configuration files
CONKEROR_SRC_DIR="/home/$USER/$DEST_DIR/conkeror-dir/"
CONKEROR_DEST_DIR="/home/$USER/.conkerorrc/"

# NOTE: stumpwm configuration files
STUMPWM_SRC_DIR="/home/$USER/$DEST_DIR/stumpwm-dir/"
STUMPWM_DEST_DIR="/home/$USER/.stumpwm.d/"

# FIX: ...
STUMPWM_SRC_RC="/home/$USER/$DEST_DIR/stumpwm-dir/init.lisp"
STUMPWM_DEST_RC="/home/$USER/.stumpwmrc"

# NOTE: bash configuration files
BASH_SRC_DIR="/home/$USER/$DEST_DIR/bash-dir/"
BASH_DEST_DIR="/home/$USER/.bash.d/"

# FIX: ...
BASH_SRC_RC="/home/$USER/$DEST_DIR/bash-dir/init.sh"
BASH_DEST_RC="/home/$USER/.bashrc"

# FIX: ...
BASH_SRC_PR="/home/$USER/$DEST_DIR/bash-dir/profile.sh"
BASH_DEST_PR="/home/$USER/.bash_profile"

# COMMENT: user functions
function welcome-message { # NOTE: print a welcome message to the screen
    echo -e "Welcome to \e[01;32m./conf-setup.sh\e[00m \t\t\t\t\t\t\t\t\t\t\t\t\t (version: \e[00;31m$VERSION_NUMBER\e[00m)"
    echo -e "---"
}

function symlink { # NOTE: symlink source file to destination
    echo -e " + [\e[00;31mdebug\e[00m] creating symlink from source \e[00;32m$1\e[00m to destination \e[00;32m$2\e[00m"
    #symlink-file $2 $3
}

function root-setup { # NOTE: commands to be run as root
    sources-list-configuration
    disable-display-manager
    echo -e "- [\e[00;31mdebug\e[00m] execute command \e[00;33msudo shutdown -r now\e[00m"
}

function add-repository {
    echo -e " + [\e[00;31mdebug\e[00m] (\e[01;33m$1\e[00m) add repository \e[00;33m$2\e[00m to \e[00;32m$SOURCES_DEST\e[00m"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33msudo cat # $1 >> $SOURCES_DEST\e[00m"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33msudo cat deb $2 >> $SOURCES_DEST\e[00m"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33msudo cat deb-src $2 >> $SOURCES_DEST\e[00m"
}

function sources-list-configuration { # COMMENT: modify sources.list # WARNING: need to be root
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34msources.list\e[00m with \e[00;31mroot\e[00m"
    add-repository "aarnet"         "http://mirror.aarnet.edu.au/debian/ squeeze main"
    add-repository "emacs-snapshot" "http://emacs.naquadah.org/ stable/"
    add-repository "conkeror"       "http://noone.org/conkeror-nightly-debs squeeze main"
}

function disable-display-manager { # COMMENT: disable display manager # WARNING: need to be root
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34mdisplay manager\e[00m with \e[00;31mroot\e[00m"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33msudo update-rc.d -f gdm remove\e[00m"
}

function clone-git-project { # COMMENT: clone project directory # WARNING: system requires `git' to be installed
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34mgit clone\e[00m of project files"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33mcd ~/$DEST_DIR\e[00m"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33mgit clone $GITHUB_SRC\e[00m"
}

function quicklisp-configuration { # COMMENT: quicklisp configuration # WARNING: requires `quicklisp' file to run
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34mquicklisp\e[00m system"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33mcurl -O $QUICKLISP_SRC\e[00m"
    echo -e " + [\e[00;31mdebug\e[00m] execute command \e[00;33msbcl --load quicklisp.lisp\e[00m"
    echo -e "  * [\e[00;31mdebug\e[00m] execute command \e[00;33mswank\e[00m"
    echo -e "  * [\e[00;31mdebug\e[00m] execute command \e[00;33mstumpwm\e[00m"
}

function check_directory {
    if [ ! -z ${FORCE} ]; then
	if [ ${FORCE} == "true" ]; then
	    if [ -d "$2" ]; then
	    	echo -e " + [\e[00;31mdebug\e[00m] directory \e[00;33m$2\e[00m exists (with \e[01;34mforce\e[00m)"
	    fi
	    echo -e " + [\e[00;31mdebug\e[00m] creating directory \e[00;33m$2\e[00m (with \e[01;34mforce\e[00m)"
	    # TODO: check to make sure SOURCE file is available (if not, run `clone-git-project' function)
	    # symlink $1 $2 # NOTE: $1 is SOURCE, $2 is DESTINATION
	fi
    else
	if [ -d "$2" ]; then
	    echo -e " + [\e[00;31mdebug\e[00m] directory \e[00;33m$2\e[00m exists"
	    return
	else
	    echo -e " + [\e[00;31mdebug\e[00m] creating directory \e[00;33m$2\e[00m"
	    # TODO: check to make sure SOURCE file is available (if not, run `clone-git-project' function)
	    # symlink $1 $2 # NOTE: $1 is SOURCE, $2 is DESTINATION
	fi
    fi
}

function check_file { # NOTE: ...
    if [ ! -z ${FORCE} ]; then
	if [ ${FORCE} == "true" ]; then
	    if [ -e "$2" ]; then
	    	echo -e " + [\e[00;31mdebug\e[00m] file \e[00;33m$2\e[00m exists (with \e[01;34mforce\e[00m)"
                return
	    else
	        echo -e " + [\e[00;31mdebug\e[00m] creating file \e[00;33m$2\e[00m (with \e[01;34mforce\e[00m)"
            fi
	    # TODO: check to make sure SOURCE file is available (if not, run `clone-git-project' function)
	    # symlink $1 $2 # NOTE: $1 is SOURCE, $2 is DESTINATION
	fi
    else
	if [ -e "$2" ]; then
	    echo -e " + [\e[00;31mdebug\e[00m] file \e[00;33m$2\e[00m exists"
	    return
	else
	    echo -e " + [\e[00;31mdebug\e[00m] creating file \e[00;33m$2\e[00m"
	    # TODO: check to make sure SOURCE file is available (if not, run `clone-git-project' function)
	    # symlink $1 $2 # NOTE: $1 is SOURCE, $2 is DESTINATION
	fi
    fi
}

function configure-bash {
    #configure bash
    echo -e " + [\e[00;31mdebug\e[00m] configuring \e[01;34mbash\e[00m"
    # check_directory $BASH_SRC_DIR $BASH_DEST_DIR ${FORCE}
    # check_file      $BASH_SRC_RC  $BASH_DEST_RC  ${FORCE}
    # check_file      $BASH_SRC_PR  $BASH_DEST_PR  ${FORCE}
}

function configure-conkeror {
    # configure conkeror
    echo -e " + [\e[00;31mdebug\e[00m] configuring \e[01;34mconkeror\e[00m"
    # check_directory $CONKEROR_SRC_DIR $CONKEROR_DEST_DIR ${FORCE}
}

function configure-emacs {
    # configure emacs
    echo -e " + [\e[00;31mdebug\e[00m] configuring \e[01;34memacs\e[00m"
    # check_directory $EMACS_SRC_DIR $EMACS_DEST_DIR ${FORCE}
}

function configure-stumpwm {
    # configure stumpwm
    echo -e " + [\e[00;31mdebug\e[00m] configuring \e[01;34mstumpwm\e[00m"
    # check_directory $STUMPWM_SRC_DIR $STUMPWM_DEST_DIR ${FORCE}
    # check_file      $STUMPWM_SRC_RC  $STUMPWM_DEST_RC  ${FORCE}
}

function configure-conf-scripts {
    # COMMENT: configure conf-scripts
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34mconf-scripts\e[00m project"
    configure-bash     # NOTE: setup bash configuration
    configure-conkeror # NOTE: set up conkeror configuration
    configure-emacs    # NOTE: set up emacs configuration
    configure-stumpwm  # NOTE: set up stumpwm configuration
}

# COMMENT: start main program
while getopts ":figqr" opt;
do
    case $opt in
	f ) # NOTE: force
	    FORCE='true'
	    ;;
	i ) # NOTE: initialise
	    INIT='true'
	    ;;
	g ) # NOTE: git clone configuration files
	    clone-git-project
	    ;;
	q ) # NOTE: set up quicklist
	    quicklisp-configuration
	    ;;
	r ) # NOTE: disable display and set sources.list file
	    if [ "$(id -u)" == "0" ]; then
		root-setup # WARNING: need root
		exit 0
	    else
		echo -e "- [\e[00;31mdebug\e[00m] \e[01;30msources-list-configuration\e[00m and \e[01;30mdisable-display-manager\e[00m require system privileges"
		exit 1
	    fi
	    ;;
	\?) echo -e 'USAGE: ./config-setup [-f] [-i] [-g] [-q] [-r]\n
Run the following configuration setup procedures:
1. [f]orce file creation
2. [i]nitial setup process
3. [g]it project clone
4. [q]uicklisp configuration
5. [r]oot configuration'
	    exit 1
    esac
done
shift $(($OPTIND-1))

# COMMENT: running setup
welcome-message # NOTE: print the welcome message
if [ "$(id -u)" == "0" ]; then
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34mall\e[00m with \e[00;31mroot\e[00m"
    root-setup # NOTE: if root, run `sources-list-configuration' and `disable-display-manager' functions
               # TODO: run the script again as normal user
    exit 0
else
    echo -e "- [\e[00;31mdebug\e[00m] configuring \e[01;34mall\e[00m without \e[00;31mroot\e[00m"
    # NOTE: force
    if [ ! -z ${FORCE} ]; then
	if [ ${FORCE} == "true" ]; then
	    echo -e "- [\e[00;31mdebug\e[00m] configuring with \e[01;34mforce\e[00m enabled"
	fi
    fi
    # NOTE: initial
    if [ ! -z ${INIT} ]; then
	if [ ${INIT} == "true" ]; then
	    echo -e "- [\e[00;31mdebug\e[00m] configuring with \e[01;34minit\e[00m enabled"
	fi
    fi

    clone-git-project       # NOTE: clone git project
    quicklisp-configuration # NOTE: setup quicklisp configuration
    configure-conf-scripts  # NOTE: setup conf-scripts configuration
fi

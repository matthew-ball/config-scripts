#!/bin/bash

# variables
version="0.01"

project_branch="laptop"
project_source="https://github.com/matthew-ball/config-scripts.git"
project_target=".config-scripts"

declare -a source_links=("bash-dir/init.sh" "bash-dir/profile.sh" "xinit-dir/session.sh" "xinit-dir/init.sh" "stumpwm-dir/init.lisp" "emacs-dir/")
declare -a destination_links=(".bashrc" ".profile" ".xsessionrc" ".xinitrc" ".stumpwmrc" ".emacs.d/")
declare -a default_packages=("emacs24" "chromium" "htop" "sudo")
declare -a development_packages=("build-essential" "gcc" "gcc-doc" "gdb" "gdb-doc" "sbcl" "sbcl-doc" "ghc" "ghc-doc" "git" "strace" "ltrace")
declare -a packages=(${default_packages[@]} ${development_packages[@]})

# TODO: set user variables
# - full-name
# - email
# - etc

# IMPORTANT: interface
function print_options {
    echo "[info] to use config-setup:"
    echo "-f, --full-install :: full configuration install"
    echo "-i, --install :: install config-setup"
    echo "-l, --linking :: sym-link files"
    echo "-p, --project :: clone project"
    echo "-u, --update :: update config-setup"
    echo "-U, --upgrade-and-update :: upgrade and update config-setup"
    exit
}

function print_welcome_message {
    echo "[info] welcome to config-setup version: $version"
}

# IMPORTANT: install script
function clone_project {
    # TODO: pull config-scripts project from github
    echo "[info] cloning $project_source into $project_target"
    # echo "cd && git clone -b $project_branch $project_source $project_target"
}

function clone_window_manager {
    # TODO: pull stumpwm source code from github
    echo "[info] cloning stumpwm"
    echo "[info] cloning stumpwm-contrib"

    # git clone https://github.com/stumpwm/stumpwm.git
    # TODO: compile stumpwm source code
    echo "[info] compiling stumpwm"
}

# IMPORTANT: output colours
default_colour="\e[39m"
green_colour="\e[32m"
red_colour="\e[31m"

function sym_link {
    # TODO: symlinking
    for ((i = 1; i < ${#source_links[@]}+1; i++));
    do
	echo "[info] linking: ${source_links[$i-1]} -> ${destination_links[$i-1]}"
	# TODO: check if destination exists
	echo "[info] checking if file ${destination_links[$i-1]} exists"
	if [ -f ${destination_links[$i-1]} -o -d ${destination_links[$i-1]} ]; then
	    echo -e "$green_colour[info]$default_colour ... file does exist"
	    # rm -rf ${destination_links[$i-1]}
	else
	    echo -e "$red_colour[info]$default_colour ... file doesn't exist"
	fi
	# rm -rf ~/$link
	# ln -s $PWD/$source_links ~/$link
    done
}

function install_packages {
    # TODO: install default and development packages
    # sudo apt-get install emacs24 chromium htop sudo build-essential gcc gcc-doc gdb gdb-doc sbcl sbcl-doc ghc ghc-doc git strace ltrace
    for package in ${packages[@]}; do
	echo "[info] installing package: $package"
	# sudo apt-get install $package
    done
}

# IMPORTANT: update script
# TODO: fetch and merge project from github

# IMPORTANT: this is top level
cd

if [ -z "$1" ]; then
    print_options
else
    case $1 in
	-f|--full-install	) echo "[info] full-install" ;;
	-i|--install		) echo "[info] install" ;;
	-l|--linking		) sym_link ;;
	-p|--project		) clone_project ;;
	-u|--update		) echo "[info] update" ;;
	-U|--upgrade-and-update	) echo "[info] upgrade-and-update" ;;
	*			) print_options
    esac
fi

#!/bin/bash

# variables
source_links="bash-dir/init.sh stumpwm-dir/init.lisp bash-dir/session.sh bash-dir/profile.sh emacs-dir/"
destination_links=".bashrc .stumpwmrc .xsessionrc .profile .emacs.d/"

# IMPORTANT: install script
# TODO: install state package-list
# this just makes sure we have emacs24, and chromium installed, basically
# TODO: pull config-scripts project from github
git clone https://github.com/matthew-ball/config-scripts.git
# TODO: pull stumpwm source code from github
git clone https://github.com/stumpwm/stumpwm.git
# TODO: compile stumpwm source code
# TODO: symlinking
for link in $destination_links; do
    echo "Linking $link"
    rm -rf ~/$link
    ln -s $PWD/$source_links ~/$link
done

# IMPORTANT: update script
# TODO: fetch and merge project from github

# IMPORTANT: save state
# TODO: export package-list


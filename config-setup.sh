#!/bin/bash

# Config Setup Script
# This script will automatically setup user's configuration files.
# To give this script execute permissions run the command: 'chmod +x config-setup'

# TODO: finish this script
# - are we going to have to hardcode files in here?
# - or, is there a better way

# list of files:
# ./emacs     -> ~/.emacs
# ./emacs-dir -> ~/.emacs.d/
# ./bashrc    -> ~/.bashrc
# etc ...

function symlink-file {
    SRC=$1 # the first argument is the file source
    DST=$2 # the second argument is the file destination
    # symlink file
    # ln -s ./SRC ~/DST
}

# example: symlink-file bashrc .bashrc

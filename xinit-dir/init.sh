#!/bin/sh
## FILE: /home/chu/.conf-scripts/xinit-dir/init.sh
## AUTHOR: Matthew Ball (copyleft 2012)

exec sbcl --eval "(ql:quickload 'stumpwm)" --eval "(stumpwm:stumpwm)" # NOTE: launch stumpwm from quicklisp

#exec stumpwm # NOTE: launch stumpwm session
#exec openbox # NOTE: launch openbox
#exec lxde # NOTE: launch lxde


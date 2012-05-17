## FILE: /home/chu/.conf-scripts/xinit-dir/init.sh
## AUTHOR: Matthew Ball (copyleft 2012)
## TIME: Wed 16 May 2012 15:07:10 EST

# ERROR: I don't think that actually worked
# TODO: compile stumpwm!!
#exec /home/chu/Programming/lisp/common-lisp/stumpwm/stumpwm # DEBUG: testing some thing

exec sbcl --eval "(ql:quickload 'stumpwm)" --eval "(stumpwm:stumpwm)" # NOTE: launch stumpwm with quicklisp

#exec stumpwm # NOTE: launch stumpwm session
#exec openbox # NOTE: launch openbox
#exec lxde # NOTE: launch lxde


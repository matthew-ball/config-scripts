## FILE: /home/chu/.conf-scripts/xinit-dir/init.sh
## AUTHOR: Matthew Ball (copyleft 2012, 2013)

# exec /home/chu/Programming/lisp/common-lisp/stumpwm/stumpwm

exec sbcl --eval "(ql:quickload 'stumpwm)" --eval "(stumpwm:stumpwm)" # NOTE: launch stumpwm with quicklisp

#exec stumpwm # NOTE: launch stumpwm session
#exec openbox # NOTE: launch openbox
#exec lxde # NOTE: launch lxde


## FILE: /home/chu/.conf-scripts/bash-dir/profile.sh
## AUTHOR: Matthew Ball (copyleft 2012, 2013)

## IMPORTANT: if running bash ...
if [ -n "$BASH_VERSION" ]; then
    # NOTE: ... include ~/.bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

## IMPORTANT: path variables
## NOTE: add clojure's lein to $PATH
export PATH=/home/chu/Programming/lisp/clojure/leiningen/:$PATH
export LEIN_JVM_OPTS=

## IMPORTANT: start the X server
startx &

## FILE: /home/chu/.conf-scripts/bash-dir/profile.sh
## AUTHOR: Matthew Ball (copyleft 2012, 2013)

## IMPORTANT: path variables
## NOTE: add clojure's lein to $PATH
export PATH=/home/chu/Programming/lisp/clojure/leiningen/:$PATH
export LEIN_JVM_OPTS=

## IMPORTANT: if ~/.bashrc exists load it
if [ -f ~/.bashrc ]; then
. ~/.bashrc
fi

## IMPORTANT: start the X server
startx

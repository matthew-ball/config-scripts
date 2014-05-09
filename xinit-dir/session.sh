## FILE: /home/chu/config-scripts/xinit-dir/session.sh
## AUTHOR: Matthew Ball (copyleft 2014)

# export environment variables for stumpwm

# --- IMPORTANT: environment settings
export LOCALE=UTF-8
# --- IMPORTANT: default applications
export TERMINAL="x-terminal-emulator"
export BROWSER="x-www-browser"
export FILE_MANAGER="thunar"
export EDITOR="emacsclient -n -c"
export ALTERNATE_EDITOR=""
export VISUAL="emacsclient"
export PACKAGE_MANAGER="aptitude"
export SYSTEM_MONITOR="htop"
export OFFICE_SUITE="libreoffice"
export DOCUMENT_VIEWER="epdfview"
# export AUDIO_PLAYER="ncmpcpp"
export VIDEO_PLAYER="vlc"
# --- IMPORTANT: default user variables
export USER_FULL_NAME="Matthew Ball"
export USER_UNI_ID="u4537508"
export USER_UNI="anu.edu.au"
export PRIMARY_EMAIL="mathew.ball@gmail.com"
# --- IMPORTANT: default path variables
export STUMPWM_SRC_DIR="/home/chu/Public/stumpwm"
export SLIME_DIR="/home/chu/Public/slime"
export QUICKLISP_DIR="/home/chu/quicklisp/dists/quicklisp/software"
export USER_PROJECTS_DIR="/home/chu/Public/"
export CONFIG_SCRIPTS_DIR="/home/chu/config-scripts/"
# NOTE: add clojure's lein to $PATH
#export PATH=/home/chu/Programming/lisp/clojure/leiningen/:$PATH
#export LEIN_JVM_OPTS=

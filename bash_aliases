# ========================
# Custom alias definitions
# ========================

alias ls='ls --color=auto'
    
alias grep='grep --color=auto'

alias search='apt-cache search' # search apt-cache for packages
alias show='apt-cache show' # show package details from apt-cache
alias update='sudo apt-get update' # update apt packages
alias upgrade='sudo apt-get upgrade' # upgrade (any) available apt packages
alias install='sudo apt-get install' # install package via apt-get
alias screenshot='import -window root' # capture screenshot

alias resume='screen -r' # resume screen session

alias connect='nmcli con up id' # connect to network

alias hibernate='sudo pm-hibernate'

# jump to the current emacs directory
alias jem='cd $(emacsclient -e "(with-current-buffer (window-buffer (frame-selected-window)) (expand-file-name default-directory))" | '"sed -E 's/(^\")|(\"$)//g')"

# if [ ! -n "$EMACS" ]; then # add colour for directory listings and grep
#     alias ls='ls --color=auto'

#     alias grep='grep --color=auto'
# fi







## FILE: ~/.conf-scripts/bash-dir/bash_aliases
## AUTHOR: Matthew Ball (copyleft 2012)

# COMMENT: shortcut aliases
# alias lsr='ls -R' # NOTE: recursive directory listing
# alias ls='ls --color=auto' # NOTE: colour ls output
# alias ack="ack-grep -i -u" # TODO: install ack?

alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable --group-directories-first' # NOTE: fancy ls
alias grep='grep --color=auto'
alias clr='clear'

# COMMENT: git command aliases
alias gst="git status "
alias gc="git commit "
alias gca="git commit -a "
alias ga="git add "
alias gco="git checkout "
alias gb="git branch "
alias gm="git merge "

# COMMENT: emacs related aliases
alias emacs_file='emacsclient -n' # NOTE: open file in the current emacs session

alias jem='cd $(emacsclient -e "(with-current-buffer (window-buffer (frame-selected-window)) (expand-file-name default-directory))" | '"sed -E 's/(^\")|(\"$)//g')" # NOTE: jump to the current emacs directory

# COMMENT: system aliases
alias temp='acpi -t' # NOTE: show battery status details
alias screenshot='import -window root' # NOTE: capture screenshot
alias resume='screen -r' # NOTE: resume screen session
alias connect='nmcli con up id' # NOTE: connect to network
alias resource='source ~/.bashrc' # NOTE: re-source bashrc file

# COMMENT: package management aliases
alias search='apt-cache search' # NOTE: search apt-cache for packages
alias show='apt-cache show' # NOTE: show package details from apt-cache
alias update='sudo apt-get update' # NOTE: update apt package
alias upgrade='sudo apt-get upgrade' # NOTE: upgrade available apt packages
alias dist_upgrade='sudo apt-get dist-upgrade' # NOTE: upgrade apt packages
alias install='sudo apt-get install' # NOTE: install package via apt-get
alias remove='sudo apt-get remove' # NOTE: remove apt package
alias purge='sudo apt-get purge' # NOTE: purge apt package
alias hibernate='sudo pm-hibernate' # NOTE: hibernate computer

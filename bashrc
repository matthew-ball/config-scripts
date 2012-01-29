## =================================
## custom ~/.bashrc
## Matthew Ball (copyleft 2008-2011)
## =================================

# if not running interactively, don't do anything
[ -z "$PS1" ] && return

## ========
### exports
## ========
export HISTCONTROL=ignoredups # don't put duplicate lines in the history
export HISTCONTROL=ignoreboth # ... and ignore same sucessive entries

# export TERM=xterm-color # export 8 colours in shell session
export TERM=xterm-256color # export 256 colours in shell session

export ALTERNATE_EDITOR=emacs # set the alternate editor as emacs
export EDITOR=emacsclient # set the main editor as emacsclient (requiring emacs-server)
# export VISUAL=emacsclient # set the visual edit as emacsclient (requiring emacs-server)

# TODO: configure appropriate environment

export BROWSER="conkeror" # export BROWSER as conkeror
# export BROWSER="chromium-browser" # export BROWSER as chromium
export GREP_COLOR="1;33"
export MOZ_DISABLE_PANGO=1

shopt -s checkwinsize # check the window size after each command and, if necessary, update the values of LINES and COLUMNS

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # make less more friendly for non-text input files, see lesspipe

if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then # set variable identifying the chroot you work in (used in the prompt below)
    debian_chroot=$(cat /etc/debian_chroot)
fi

# if [ -n "$EMACS" ]; then # if we're in emacs don't worry about the fancy terminal prompt, else use coloured prompt
#     PS1="\u@\h:\w\$ "
# else
#     force_color_prompt=yes
# fi

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # we have color support; assume it's compliant with Ecma-48 (ISO/IEC-6429)
	# (lack of such support is extremely rare, and such a case would tend to support setf rather than setaf.)
        color_prompt=yes;
    else
        color_prompt=no;
    fi
fi

# case "$TERM" in # set a fancy prompt (non-color, unless we know we "want" color)
# xterm-color|screen|xterm-256color)
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#     ;;
# *)
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#     ;;
# esac

## =============
### shell prompt
## =============
if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

## ===============================
### enable programmable completion
## ===============================
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

## ===============================
### alias definitions
### (sourced from ~/.bash_aliases)
## ===============================
if [ -f ~/.conf-scripts/bash_aliases ]; then
   . ~/.conf-scripts/bash_aliases
fi

## =================
### custom functions
## =================
extract () { # extract from an archive
    if [ -f $1 ] ; then
	case $1 in
            *.tar.bz2)   tar xvjf $1    ;;
            *.tar.gz)    tar xvzf $1    ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1      ;;
            *.tar)       tar xvf $1     ;;
            *.xz)        tar Jxf $1     ;;
            *.tbz2)      tar xvjf $1    ;;
            *.tgz)       tar xvzf $1    ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)           echo "Can not extract '$1'..." ;;
	esac
    else
	echo "'$1' is not a valid file!"
    fi
}

## ================================
### autolaunch ssh-agent on msysgit
## ================================
SSH_ENV="$HOME/.ssh/environment"

# start the ssh-agent
function start_agent {
    echo "Initializing new SSH agent..."
    # spawn ssh-agent
    ssh-agent | sed 's/^echo/#echo/' > "$SSH_ENV"
    echo succeeded
    chmod 600 "$SSH_ENV"
    . "$SSH_ENV" > /dev/null
    ssh-add
}

# test for identities
function test_identities {
    # test whether standard identities have been added to the agent already
    ssh-add -l | grep "The agent has no identities" > /dev/null
    if [ $? -eq 0 ]; then
        ssh-add
        # $SSH_AUTH_SOCK broken so we start a new proper agent
        if [ $? -eq 2 ];then
            start_agent
        fi
    fi
}

# check for running ssh-agent with proper $SSH_AGENT_PID
if [ -n "$SSH_AGENT_PID" ]; then
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
	test_identities
    fi
# if $SSH_AGENT_PID is not properly set, we might be able to load one from
# $SSH_ENV
else
    if [ -f "$SSH_ENV" ]; then
	. "$SSH_ENV" > /dev/null
    fi
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
        test_identities
    else
        start_agent
    fi
fi

### youtube
# mplayer -cookies -cookies-file /foo/bar.txt $(youtube-dl -g --cookies /foo/bar.txt "youtube-share-link")

## ===========
### gnu screen
## ===========
# autostart gnu screen whenever a new terminal session is initiated
# if there's a session available then reattach, else start a new GNU Screen session
# if [ -z "$STY" ]; then
#     exec screen -dR
#     exec screen -rD
# fi

## ============
### emacs shell
## ============
# unset LC_MONETARY
# unset LC_NUMERIC
# unset LC_MESSAGES
# unset LC_COLLATE
# unset LC_CTYPE
# unset LC_TIME

# export LC_CTYPE=en_US.UTF-8
# export PAGER=cat
# export TERM=emacs

# # alias less=cat
# # alias more=cat

# stty rows 10000 columns 80  2>/dev/null
# # echo "Welcome to emacs shell"

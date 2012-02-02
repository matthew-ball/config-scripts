## ~/.conf-scripts/.bashrc
## Matthew Ball (copyleft 2008-2012)

# if not running interactively, don't do anything
[ -z "$PS1" ] && return

### export functionality
export HISTCONTROL=ignoredups # don't put duplicate lines in the history
export HISTCONTROL=ignoreboth # ... and ignore same sucessive entries
# export TERM=xterm-color # export 8 colours in shell session
export TERM=xterm-256color # export 256 colours in shell session

export ALTERNATE_EDITOR="" # set the alternate editor as emacs (automatically start an emacs in daemon mode and connect to it if one is not found running)
export EDITOR='emacsclient -n' # set the main editor as emacsclient (requiring emacs-server)
# export VISUAL=emacsclient # set the visual edit as emacsclient (requiring emacs-server)

# export BROWSER="conkeror" # export BROWSER as conkeror
# export BROWSER="chromium-browser" # export BROWSER as chromium
export BROWSER="x-www-browser" # export BROWSER as x-www-browser ... NOTE: requires debian (???)
export GREP_COLOR="1;33"
export MOZ_DISABLE_PANGO=1

shopt -s checkwinsize # check the window size after each command and, if necessary, update the values of LINES and COLUMNS

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # make less more friendly for non-text input files, see lesspipe

if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then # set variable identifying the chroot you work in (used in the prompt below)
    debian_chroot=$(cat /etc/debian_chroot)
fi

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

### enable programmable completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

### bash aliases (sourced from ~/.conf-scripts/bash-dir/bash_aliases)
if [ -f ~/.conf-scripts/bash-dir/bash_aliases ]; then
    . ~/.conf-scripts/bash-dir/bash_aliases
fi

### bash functions (sourced from ~/.conf-scripts/bash-dir/bash_functions) 
if [ -f ~/.conf-scripts/bash-dir/bash_functions ]; then
    . ~/.conf-scripts/bash-dir/bash_functions
fi

# prompt colours
GREY="\[\033[00m\]" # code for the colour "grey"
RED="\[\033[01;31m\]" # code for the colour "red"
GREEN="\[\033[01;32m\]" # code for the colour "green"
YELLOW="\[\033[01;33m\]" # code for the colour "yellow"
BLUE="\[\033[01;34m\]" # code for the colour "blue"

if [ "$color_prompt" = yes ]; then
    # PS1="\$ " # plain prompt
    # PS1="${debian_chroot:+($debian_chroot)}\u@\h:\w\$ " # prompt with no colour
    # PS1="${debian_chroot:+($debian_chroot)}$GREEN\u$GREY@$GREEN\h$GREY:$BLUE\w$GREY\$ " # prompt with colour (without sv status)

    PS1="${debian_chroot:+($debian_chroot)}$GREEN\u$GREY@$GREEN\h$GREY:$BLUE\w$YELLOW\$(parse_git_branch)$GREY\$ " # requires parse_git_branch function
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

SSH_ENV="$HOME/.ssh/environment" # autolaunch ssh-agent on msysgit

if [ -n "$SSH_AGENT_PID" ]; then # check for running ssh-agent with proper $SSH_AGENT_PID
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
	test_identities
    fi
else # if $SSH_AGENT_PID is not properly set, we might be able to load one from $SSH_ENV
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

### gnu screen
# autostart gnu screen whenever a new terminal session is initiated
# if there's a session available then reattach, else start a new GNU Screen session
# if [ -z "$STY" ]; then
#     exec screen -dR
#     exec screen -rD
# fi

### shell prompt
# case "$TERM" in # set a fancy prompt (non-color, unless we know we "want" color)
# xterm-color|screen|xterm-256color)
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#     ;;
# *)
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#     ;;
# esac

# if [ -n "$EMACS" ]; then # if we're in emacs don't worry about the fancy terminal prompt, else use coloured prompt
#     PS1="\u@\h:\w\$ "
# else
#     force_color_prompt=yes
# fi

### emacs shell
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

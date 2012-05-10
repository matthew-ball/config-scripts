## FILE: ~/.conf-scripts/.bashrc
## AUTHOR: Matthew Ball (copyleft 2008-2012)

# NOTE: if not running interactively, don't do anything
[ -z "$PS1" ] && return

### COMMENT: export functionality
export HISTCONTROL=ignoredups # NOTE: don't put duplicate lines in the history
export HISTCONTROL=ignoreboth # NOTE: ... and ignore same sucessive entries

export TERM=xterm-color # NOTE: export 8 colours in shell session
# export TERM=xterm-256color # NOTE: export 256 colours in shell session

export ALTERNATE_EDITOR="" # NOTE: set the alternate editor as emacs (automatically start an emacs in daemon mode and connect to it if one is not found running)
export EDITOR='emacsclient -n' # NOTE: set the main editor as emacsclient (IMPORTANT: requiring emacs-server)
# export VISUAL=emacsclient # NOTE: set the visual edit as emacsclient (IMPORTANT: requiring emacs-server)

# export BROWSER="conkeror" # NOTE: export BROWSER as conkeror
# export BROWSER="chromium-browser" # NOTE: export BROWSER as chromium
export BROWSER="x-www-browser" # NOTE: export BROWSER as x-www-browser ... (IMPORTANT: requires debian ???)
export GREP_COLOR="1;33"
export MOZ_DISABLE_PANGO=1

shopt -s checkwinsize # NOTE: check the window size after each command and, if necessary, update the values of LINES and COLUMNS

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # NOTE: make less more friendly for non-text input files, see lesspipe

if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then # NOTE: set variable identifying the chroot you work in (used in the prompt below)
    debian_chroot=$(cat /etc/debian_chroot)
fi

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then # NOTE: we have color support; assume it's compliant with Ecma-48 (ISO/IEC-6429)
        color_prompt=yes;
    else
        color_prompt=no;
    fi
fi

### COMMENT: enable programmable completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

### COMMENT: bash aliases (sourced from ~/.conf-scripts/bash-dir/bash_aliases)
if [ -f ~/.conf-scripts/bash-dir/bash_aliases.sh ]; then
    . ~/.conf-scripts/bash-dir/bash_aliases.sh
fi

### COMMENT: bash functions (sourced from ~/.conf-scripts/bash-dir/bash_functions) 
if [ -f ~/.conf-scripts/bash-dir/bash_functions.sh ]; then
    . ~/.conf-scripts/bash-dir/bash_functions.sh
fi

# NOTE: prompt colours
GREY="\[\033[00m\]" # NOTE: code for the colour "grey"
RED="\[\033[01;31m\]" # NOTE: code for the colour "red"
GREEN="\[\033[01;32m\]" # NOTE: code for the colour "green"
YELLOW="\[\033[01;33m\]" # NOTE: code for the colour "yellow"
BLUE="\[\033[01;34m\]" # NOTE: code for the colour "blue"

if [ "$color_prompt" = yes ]; then
    # PS1="\$ " # plain prompt
    # PS1="${debian_chroot:+($debian_chroot)}\u@\h:\w\$ " # NOTE: prompt with no colour
    # PS1="${debian_chroot:+($debian_chroot)}$GREEN\u$GREY@$GREEN\h$GREY:$BLUE\w$GREY\$ " # NOTE: prompt with colour (without vc status)

    PS1="${debian_chroot:+($debian_chroot)}$RED\u$GREY@$GREEN\h$GREY:$BLUE\w$YELLOW\$(parse_git_branch)$GREY\$ " # NOTE: requires parse_git_branch function
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

SSH_ENV="$HOME/.ssh/environment" # NOTE: autolaunch ssh-agent on msysgit

if [ -n "$SSH_AGENT_PID" ]; then # NOTE: check for running ssh-agent with proper $SSH_AGENT_PID
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
	test_identities
    fi
else # NOTE: if $SSH_AGENT_PID is not properly set, we might be able to load one from $SSH_ENV
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

### COMMENT: youtube
# mplayer -cookies -cookies-file /foo/bar.txt $(youtube-dl -g --cookies /foo/bar.txt "youtube-share-link")

### COMMENT: gnu screen
# NOTE: autostart gnu screen whenever a new terminal session is initiated (if there's a session available then reattach, else start a new GNU Screen session)
# if [ -z "$STY" ]; then
#     exec screen -dR
#     exec screen -rD
# fi

### COMMENT: shell prompt
# case "$TERM" in # NOTE: set a fancy prompt (non-color, unless we know we "want" color)
# xterm-color|screen|xterm-256color)
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#     ;;
# *)
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#     ;;
# esac

# if [ -n "$EMACS" ]; then # NOTE: if we're in emacs don't worry about the fancy terminal prompt, else use coloured prompt
#     PS1="\u@\h:\w\$ "
# else
#     force_color_prompt=yes
# fi

### COMMENT: emacs shell
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

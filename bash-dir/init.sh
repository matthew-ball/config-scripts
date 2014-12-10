## FILE: /home/chu/.conf-scripts/bash-dir/init.sh
## AUTHOR: Matthew Ball (copyleft 2012, 2013)

# NOTE: if not running interactively, don't do anything
#[ -z "$PS1" ] && return

# TODO: this file has to inherit the environment variables set in profile.sh

### COMMENT: export functionality
export HISTCONTROL=ignoredups # NOTE: don't put duplicate lines in the history
export HISTCONTROL=ignoreboth # NOTE: ... and ignore same sucessive entries
export LOCALE=UTF-8
export GREP_COLOR="1;33"
export MOZ_DISABLE_PANGO=1
export TERM=xterm-color # NOTE: export 8 colours in shell session
# export TERM=xterm-256color # NOTE: export 256 colours in shell session

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

# TODO: $CONFIG_SCRIPTS_DIR
### COMMENT: bash aliases (sourced from ~/.conf-scripts/bash-dir/aliases.sh)
if [ -f ~/.config-scripts/bash-dir/aliases.sh ]; then
    . ~/.config-scripts/bash-dir/aliases.sh
fi

### COMMENT: bash functions (sourced from ~/.conf-scripts/bash-dir/functions.sh)
if [ -f ~/.config-scripts/bash-dir/functions.sh ]; then
    . ~/.config-scripts/bash-dir/functions.sh
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
    # ---
    # NOTE: requires parse_git_branch function
    PS1="${debian_chroot:+($debian_chroot)}$RED\u$GREY@$GREEN\h$GREY:$BLUE\w$YELLOW\$(parse_git_branch)$GREY\$ "
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# NOTE: set up ssh-agent
SSH_ENV="$HOME/.ssh/environment"
 
function start_agent {
    echo "Initializing new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    # echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}
 
# NOTE: source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    # NOTE: ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

### COMMENT: youtube
# mplayer -cookies -cookies-file /foo/bar.txt $(youtube-dl -g --cookies /foo/bar.txt "youtube-share-link")

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

# login shell
# if shopt -q login_shell ; then
#     : # NOTE: this is executed only when it is a login shell
#     exec emacs --daemon
# fi

# NOTE: change to home
# cd $HOME

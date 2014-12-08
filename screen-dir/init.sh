
autodetach on # detach on hangup
defmonitor on # turn monitoring on by default
altscreen on # fix residual text issues
defbce on # ...
defutf8 on # ...
nethack on # enable nethack-inspired error messages

## NOTE: setup user interface
# term screen-color # set 8 colour shell session
# term screen-256color # set 256 colour shell session
term xterm-color # set 8 colour shell session
# term xterm-256color # set 256 colour shell session

defscrollback 20000 # define a bigger scrollback, default is 100 lines
vbell off # turn off the visual bell
startup_message off # turn off the startup message
activity "[%c] Activity in window %n (%t)." # custom activity message

# termcapinfo xterm* ti@:te@ # enable mousewheel scrolling in screen (WARNING: this is somewhat buggy)

hardstatus alwaysignore # status bar off by default
# hardstatus alwayslastline # status bar at the bottom
caption splitonly "%n %t" # only display caption during split

hardstatus string "%-Lw%{= BW}%50>%n %t%{-}%+Lw%< " # window list (plain)
# hardstatus string "%-Lw%{= BW}%50>%n %t%{-}%+Lw%< %=%M %d (%D),%C%A " # window list (clock on right)
# hardstatus string "%-Lw%{= BW}%50>%n %t%{-}%+Lw%< %=[%l] %M %d (%D),%C%A " # window list (CPU load and clock on right)

## NOTE: custom key bindings
escape ^Zz # make the escape key C-z so it plays nice with emacs

bind E screen -t emacs 0 emacsclient -nw -c # launch an emacsclient session (C-z E)
bind T screen -t htop 0 htop -u $USER # launch a new htop session for my user (C-z T)

bind u hardstatus alwayslastline # show the status bar (C-z u)
bind U hardstatus alwaysignore # hide the status bar (C-z U)

bind A title # rename session (C-z A)
bind R resize # resize window session (C-z R)
bind m monitor # monitor a session (C-z m)

## NOTE: startup
screen -t emacs 0 emacsclient -nw -c # start an emacsclient session by default

select 0 # return control to the first window


;; FILE: /home/chu/.conf-scripts/stumpwm-dir/init.lisp
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:06:40 EST

;;; COMMENT: lisp implementation
;debian=sbcl

;;; COMMENT: initial config
(in-package :stumpwm) ;; NOTE: declare the package scope

(setf *default-package* :stumpwm ;; NOTE: set default package to be stumpwm
      *startup-message* "Welcome to stumpwm, happy hacking!"
      ;; *startup-message* nil ;; NOTE: suppress the startup message
      ;; *debug-level* 10 ;; NOTE: turn on stumpwm debugging (WARNING: creates massive text dumps)
      *shell-program* (getenv "SHELL") ;; NOTE: set the default shell
      *mouse-focus-policy* :sloppy) ;; NOTE: set the mouse policy so focus follows mouse (alternatives are: :click, :ignore, :sloppy)

(redirect-all-output (data-dir-file "debug-output" "txt")) ;; NOTE: send debug information to ~/.stumpwm.d/debug-output.txt

(set-prefix-key (kbd "s-z")) ;; NOTE: set the stumpwm prefix key to super+z (default is C-t)

;; COMMENT: old
;; (defparameter *foreground-colour* "darkseagreen4" "Set the foreground colour.") ;; NOTE: old zenburn foreground colour
;; (defparameter *background-colour* "grey25" "Set the background colour.") ;; NOTE: old zenburn background colour
;; (defparameter *border-colour* "grey25" "Set the border colour.")
;; (defparameter *focus-colour* "darkseagreen1" "Set the focus colour.")
;; (defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")

;; COMMENT: new
;; (defparameter *foreground-colour* "grey" "Set the foreground colour.") ;; NOTE: new foreground colour
;; (defparameter *background-colour* "grey25" "Set the background colour.") ;; NOTE: new background colour
;; (defparameter *border-colour* "black" "Set the border colour.")
;; (defparameter *focus-colour* "grey" "Set the focus colour.")
;; (defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")

;; (defparameter *groups* '("default" "internet" "misc") "Default group names.") ;; TODO: should actually do something with this ...

;;; COMMENT: default applications
(defvar *browser* "" "Set the default web browser.") ;; TODO: this should be "x-www-browser"
(defvar *terminal* "x-terminal-emulator" "Set the default terminal emulator.") ;; NOTE: this line (and the previous) is redundant

;; (defvar *editor* "emacs" "Set the default editor.") ;; NOTE: older, using the default emacs
(defvar *editor* "emacsclient -n -c" "Set the default editor.") ;; NOTE: newer, using the `emacs --daemon' service

(defvar *file-manager* "pcmanfm" "Set the default file manager.")
(defvar *package-manager* "aptitude" "Set the default package manager.")
(defvar *system-monitor* "htop" "Set the default system monitor.") ;; NOTE: can use `proced'
(defvar *document-viewer* "evince" "Set the default document reader.")
(defvar *audio-player* "ncmpcpp" "Set the default audio player.") ;; NOTE: can use `emms'
(defvar *video-player* "vlc" "Set the default video player.")

;; (defvar *system-monitor* "proced" "Set the default system monitor.") ;; TODO: implement this in emacs
;; (defvar *audio-player* "emms" "Set the default audio player.") ;; TODO: implement this in emacs
;; (defvar *mail-client* "gnus" "Set the default mail application.") ;; TODO: implement this in emacs
;; (defvar *irc-client* "erc" "Set the default IRC client.") ;; TODO: implement this in emacs

;; (defvar *office-suite* "openoffice.org" "Set the default office suite.") ;; TODO: set this up

;; TODO: find a better way to set default browser
;; (setf *browser* "x-www-browser") ;; NOTE: this does not work (but should!)
(setf *browser* "conkeror") ;; NOTE: set the default browser as conkeror
;; (setf *browser* "chromium-browser") ;; NOTE: set the default browser as chromium

;;; COMMENT: general functions
(defun cat (&rest strings) ;; TODO: rename to 'concat' (for consistency with emacs)
  "Concatenates strings, like the Unix command 'cat'. A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

(defun hostname (&rest junk)
  "Returns a string representing the hostname."
  (first (split-string (machine-instance) ".")))

;; (defun launch-mpd (&rest junk)
;;   "Start music player daemon server."
;;   (run-shell-command "mpd"))

;;; COMMENT: slime and swank
;; NOTE: requires `quicklisp'
;; ERROR: this is not very wise to run with an sbcl session underneath (??!?!?!?!)
;; (load "/usr/share/common-lisp/source/slime/swank-loader.lisp")
;; (load "/home/chu/quicklisp/dists/quicklisp/software/slime-20120208-cvs/swank-loader.lisp") ;; ERROR: should not be hardcoded
;; (require 'swank) ;; NOTE: am I dreaming, or does this work ...
;; (swank:create-server)
;; (swank-loader:init)

;; (defvar *swank-p* nil)

;; (defcommand swank () () ;; NOTE: command to start a swank server
;;   "Starts a swank server on port 4006 and notifies the user."
;;   (setf *top-level-error-action* :break)
;;   (if *swank-p*
;;       (message "Swank server already running.")
;;     (progn
;;       (swank:create-server :port 4006
;;                            :style swank:*communication-style*
;;                            :dont-close t)
;;       (setf *swank-p* t)
;;       (message "Starting swank server. M-x slime-connect RET RET, then (in-package :stumpwm)."))))

;; (swank) ;; NOTE: start the swank server

;;; COMMENT: monitoring scripts
(set-contrib-dir "/usr/share/common-lisp/source/stumpwm/contrib") ;; set contrib directory

(mapcar #'load-module '(;; "cpu" ;; load selected modules ;; ERROR: doesn't appear to work
                        ;; "mem"
                        ;; "battery-portable"
			;; "amixer"
			;; "mpd"
			;; "notifications"
			;; "productivity"
                        ;; "net"
                        ;; "disk"
                        ;; "wifi"
			;; "battery"
			))

;;; COMMENT: window appearance
(setf *normal-border-width* 0 ;; the width in pixels given to the borders of regular windows
      *maxsize-border-width* 0 ;; the width in pixels given to the borders of windows with maxsize or ratio hints
      *transient-border-width* 0 ;; the width in pixels given to the borders of transient or pop-up windows
      *window-border-style* :thin) ;; set the window border to thin (alternatives are: :thick :thin :tight :none)

(set-normal-gravity :top)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

(set-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1") ;; NOTE: set the font for the message and input bars, and the mode line (emacs font)

;; ;;; COMMENT: message and input box
;; (set-bg-color *background-colour*)
;; (set-fg-color *foreground-colour*)
;; (set-border-color *border-colour*)
;; (set-focus-color *focus-colour*)
;; (set-unfocus-color *unfocus-colour*)
(set-msg-border-width 0)

(setf *message-window-gravity* :top-right ;; NOTE: set the message-box to the top right
      *input-window-gravity* :top-right ;; NOTE: set the input-box to the top right
      ;; *window-name-source* :title ;; NOTE: windows get their name from their title property
      *timeout-wait* 5) ;; NOTE: how long a message will appear for (in seconds)

;;; COMMENT: mode line
(set-frame-outline-width 0)

(setf ;; *mode-line-background-color* *background-colour*
      ;; *mode-line-foreground-color* *foreground-colour*
      ;; *mode-line-border-color* *border-colour*
      *mode-line-border-width* 0 ;; NOTE: set thickness of the mode line border
      *mode-line-pad-x* 0 ;; NOTE: set the padding between the mode line text and the sides
      *mode-line-pad-y* 0 ;; NOTE: set the padding between the mode line text and the top/bottom
      *mode-line-position* :top
      *mode-line-screen-position* :top
      *mode-line-frame-position* :top
      *window-format* "%n%s %20t"
      *window-info-format* "[%i] - (%t)"
      *group-format* "%n:%t"
      *mode-line-timeout* 1) ;; NOTE: update every second (if nothing else has triggered it already)

(setf *screen-mode-line-format*
      (list 
       ;; '(:eval (cat " [(^B" (run-shell-command "date '+%d %b, %R'|tr -d [:cntrl:] " t) "^b)] ")) ;; NOTE: display time
       ;; "[(^B%w^b)]" ;; NOTE: display current group/frames
       ;; "[(^B%g^bA)] [(^B%W^b)] " ;; NOTE: display groups/current frame
       ;; "[(%c) (%M)] " ;; NOTE: display cpu and memory
       ;; " [(%B) (%I)]" ;; NOTE: display battery and wireless
       ;; "[^B%m^b]" ;; NOTE: display mpd
       ))

;; (when (not (head-mode-line (current-head))) ;; NOTE: turn on the `mode-line'
;;   (toggle-mode-line (current-screen) (current-head)))

;;; COMMENT: key bindings
(defmacro defkey-top (key cmd)
  `(define-key *top-map* (kbd, key), cmd))

(defmacro defkeys-top (&rest keys)
  (let ((ks (mapcar #'(lambda (k) (cons 'defkey-top k)) keys)))
    `(progn ,@ks)))

(defmacro defkey-root (key cmd)
  `(define-key *root-map* (kbd, key), cmd))

(defmacro defkeys-root (&rest keys)
  (let ((ks (mapcar #'(lambda (k) (cons 'defkey-root k)) keys)))
    `(progn ,@ks)))

(defkeys-root ;; NOTE: define root-map keys
  ;; ("s-g" "google") ;; NOTE: quick search google
  ;; ("s-w" "wikipedia") ;; NOTE: quick search wikipedia
  ("s-A" "title") ;; NOTE: modify current frame's title
  ("s-R" "loadrc") ;; NOTE: reload run-time configuartion file
  ("M-b" "show-battery") ;; NOTE: show battery status
  ("M-u" "show-uptime") ;; NOTE: show uptime status
  ("M-h" "show-hostname") ;; NOTE: show hostname
  ("M-i" "show-window-properties")) ;; show current window's properties

(defkeys-top ;; NOTE: define top-map keys (these don't require prefix key)
  ("s-E" '*emacs-map*)
  ;; ("s-M" '*mpd-map*)
  ("s-S" '*sudo-map*)
  ;; ("s-V" '*volume-map*)
  ;; ("s-N" '*notifications-map*) ;; TODO: setup
  ("s-a" "run-audio-player") ;; open (or switch to an existing instance of) *audio-player*
  ("s-b" "run-browser") ;; open (or switch to an existing instance of) *browser*
  ("s-e" "run-editor") ;; open (or switch to an existing instance of) *editor*
  ("s-f" "run-file-manager") ;; open (or switch to an existing instance of) *file-manager*
  ("s-h" "run-system-monitor") ;; open (or switch to an existing instance of) *system-monitor*
  ;; ("s-i" "run-irc") ;; open (or switch to an existing instance of) *irc-client*
  ;; ("s-m" "run-mail") ;; open (or switch to an existing instance of) *mail-client*
  ("s-p" "run-package-manager") ;; open (or switch to an existing insance of) *package-manager*
  ("s-t" "run-terminal") ;; open (or switch to an existing instance of) *terminal*
  ("s-v" "run-video-player")) ;; open (or switch to an existing instance of) *video-player*

(defvar *sudo-map* nil "Super-user specific key-bindings.")
;; (defvar *emacs-map* nil "Emacs specific key-bindings.")
;; (defvar *volume-map* nil "Control volume key-bindings.")

(fill-keymap *sudo-map*
             (kbd "r") "reboot"
	     (kbd "s") "shutdown"
             (kbd "h") "hibernate")

;; (fill-keymap *emacs-map*
;;              (kbd "a") "emacs-agenda"
;; 	     (kbd "b") "emacs-bookmarks"
;;              (kbd "c") "emacs-capture"
;;              (kbd "d") "emacs-dired"
;; 	     (kbd "e") "emacs-erc"
;; 	     (kbd "g") "emacs-gnus"
;; 	     (kbd "i") "emacs-info"
;; 	     (kbd "t") "emacs-term"
;;              (kbd "C") "emacs-calendar")

;; (fill-keymap *volume-map*
;; 	     (kbd "u") "volume-up"
;; 	     (kbd "d") "volume-down"
;; 	     (kbd "m") "volume-toggle-mute")

;;; COMMENT: virtual desktops
(setf (group-name (first (screen-groups (current-screen)))) "default") ;; NOTE: rename 'Default' group 'default'

;; NOTE: don't really need this anymore
;; (run-commands "gnewbg internet" ;; NOTE: create internet group
;; 	      "gnewbg misc") ;; NOTE: create misc group

;;; COMMENT: run applications
(defun run-app (cmd prop &optional args) ;; FIX: fix
  "Run an instance of `cmd' with property `prop' (and any optional arguments `args')"
  (if (null args)
      (run-or-raise cmd prop)
    (run-or-raise (cat cmd " " args) prop)))

(defun run-terminal-app (cmd ttl &optional args) ;; FIX: fix
  "Run an instance of `cmd' with property `title' (and any optional arguments `args') in `*terminal*' titled `ttl'."
  (if (null args)
      (run-app (cat *terminal* " -t \"" ttl "\" -e \"" cmd "\"") (list :title ttl))
    (run-app (cat *terminal* " -t \"" ttl "\" -e \"" cmd " " args "\"") (list :title ttl))))

;; (defcommand exec-in-terminal (cmd) ((:string "Command: ")) (run-shell-command (format nil "~A -e ~A" *terminal* cmd))) ;; TODO: clean this up

(defcommand run-editor () () "Run an instance of `*editor*' with property `:instance'." (run-app *editor* (list :instance *editor*)))
(defcommand run-browser () () "Run an instance of `*browser*' with property `:instance'." (run-app *browser* (list :instance *browser*)))
(defcommand run-terminal () () "Run an instance of `*terminal*' with property `:title'." (run-app *terminal* (list :title *terminal*)))
(defcommand run-document-viewer () () "Run an instance of `*document-viewer' with `:instance'," (run-app *document-viewer* `(:instance ,*document-viewer*)))
(defcommand run-file-manager () () "Run an instance of `*file-manager' with `:instance'." (run-app *file-manager* `(:instance ,*file-manager*)))
(defcommand run-video-player () () "Run an instance of `*video-player*' with property `:instance'." (run-app *video-player* `(:instance ,*video-player*)))

(defcommand run-system-monitor () () "Run an instance of `*system-monitor*' with property `:title'." (run-terminal-app *system-monitor* *system-monitor*))
(defcommand run-package-manager () () "Run an instance of `*package-manager*' with property `:title'." (run-terminal-app *package-manager* *package-manager*))
(defcommand run-audio-player () () "Run an instance of `*audio-player*' with property `:title'." (run-terminal-app *audio-player* *audio-player*))

;; (defcommand run-irc () () "Run an instance of `*irc-client*' with property `:title'." (run-terminal-app *irc-client* *irc-client*))
;; (defcommand run-mail () () "Run an instance of `*mail-client*' with property `:title'." (run-terminal-app (cat "env TERM=xterm-256color " *mail-client*) *mail-client* "-n"))

(defcommand run-stumpish () () "Run an instance of `stumpish' with property `:title'." (run-terminal-app "stumpish" "stumpish"))
;; (defcommand run-screen () () "Run an instance of `screen' with property `:title'." (run-terminal-app "screen" "screen"))

(defcommand run-referencer () () "Run an instance of `referencer' with property `:instance'." (run-app "referencer" (list :instance "referencer")))
(defcommand run-tuxguitar () () "Run an instance of `tuxguitar' with property `:instance'." (run-app "tuxguitar" (list :instance "tuxguitar")))
(defcommand run-pyscrabble () () "Run an instance of `pyscrabble' with property `:instance'." (run-app "pyscrabble" (list :instance "pyscrabble")))
;; (defcommand run-banshee () () "Run an instance of `banshee' with property `:instance'." (run-app "banshee" (list :instance "banshee")))

;;; COMMENT: window placement
(defmacro frame-preference-rule (group-name prop-type prop-name)
  `(progn
     (case ,prop-type
       (c (define-frame-preference ,group-name (0 t t :class ,prop-name)))    ;; match the window's class
       (i (define-frame-preference ,group-name (0 t t :instance ,prop-name))) ;; match the window's instance or resource-name
       (r (define-frame-preference ,group-name (0 t t :role ,prop-name)))     ;; match the window's @code{WM_WINDOW_ROLE}
       (t (define-frame-preference ,group-name (0 t t :title ,prop-name)))))) ;; match the window's title

;; FIX: clean these rules up (the property names shouldn't be hardcoded)
;; instance rules
;; (frame-preference-rule "default" 'i "emacs")         ;; => *editor*
;; (frame-preference-rule "default" 'i "pcmanfm")             ;; => *file-manager*
;; (frame-preference-rule "default" 'i "x-terminal-emulator") ;; => *terminal*
;; (frame-preference-rule "default" 'i "evince")              ;; => *document-viewer*
;; (frame-preference-rule "internet" 'i "browser")            ;; => *browser*
;; (frame-preference-rule "internet" 'i "x-www-browser")   ;; => *browser*
;; title rules
;; (frame-preference-rule "misc" 't "htop")                   ;; => *system-monitor*
;; (frame-preference-rule "misc" 't "aptitude")               ;; => *package-manager*

;;; COMMENT: user commands
;; TODO: add `package-manager' commands
(defcommand reinit () () "Reload the stumpwm configuration file." (run-commands "reload" "loadrc"))

(defcommand show-battery () () "Show current battery status." (echo-string (current-screen) (run-shell-command "acpi" t)))
(defcommand show-uptime () () "Show current uptime." (echo-string (current-screen) (run-shell-command "uptime" t)))
(defcommand show-hostname () () "Show the hostname." (echo-string (current-screen) (cat "Hostname: " (hostname))))

;;; COMMENT: super user commands
(define-stumpwm-type :password (input prompt)
  (let ((history *input-history*)
        (arg (argument-pop input))
        (fn (symbol-function 'draw-input-bucket)))
    (unless arg
      (unwind-protect
           (setf (symbol-function 'draw-input-bucket)
                 (lambda (screen prompt input &optional errorp)
                   (let ((i (copy-structure input)))
                     (setf (input-line-string i)
                           (make-string (length (input-line-string i))
                                        :initial-element #\*))
                     (funcall fn screen prompt i)))
                 arg (read-one-line (current-screen) prompt))
        (setf (symbol-function 'draw-input-bucket) fn
              *input-history* history))
      arg)))

(defmacro sudo-command (name command &key output)
  (let ((cmd (gensym)))
    `(defcommand ,name (password) ((:password "Password: "))
      (let ((,cmd (concat "echo '" password "' | sudo -S " ,command)))
        ,(if output
             `(run-prog-collect-output *shell-program* "-c" ,cmd)
             `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

(sudo-command reboot "reboot")
(sudo-command shutdown "shutdown -h now")
(sudo-command hibernate "pm-hibernate")

;;; COMMENT: process management
(defun ps-exists (ps)
  (let ((f "ps -ef | grep ~S | grep -v -e grep -e stumpish | wc -l"))
    (< 0 (parse-integer (run-shell-command (format nil f ps) t)))))

(defun start-uniq-command-ps (command &key options (background t))
  (unless (ps-exists command)
    (run-shell-command
     (concat command " " options " " (when background "&")))))

(defun kill-ps-command (command)
  (format nil "kill -TERM `ps -ef | grep ~S | grep -v grep | awk '{print $2}'`"
          command))

(defun kill-ps (command)
  (run-shell-command (kill-ps-command command)))

(defcommand ps-kill (ps) ((:rest "Process to kill: "))
  (kill-ps ps))

;;; COMMENT: key sequence
(defun key-press-hook (key key-seq cmd)
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :top-right))
      (message "Key sequence: ~A" (print-key-seq (reverse key-seq))))
    (when (stringp cmd) ;; give them time to read it
      (sleep 0.5))))

(defmacro replace-hook (hook fn)
  `(remove-hook ,hook ,fn)
  `(add-hook ,hook ,fn))

(replace-hook *key-press-hook* 'key-press-hook)

;;; COMMENT: web jumping
;; (defmacro make-web-jump (name url-prefix)
;;   `(defcommand ,name (search) ((:rest ,(string-capitalize (string-downcase (concatenate 'string "Search in " (symbol-name name) " for: ")))))
;;      (run-shell-command (format nil (concatenate 'string *browser* " '~A=~A'") ,url-prefix (substitute #\+ #\Space search)))))

;; (make-web-jump google "http://www.google.com/search?q")
;; (make-web-jump wikipedia "http://en.wikipedia.org/wiki/Special:Search?search")

;;; COMMENT: safe quit
(defcommand safe-quit () () ;; redefine the "quit" command (only leave the stumpwm session if there are no windows open)
  "Checks if any windows are open before quitting."
  (let ((win-count 0))
    (dolist (group (screen-groups (current-screen))) ;; count the windows in each group
      (setq win-count (+ (length (group-windows group)) win-count)))
    (if (= win-count 0) ;; display the number of open windows or quit
        (run-commands "quit")
      (message (format nil "You have ~d ~a open" win-count
		       (if (= win-count 1) "window" "windows"))))))

;;; COMMENT: notifications
;; (define-key *root-map* (kbd "N") '*notifications-map*)

;; (defun notify (fmt args)
;;   (let ((*executing-stumpwm-command* nil)
;;         (*message-window-gravity* :center))
;;     (message-no-timeout fmt args)))
;; (export 'notify)

;; (define-stumpwm-command "notify" ((msg :rest "Notify: "))
;;   (notify "~a" msg))

;;; COMMENT: quick menu
;; (defparameter *quick-menu*
;;   '(("graphics"
;;      ("gimp" "gimp"))
;;     ("internet"
;;      ("chromium" "chromium-browser")
;;      ;; ("irssi"    "irssi") ;; fix
;;      ;; ("mutt"     "mutt") ;; fix
;;      ("pidgin"   "pidgin"))
;;     ("office"
;;      ;; ("emacs" "emacsclient %c") ;; fix (i.e. it opens a *new* session rather than jumping to an existing session
;;      ("office" "openoffice.org"))
;;     ("sound and video"
;;      ("guitar"  "tuxguitar")
;;      ("video"   "totem")
;;      ;; ("mplayer" "mplayer") ;; need to install mplayer (and then fix)
;;      )
;;     ("system tools"
;;      ("calculator"        "gnome-calculator")
;;      ("file manager"      "nautilus --no-desktop") ;; gconftool -t bool /apps/nautilus/preferences/show_desktop -s false (should just use dired somehow)
;;      ("nvidia x server"   "nvidia-settings")
;;      ("power preferences" "gnome-power-preferences")
;;      ("printers"          "system-config-printer"))
;;     ("x windows tools"
;;      ("clipboard"  "xclipboard")
;;      ("fonts"      "xfontsel")
;;      ("ruler"      "kruler")
;;      ("events"     "xev"))))

;; (defcommand menu () ()
;;   "Display quick access menu."
;;   (labels ((pick (options)
;;                  (let ((selection (select-from-menu
;;                                    (current-screen) options "")))
;;                    (cond
;;                     ((null selection)
;;                      (throw 'error "Abort."))
;;                     ((stringp (second selection))
;;                      (second selection))
;;                     (t
;;                      (pick (cdr selection)))))))
;;     (let ((choice (pick *quick-menu*)))
;;       (run-shell-command choice)))

;;; COMMENT: music player daemon
;; (setf *mpd-port* 7700
;;       *mpd-volume-step* 10
;;       ;; *mpd-status-fmt* "" ;; message display by mpd-status
;;       ;; *mpd-current-song-fmt* "" ;; message displayed by mpd-current-song
;;       *mpd-modeline-fmt* "%S: %a - %t (%n/%p)") ;; mode-line format for mpd

;;; COMMENT: volume control
;; (defcommand volume-up () ()
;;   "Increase volume level."
;;   (dotimes (n 10)
;;     (run-commands "amixer-Master-1+"))) ;; increase master volume +10

;; (defcommand volume-down () ()
;;   "Decrease volume level."
;;   (dotimes (n 10)
;;     (run-commands "amixer-Master-1-"))) ;; decrease master volume -10

;; (defcommand volume-toggle-mute () ()
;;   "Toggle between mute/unmute volume level."
;;   (run-commands "amixer-Master-toggle")) ;; toggle master between mute/unmute

;;; COMMENT: interact with emacs
;; (defun send-emacs-key-command (cmd) ;; FIX: fix
;;   "Send a key-command to an existing emacs session."
;;   (emacs)
;;   (send-meta-key (current-screen) (kbd cmd)))

;; (defun send-emacs-string (str)
;;   "Send a string to an extisting emacs session."
;;   (emacs)
;;   (window-send-string str))

;; (defcommand emacs-agenda () ()
;;   "View agenda in Emacs."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "org-agenda")
;;   (send-emacs-key-command "RET")
;;   (message "Starting agenda."))

;; (defcommand emacs-bookmarks () ()
;;   "Open bookmarks in Emacs."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "list-bookmarks")
;;   (send-emacs-key-command "RET")
;;   (message "Starting bookmarks."))

;; (defcommand emacs-capture () ()
;;   "Capture note in Emacs."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "org-capture")
;;   (send-emacs-key-command "RET")
;;   (message "Starting capture."))

;; (defcommand emacs-dired (directory) ((:string "Enter a directory: ")) ;; FIX: fix
;;   "Open directory for file management in Emacs."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "ido-dired")
;;   (send-emacs-key-command "RET")
;;   (send-emacs-string directory)
;;   (send-emacs-key-command "RET")
;;   (message "Starting dired."))

;; (defcommand emacs-calendar () ()
;;   "Open calendar in Emacs."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "calendar")
;;   (send-emacs-key-command "RET")
;;   (message "Starting calendar."))

;; (defcommand emacs-erc () ()
;;   "Start an IRC connection in Emacs with ERC."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "erc-start-or-switch")
;;   (send-emacs-key-command "RET")
;;   (message "Starting ERC."))

;; (defcommand emacs-gnus () ()
;;   "Read mail and RSS feeds in Emacs with GNUS."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "gnus")
;;   (send-emacs-key-command "RET")
;;   (message "Starting GNUS."))

;; (defcommand emacs-info () ()
;;   "Open info documentation in Emacs."
;;   (send-emacs-key-command "C-h")
;;   (send-emacs-key-command "i")
;;   (message "Starting info."))

;; (defcommand emacs-term () ()
;;   "Start a new (or switch to an existing) ANSI terminal session in Emacs."
;;   (send-emacs-key-command "M-x")
;;   (send-emacs-string "switch-term")
;;   (send-emacs-key-command "RET")
;;   (message "Starting term."))

;;; COMMENT: startup applications
(when *initializing*
  ;; (run-shell-command "emacs --daemon") ;; NOTE: start emacs server process
  ;; (launch-mpd) ;; NOTE: start mpd server
  ;; (run-editor) ;; NOTE: start the editor
  ;; (run-browser) ;; NOTE: start the browser
  ;; (run-system-monitor) ;; NOTE: start the system monitor
  ) 

;; (mpd-connect) ;; start mpd connection

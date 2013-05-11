;; FILE: /home/chu/.conf-scripts/stumpwm-dir/init.lisp
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; IMPORTANT: common-lisp implementation
;; SOURCE: ...
;debian=sbcl

;;; IMPORTANT: initial config
;; SOURCE: ...
(in-package :stumpwm) ;; NOTE: declare the package scope

(setf *default-package* :stumpwm ;; NOTE: set default package to be stumpwm
      *startup-message* nil ;; NOTE: suppress the startup message
      ;; *debug-level* 5 ;; NOTE: turn on stumpwm debugging (WARNING: creates massive text dumps)
      *shell-program* (getenv "SHELL") ;; NOTE: set the default shell
      *mouse-focus-policy* :sloppy) ;; NOTE: focus follows mouse (alternatives are: `:click', `:ignore', `:sloppy')

(redirect-all-output (data-dir-file "debug-output" "txt")) ;; NOTE: debug information `~/.stumpwm.d/debug-output.txt'

;; (set-prefix-key (kbd "C-t")) ;; NOTE: default stumpwm prefix key (control+t)
(set-prefix-key (kbd "s-z")) ;; NOTE: set stumpwm prefix key (super+z) 

;;; IMPORTANT: general functions
;; SOURCE: ...
(defun cat (&rest strings)
  "Return STRINGS concatenated together, like the Unix command 'cat'. A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

(defun hostname (&rest junk)
  "Return a string representing the hostname."
  (first (split-string (machine-instance) ".")))

;; TODO: need to look into "apt-cache policy application"
;; ..... possibly grep for "Installed: (none)" and return nil
(defun installed-p (application)
  "Return `t' if application is installed, return `nil' otherwise.")

;; IMPORTANT: the following functions are called during initialization
;; (defun launch-mpd (&rest junk)
;;   "Start music player daemon, `mpd', server."
;;   (run-shell-command "mpd"))

(defun launch-nm-applet (&rest junk)
  "Start the network manager applet, `nm-applet'."
  (run-shell-command "nm-applet"))

(defun launch-lxpanel (&rest junk)
  "Start an instance of `lxpanel'."
  (run-shell-command "lxpanel"))

(defun launch-emacs-daemon (&rest junk)
  "Start an instance of `emacs --daemon'."
  (run-shell-command "emacs --daemon"))

;; TODO: create (setup) the environment
;; (defun startup-function (&rest args)
;;   "Creates the StumpWM environment."
;;   (launch-emacs-daemon)
;;   (launch-lxpanel)
;;   (launch-nm-applet)
;;   (launch-mpd)
;;   ;; ---
;;   (run-editor)
;;   (run-browser))

;;; IMPORTANT: user variables
;; SOURCE: ...
;;(defvar *user-home-directory* (cat "/home/" (getenv "USER")) "User's home directory.")
(defvar *user-home-directory* (getenv "HOME") "User's home directory.")
(defvar *user-source-directory* (cat *user-home-directory* "/Programming/lisp/common-lisp/stumpwm") "Source directory.")
(defvar *user-quicklisp-directory* (cat *user-home-directory* "/quicklisp/dists/quicklisp/software") "Quicklisp directory path.")

;;; IMPORTANT: default applications
;; SOURCE: ...
(defvar *browser* "x-www-browser" "Set the default web browser.")
(defvar *terminal* "x-terminal-emulator" "Set the default terminal emulator.")
(defvar *editor* (getenv "EDITOR") "Set the default editor.") ;; NOTE: set shell environment editor
(defvar *file-manager* "pcmanfm" "Set the default file manager.")
(defvar *package-manager* "aptitude" "Set the default package manager.")
(defvar *system-monitor* "htop" "Set the default system monitor.")
(defvar *document-viewer* "evince" "Set the default document reader.")
;; (defvar *office-suite* "openoffice.org" "Set the default office suite.") ;; TODO: set this up
(defvar *audio-player* "ncmpcpp" "Set the default audio player.")
(defvar *video-player* "vlc" "Set the default video player.")

;; (defvar *default-applications* (list *browser* *terminal* *editor* *file-manager* *package-manager* *system-monitor* *document-viewer* *audio-player* *video-player*)) ;; TODO: do something with this list

;; (defun print-default-applications (&rest args)
;;   "Pretty print the list of default applications."
;;   (print (cat "=====================\n"
;; 	      "Default Applications:\n"
;; 	      "=====================\n"))
;;   (let ((counter 1))
;;     (mapc
;;      '(lambda (application) (progn (print "~D: ~A\n" counter application) (setq counter (1+ counter))))
;;      *default-applications*))
;;   (print "=====================\n"))

;;; IMPORTANT: (zenburn-inspired) color theme
;; SOURCE: ...
;; (defparameter *foreground-colour* "darkseagreen4" "Set the foreground colour.")
;; (defparameter *background-colour* "grey25" "Set the background colour.")
;; (defparameter *border-colour* "grey25" "Set the border colour.")
;; (defparameter *focus-colour* "darkseagreen1" "Set the focus colour.")
;; (defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")

;;; IMPORTANT: slime and swank
;; SOURCE: ...
;; NOTE: requires `quicklisp'
(load (cat *user-quicklisp-directory* "/slime-20120407-cvs/swank-loader.lisp")) ;; ERROR: should not be hardcoded
;;(load "/home/chu/quicklisp/dists/quicklisp/software/slime-20120407-cvs/swank-loader.lisp")
(swank-loader:init)

(defvar *swank-p* nil)

(defcommand run-swank () () ;; NOTE: command to start a swank server
  "Starts a swank server on port 4006 and notifies the user."
  (setf *top-level-error-action* :break)
  (if *swank-p*
      (message "Swank server already running.")
    (progn
      (swank:create-server :port 4006
                           :style swank:*communication-style*
                           :dont-close t)
      (setf *swank-p* t)
      (message "Swank server: M-x slime-connect RET RET, then enter (in-package :stumpwm) to begin."))))

;; (run-swank) ;; NOTE: start the swank server

;;; IMPORTANT: monitoring scripts
;; SOURCE: ...
(set-contrib-dir (cat *user-source-directory* "/contrib"))
;;(set-contrib-dir "/home/chu/Programming/lisp/common-lisp/stumpwm/contrib") ;; NOTE: set contrib directory

;; NOTE: load selected modules
(mapcar #'load-module '(;; "amixer"
			;; "app-menu"
			;; "aumix"
			;; "battery"
			"battery-portable"
			;; "cpu"
			;; "disk"
			;; "g15-keysyms"
			;; "maildir"
			;; "mem"
			;; "mpd"
			;; "net"
			;; "notifications"
			;; "passwd"
			;; "productivity"
			"sbclfix"
			;; "surfraw"
			;; "undocumented"
			;; "wifi"
			;; "window-tags"
			))

;;; IMPORTANT: window appearance
;; SOURCE: ...
(setf *normal-border-width* 0 ;; NOTE: the width in pixels given to the borders of regular windows
      *maxsize-border-width* 0 ;; NOTE: the width in pixels given to the borders of windows with maxsize or ratio hints
      *transient-border-width* 0 ;; NOTE: the width in pixels given to the borders of transient or pop-up windows
      *window-border-style* :thin) ;; NOTE: set the window border to thin (alternatives are: `:thick' `:thin' `:tight' `:none')

(set-normal-gravity :top)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

;; NOTE: set the font for the message and input bars, and the mode line (emacs font)
(set-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

;;; IMPORTANT: message and input box
;; SOURCE: ...
;; (set-bg-color *background-colour*)
;; (set-fg-color *foreground-colour*)
;; (set-border-color *border-colour*)
;; (set-focus-color *focus-colour*)
;; (set-unfocus-color *unfocus-colour*)
(set-msg-border-width 0)

(setf *message-window-gravity* :top-right ;; NOTE: set the message-box to the top right
      *input-window-gravity* :top-right ;; NOTE: set the input-box to the top right
      ;;*window-name-source* :title ;; NOTE: windows get their name from their title property
      *timeout-wait* 5) ;; NOTE: how long a message will appear for (in seconds)

;;; IMPORTANT: mode line
;; SOURCE: ...
(set-frame-outline-width 0)

;; (setf *mode-line-background-color* *background-colour*
      ;; *mode-line-foreground-color* *foreground-colour*
      ;; *mode-line-border-color* *border-colour*
      ;; *mode-line-border-width* 0 ;; NOTE: set thickness of the mode line border
      ;; *mode-line-pad-x* 0 ;; NOTE: set the padding between the mode line text and the sides
      ;; *mode-line-pad-y* 0 ;; NOTE: set the padding between the mode line text and the top/bottom
      ;; *mode-line-position* :top
      ;; *mode-line-screen-position* :top
      ;; *mode-line-frame-position* :top
      ;; *window-format* "%n%s %20t"
      ;; *window-info-format* "[%i] - (%t)"
      ;; *group-format* "%n:%t"
      ;; *mode-line-timeout* 1 ;; NOTE: update every second (if nothing else has triggered it already)
      ;; )

(setf *screen-mode-line-format*
      (list 
       "[^B%n^b] " ;; NOTE: display current group
       "%B " ;; NOTE: display battery details
       ;; "%l " ;; NOTE: show network connection details
       "%W " ;; window list
       ;; '(:eval (cat "(^B" (run-shell-command "date '+%R, %d %b'|tr -d [:cntrl:] " t) "^b) ")) ;; NOTE: display time
       ;; "["
       ;; "%M" ;; NOTE: display memory usage
       ;; "%C" ;; NOTE: display CPU metre as a bar
       ;; "%c" ;; NOTE: display CPU metre as digits
       ;; "%b" ;; NOTE: display battery details
       ;; "%I" ;; NOTE: display wireless details
       ;; "^B%m^b" ;; NOTE: display mpd details
       ;; "%D" ;; NOTE: display disk usage
       ;; "]"
       ;; " ^B%g^b" ;; NOTE: display group name
       ;; "^B%W^b" ;; NOTE: display current and available frames
       ))

;; (when (not (head-mode-line (current-head))) ;; NOTE: turn on the `mode-line'
;;    (toggle-mode-line (current-screen) (current-head)))

;; TODO: create an interactive mode-line formatter (???)
;; TODO: construct it as a list - without evaluating - then, just delq or addq elements as needed
;; (defvar *mode-line-format-cpu-memory* "[(%c) (%M)]" "Display CPU and memory details.")
;; (defvar *mode-line-format-time* '(:eval (cat " [(^B" (run-shell-command "date '+%d %b, %R'|tr -d [:cntrl:] " t) "^b)] ")) "Display the time.")
;; (defvar *mode-line-format-group-frames* "[(^B%w^b)]" "Display current group with frame details.")
;; (defvar *mode-line-format-groups-frame* "[(^B%g^bA)] [(^B%W^b)] " "Display groups with current frame details.")
;; (defvar *mode-line-format-battery-wireless* " [(%B) (%I)]" "Display battery and wireless connection details.")
;; (defvar *mode-line-format-mpd* "[^B%m^b]" "Display mpd details.")

;; (defvar *mode-line-format-list* (list 'cpu-memory 'time 'group-frames 'groups-frame 'battery-wireless 'mpd))

;; (defun interactive-mode-line (&rest junk)
;;   "Interactively add (and remove) entries from the StumpWM mode-line."
;;   (let ((format (cat "*mode-line-format-" (completing-read (current-screen) "Select mode line identifier: " *mode-line-format-list*) "*")))
;;     (when (not (head-mode-line (current-head))) ;; NOTE: turn on the `mode-line'
;;       (toggle-mode-line (current-screen) (current-head)))
;;     (setf *screen-mode-line-format* format)))

;;; IMPORTANT: key bindings
;; SOURCE: ...
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

;; (undefine-key *root-map* (kbd "c"))
;; (undefine-key *root-map* (kbd "C-c"))
;; (undefine-key *root-map* (kbd "e"))
;; (undefine-key *root-map* (kbd "C-e"))
;; (undefine-key *root-map* (kbd "C-b"))
;; (undefine-key *root-map* (kbd "C-a"))
;;(undefine-key *root-map* (kbd "C-m")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-l")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-w")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-k")) ;; ERROR: does not work

(defkeys-root ;; NOTE: define root-map keys
    ("s-g" "google") ;; NOTE: quick search google
    ("s-w" "wikipedia") ;; NOTE: quick search wikipedia
  ("s-R" "loadrc") ;; NOTE: reload run-time configuartion file
  ("C-m" "mode-line") ;; NOTE: (de)active the `mode-line'
  ("C-w" "run-swank") ;; NOTE: start a swank server
  ("M-b" "show-battery") ;; NOTE: show battery status
  ("M-c" "command-mode") ;; NOTE: active `command-mode'
  ("M-u" "show-uptime") ;; NOTE: show uptime status
  ("M-h" "show-hostname") ;; NOTE: show hostname
  ("M-i" "show-window-properties") ;; NOTE: show current window's properties
  )

(defkeys-top ;; NOTE: define top-map keys (these don't require prefix key)
    ("s-E" '*emacs-map*)
    ("s-S" '*sudo-map*)
  ;; ("s-M" '*mpd-map*)
  ("s-V" '*volume-map*)
  ;; ("s-N" '*notifications-map*) ;; TODO: setup
  ("s-:" "eval")
  ;; ("s-a" "run-audio-player") ;; NOTE: open (or switch to an existing instance of) *audio-player*
  ("s-b" "run-browser") ;; NOTE: open (or switch to an existing instance of) *browser*
  ("s-e" "run-editor") ;; NOTE: open (or switch to an existing instance of) *editor*
  ("s-f" "run-file-manager") ;; NOTE: open (or switch to an existing instance of) *file-manager*
  ("s-h" "run-system-monitor") ;; NOTE: open (or switch to an existing instance of) *system-monitor*
  ("s-s" "run-stumpish") ;; NOTE: open (or switch to an existing instance of) "stumpish"
  ;; ("s-i" "run-irc") ;; NOTE: open (or switch to an existing instance of) *irc-client*
  ;; ("s-m" "run-mail") ;; NOTE: open (or switch to an existing instance of) *mail-client*
  ("s-p" "run-package-manager") ;; NOTE: open (or switch to an existing insance of) *package-manager*
  ("s-t" "run-terminal") ;; NOTE: open (or switch to an existing instance of) *terminal*
  ;; ("s-v" "run-video-player") ;; NOTE: open (or switch to an existing instance of) *video-player*
  )

(defvar *sudo-map* nil "Super-user specific key-bindings.")
(defvar *emacs-map* nil "Emacs specific key-bindings.")
(defvar *volume-map* nil "Control volume key-bindings.")

(fill-keymap *sudo-map*
             (kbd "r") "reboot"
	     (kbd "s") "shutdown"
             (kbd "h") "hibernate")

(fill-keymap *emacs-map*
	     (kbd "a") "emacs-agenda"
	     (kbd "b") "emacs-bookmarks"
	     (kbd "c") "emacs-capture"
	     (kbd "d") "emacs-dired"
	     (kbd "e") "emacs-erc"
	     (kbd "g") "emacs-gnus"
	     (kbd "i") "emacs-info"
	     (kbd "t") "emacs-term"
	     (kbd "C") "emacs-calendar")

(fill-keymap *volume-map*
	     (kbd "u") "volume-up"
	     (kbd "d") "volume-down"
	     (kbd "m") "volume-toggle-mute")

;;; IMPORTANT: groups (virtual desktops) and frame preferences
;; SOURCE: ...
(defparameter *groups* '("default" "internet" "misc") "Group (virtual desktop) names.")

(setf (group-name (first (screen-groups (current-screen)))) "default") ;; NOTE: rename 'Default' group 'default'

;; NOTE: create new groups
(defun create-groups (&rest args)
  "Create new groups."
  (dolist (group (cdr *groups*)) ;; NOTE: the `car' of the list is the "default" group
    (gnewbg group)))

(create-groups)

(defmacro frame-preference-rule (group-name prop-type prop-name)
  `(progn
     (case ,prop-type
       (c (define-frame-preference ,group-name (0 t t :class ,prop-name)))    ;; NOTE: match the window's class
       (i (define-frame-preference ,group-name (0 t t :instance ,prop-name))) ;; NOTE: match the window's instance or resource-name
       (r (define-frame-preference ,group-name (0 t t :role ,prop-name)))     ;; NOTE: match the window's @code{WM_WINDOW_ROLE}
       (t (define-frame-preference ,group-name (0 t t :title ,prop-name)))))) ;; NOTE: match the window's title

;; FIX: clean these rules up (the property names shouldn't be hardcoded)
;; instance rules
;; (frame-preference-rule "default" 'i "emacs")               ;; => *editor*
;; (frame-preference-rule "default" 'i "pcmanfm")             ;; => *file-manager*
;; (frame-preference-rule "default" 'i "x-terminal-emulator") ;; => *terminal*
;; (frame-preference-rule "default" 'i "evince")              ;; => *document-viewer*
;; (frame-preference-rule "internet" 'r "browser")            ;; => *browser*
;; (frame-preference-rule "internet" 'i "browser")            ;; => *browser*
;; title rules
;; (frame-preference-rule "misc" 't "htop")                   ;; => *system-monitor*
;; (frame-preference-rule "misc" 't "aptitude")               ;; => *package-manager*

;; TEST: These are just temporary but they work
(define-frame-preference "default" (0 t t :instance "emacs"))
(define-frame-preference "default" (0 t t :instance "pcmanfm"))
(define-frame-preference "default" (0 t t :title "stumpish"))
(define-frame-preference "internet" (0 t t :role "browser")) ;; NOTE: this works for `conkeror'
(define-frame-preference "internet" (0 t t :instance "chromium-browser")) ;; NOTE: this works for `chromium-browser'
;;(define-frame-preference "misc" (0 t t :instance "x-terminal-emulator"))
(define-frame-preference "misc" (0 t t :title "terminal"))
(define-frame-preference "misc" (0 t t :title "htop"))
(define-frame-preference "misc" (0 t t :title "aptitude"))

;;; IMPORTANT: run applications
;; SOURCE: ...
;; (defcommand exec-in-terminal (cmd) ((:string "Command: ")) (run-shell-command (format nil "~A -e ~A" *terminal* cmd))) ;; TODO: clean this up

(defun run-or-raise-app (command property group) ;; FIX: fix
  "Run an instance of COMMAND with property PROPERTY under GROUP."
  ;; `(define-frame-preference ,group (0 t t ,property))
  (run-or-raise command property))

;; (run-or-raise-app *editor* (list :instance "emacs") "default")
;; (run-or-raise-app *browser* (list :role "browser") "internet")
;; (run-or-raise-terminal-app *system-monitor*)

(defun run-or-raise-terminal-app (cmd)
  "Run an instance of CMD in `*terminal*'."
  (let ((ttl cmd))
    ;; TODO: set `define-frame-preference'
    (run-or-raise (format nil "~A -t ~A -e ~A" *terminal* ttl cmd) (list :title ttl))))

;; TODO: needs to be moved into `run-or-raise-app' function which not only calls the `run-or-raise' but also sets the `define-frame-preference'
;; TODO: the following has some properties which are hard-coded :(
(defcommand run-editor () () "Launch `*editor*'." (run-or-raise *editor* (list :instance "emacs"))) ;; FIX: ...
;; (defcommand run-browser () () "Launch `*browser*'." (run-or-raise *browser* (list :role "browser")))
;; (defcommand run-browser () () "Launch `*browser*'." (run-or-raise *browser* (list :instance "chromium-browser")))
(defcommand run-browser () () "Launch `*browser*'." (run-or-raise *browser* (list :instance "x-www-browser"))) ;; FIX: ...
(defcommand run-file-manager () () "Launch `*file-manager'." (run-or-raise *file-manager* (list :instance *file-manager*)))
(defcommand run-document-viewer () () "Launch `*document-viewer'." (run-or-raise *document-viewer* (list :instance *document-viewer*)))
;; (defcommand run-referencer () () "Launch `referencer'." (run-app "referencer" (list :instance "referencer")))

;; NOTE: terminal apps
(defcommand run-terminal () () "Launch `*terminal*'." (run-or-raise (format nil "~A -t ~A" *terminal* "terminal") (list :instance *terminal*)))
(defcommand run-system-monitor () () "Launch `*system-monitor*'." (run-or-raise-terminal-app *system-monitor*))
(defcommand run-package-manager () () "Launch `*package-manager*'." (run-or-raise-terminal-app *package-manager*))
(defcommand run-stumpish () () "Launch `stumpish'." (run-or-raise-terminal-app "stumpish"))
;; (defcommand run-audio-player () () "Launch `*audio-player*'." (run-terminal-app *audio-player* *audio-player*))
;; (defcommand run-video-player () () "Launch `*video-player*'." (run-app *video-player* `(:instance ,*video-player*)))
;; (defcommand run-screen () () "Launch `screen'." (run-terminal-app "screen" "screen"))
;; (defcommand run-irc () () "Launch `*irc-client*'." (run-terminal-app *irc-client* *irc-client*))
;; (defcommand run-mail () () "Launch `*mail-client*'." (run-terminal-app *mail-client* *mail-client* "-n"))

(defcommand run-screenshot (filename) ((:string "Enter filename: "))
  "Capture current desktop with a screenshot."
  (run-shell-command (concat "import -window root \"" filename "\" &")))

;;; IMPORTANT: group configuration
;; SOURCE: ...
;; (defun screen-window-count ()
;;   "Return the number of window frames in the current screen."
;;   (let ((window-count 0))
;;     (dolist (group (screen-groups (current-screen))) ;; NOTE: count the windows in the screen (all the groups)
;;       (setq window-count (+ (length (group-windows group)) window-count)))
;;     window-count))

;; (defun group-window-count ()
;;   "Return the number of window frames in current group."
;;   (length (group-windows (current-group (current-screen))))) ;; NOTE: count the windows in the current group

(defun empty-group-p ()
  "Return `t' if the current group is empty, return `nil' otherwise."
  (let ((window-count (length (group-windows (current-group (current-screen))))))
    (if (> window-count 0) nil t)))

(defun switch-to-non-empty-group (window)
  "If the current group is empty (i.e. there are no windows open) then move to `default' group."
  (declare (ignore window))
  (when (empty-group-p)
    (run-commands "gselect 1")))

(add-hook *destroy-window-hook* 'switch-to-non-empty-group)

;; TODO: read and modify
;; SOURCE: `https://github.com/sabetts/stumpwm/wiki/TipsAndTricks'
;; (defun my-global-window-names ()
;;   "Returns a list of the names of all the windows in the current screen."
;;   (let ((groups (sort-groups (current-screen)))
;; 	(windows nil))
;;     (dolist (group groups)
;;       (dolist (window (group-windows group))
;; 	;; Don't include the current window in the list
;; 	(when (not (eq window (current-window)))
;; 	  (setq windows (cons (window-name window) windows)))))
;;     windows))

;; (defun my-window-in-group (query group)
;;   "Returns a window matching QUERY in GROUP."
;;   (let ((match nil)
;; 	(end nil)
;; 	(name nil))
;;     (dolist (window (group-windows group))
;;       (setq name (window-name window)
;; 	    end (min (length name) (length query)))
;;       ;; Never match the current window
;;       (when (and (string-equal name query :end1 end :end2 end)
;; 		 (not (eq window (current-window))))
;; 	(setq match window)
;; 	(return)))
;;     match))

;; (define-stumpwm-type :my-global-window-names (input prompt)
;;   (or (argument-pop input)
;;       (completing-read (current-screen) prompt (my-global-window-names))))

;; (define-stumpwm-command "global-select" ((query :my-global-window-names "Select: "))
;;   "Like select, but for all groups not just the current one."
;;   (let ((window nil))
;;     ;; Check each group to see if it's in
;;     (dolist (group (screen-groups (current-screen)))
;;       (setq window (my-window-in-group query group))
;;       (when window
;; 	(switch-to-group group)
;; 	(frame-raise-window group (window-frame window) window)
;; 	(return)))))

;; NOTE:
;; SOURCE: `https://github.com/sabetts/stumpwm/wiki/TipsAndTricks'
;; (defun raise-urgent-window-hook (target)
;;   (gselect (window-group target))
;;   (really-raise-window target))

;; (add-hook *urgent-window-hook* 'raise-urgent-window-hook)


;;; IMPORTANT: user commands
;; SOURCE: ...
;; TODO: add `package-manager' commands
(defcommand reinit () () "Reload the stumpwm configuration file." (run-commands "reload" "loadrc"))

(defcommand show-battery () () "Show current battery status." (echo-string (current-screen) (run-shell-command "acpi" t)))
(defcommand show-uptime () () "Show current uptime." (echo-string (current-screen) (run-shell-command "uptime" t)))
(defcommand show-hostname () () "Show the hostname." (echo-string (current-screen) (cat "Hostname: " (hostname))))

;;; IMPORTANT: (auto)mounting storage devices
;; SOURCE: ...
;; TODO: implement something like: "udisks -mount /dev/sdb1"

;;; IMPORTANT: super user commands
;; SOURCE: ...
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
       ""
      (let ((,cmd (concat "echo '" password "' | sudo -S " ,command)))
        ,(if output
             `(run-prog-collect-output *shell-program* "-c" ,cmd)
             `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

(sudo-command reboot "reboot")
(sudo-command shutdown "shutdown -h now")
(sudo-command hibernate "pm-hibernate")

;;; IMPORTANT: process management
;; SOURCE: ...
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

;;; IMPORTANT: key sequence
;; SOURCE: ...
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

;;; IMPORTANT: web jumping
;; SOURCE: ...
(defmacro make-web-jump (name url-prefix)
  `(defcommand ,name (search) ((:rest ,(concatenate 'string "Search " (string-downcase (symbol-name name)) " for: ")))
     (run-shell-command (format nil (concatenate 'string *browser* " '~A=~A'") ,url-prefix (substitute #\+ #\Space search)))))

(make-web-jump google "http://www.google.com/search?q")
(make-web-jump wikipedia "http://en.wikipedia.org/wiki/Special:Search?search")
;; TODO: make an emacs wiki web-jump
;; TODO: make a stumpwm wiki web-jump
;; TODO: make a common lisp wiki web-jump

;;; IMPORTANT: safe quit
;; SOURCE: ...
(defcommand safe-quit () () ;; NOTE: redefine the "quit" command (only leave the stumpwm session if there are no windows open)
  "Checks if any windows are open before quitting."
  (let ((win-count 0))
    (dolist (group (screen-groups (current-screen))) ;; count the windows in each group
      (setq win-count (+ (length (group-windows group)) win-count)))
    (if (= win-count 0) ;; display the number of open windows or quit
        (run-commands "quit")
      (message (format nil "You have ~d ~a open" win-count
		       (if (= win-count 1) "window" "windows"))))))

;;; IMPORTANT: notifications
;; SOURCE: ...
;; (define-key *root-map* (kbd "N") '*notifications-map*)

;; (defun notify (fmt args)
;;   (let ((*executing-stumpwm-command* nil)
;;         (*message-window-gravity* :center))
;;     (message-no-timeout fmt args)))
;; (export 'notify)

;; (define-stumpwm-command "notify" ((msg :rest "Notify: "))
;;   (notify "~a" msg))

;;; IMPORTANT: quick menu
;; SOURCE: ...
;; TODO: update to reflect defaults
;; (defparameter *quick-menu*
;;   '((("internet"
;;      ("chromium" "chromium-browser"))
;;     ("editor"
;;      ("emacs" "emacsclient -n -c"))
;;     ("sound and video"
;;      ("video"   "vlc")
;;      ("mplayer" "mplayer"))
;;     ("system tools"
;;      ("file manager" "pcmanfm")
;;      ("printers"     "system-config-printer")))))

;; (defcommand menu () ()
;;   "Display quick access menu."
;;   (labels
;;       ((pick (options)
;; 	     (let ((selection (select-from-menu (current-screen) options "")))
;; 	       (cond ((null selection) (throw 'error "Abort."))
;; 		     ((stringp (second selection)) (second selection))
;; 		     (t (pick (cdr selection)))))))
;;     (let ((choice (pick *quick-menu*)))
;;       (run-shell-command choice))))

;;; IMPORTANT: music player daemon
;; SOURCE: ...
;; (setf *mpd-port* 7700
;;       *mpd-volume-step* 10
;;       ;; *mpd-status-fmt* "" ;; message display by mpd-status
;;       ;; *mpd-current-song-fmt* "" ;; message displayed by mpd-current-song
;;       *mpd-modeline-fmt* "%S: %a - %t (%n/%p)") ;; mode-line format for mpd

;;; IMPORTANT: volume control
;; SOURCE: ...
(defcommand volume-up () ()
  "Increase volume level."
  (dotimes (n 10)
    (run-commands "amixer-Master-1+"))) ;; increase master volume +10

(defcommand volume-down () ()
  "Decrease volume level."
  (dotimes (n 10)
    (run-commands "amixer-Master-1-"))) ;; decrease master volume -10

(defcommand volume-toggle-mute () ()
  "Toggle between mute/unmute volume level."
  (run-commands "amixer-Master-toggle")) ;; toggle master between mute/unmute

;;; IMPORTANT: interact with emacs
;; SOURCE: ...
(defun send-emacs-key-command (cmd) ;; FIX: fix
  "Send a key-command to an existing emacs session."
  (emacs)
  (send-meta-key (current-screen) (kbd cmd)))

(defun send-emacs-string (str)
  "Send a string to an extisting emacs session."
  (emacs)
  (window-send-string str))

(defcommand emacs-agenda () ()
  "View agenda in Emacs."
  (send-emacs-key-command "M-x")
  (send-emacs-string "org-agenda")
  (send-emacs-key-command "RET")
  (message "Starting agenda."))

(defcommand emacs-bookmarks () ()
  "Open bookmarks in Emacs."
  (send-emacs-key-command "M-x")
  (send-emacs-string "list-bookmarks")
  (send-emacs-key-command "RET")
  (message "Starting bookmarks."))

(defcommand emacs-capture () ()
  "Capture note in Emacs."
  (send-emacs-key-command "M-x")
  (send-emacs-string "org-capture")
  (send-emacs-key-command "RET")
  (message "Starting capture."))

(defcommand emacs-dired (directory) ((:string "Enter a directory: ")) ;; FIX: fix
  "Open directory for file management in Emacs."
  (send-emacs-key-command "M-x")
  (send-emacs-string "ido-dired")
  (send-emacs-key-command "RET")
  (send-emacs-string directory)
  (send-emacs-key-command "RET")
  (message "Starting dired."))

(defcommand emacs-calendar () ()
  "Open calendar in Emacs."
  (send-emacs-key-command "M-x")
  (send-emacs-string "calendar")
  (send-emacs-key-command "RET")
  (message "Starting calendar."))

(defcommand emacs-erc () ()
  "Start an IRC connection in Emacs with ERC."
  (send-emacs-key-command "M-x")
  (send-emacs-string "erc-start-or-switch")
  (send-emacs-key-command "RET")
  (message "Starting ERC."))

(defcommand emacs-gnus () ()
  "Read mail and RSS feeds in Emacs with GNUS."
  (send-emacs-key-command "M-x")
  (send-emacs-string "gnus")
  (send-emacs-key-command "RET")
  ;; (send-emacs-key-command "C-<f4>")
  (message "Starting GNUS."))

(defcommand emacs-info () ()
  "Open info documentation in Emacs."
  (send-emacs-key-command "C-h")
  (send-emacs-key-command "i")
  (message "Starting info."))

(defcommand emacs-term () ()
  "Start a new (or switch to an existing) ANSI terminal session in Emacs."
  (send-emacs-key-command "M-x")
  (send-emacs-string "switch-term")
  (send-emacs-key-command "RET")
  (message "Starting term."))

;;; IMPORTANT: startup applications
;; SOURCE: ...
(when *initializing*
  ;; (run-shell-command "emacs --daemon") ;; NOTE: start emacs server process
  ;; (run-shell-command "lxpanel")
  ;; (run-shell-command "nm-applet")
  ;; ---
  ;; (launch-lxpanel) ;; NOTE: start `lxpanel' instance
  ;; (launch-nm-applet) ;; NOTE: start `nm-applet' instance
  ;; (launch-mpd) ;; NOTE: start mpd server
  ;; (mpd-connect) ;; NOTE: start mpd connection
  (run-editor) ;; NOTE: start the editor (should also launch the emacs daemon service)
  ;; (run-browser) ;; NOTE: start the browser
  ;; (run-system-monitor) ;; NOTE: start the system monitor
  )

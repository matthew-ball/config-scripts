;;; init.lisp --- Configuration for StumpWM environment

;; Copyright (C) 2013  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: window manager

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This is the main point of entry for the StumpWM environment.
;;; This configuration sets a bunch of variables, and modifies a bunch of the default settings.

;;; Code:

;;; IMPORTANT: common-lisp implementation
;; SOURCE: ...
;debian=sbcl

;;; IMPORTANT: initial config
(in-package :stumpwm) ;; NOTE: declare the package scope

(setf *default-package* :stumpwm ;; NOTE: set default package to be stumpwm
      *startup-message* nil ;; NOTE: suppress the startup message
      ;; *debug-level* 5 ;; NOTE: turn on stumpwm debugging (WARNING: creates massive text dumps)
      *shell-program* (getenv "SHELL") ;; NOTE: set the default shell
      *mouse-focus-policy* :sloppy) ;; NOTE: focus follows mouse (alternatives are: `:click' and `:ignore')

;; (setf *data-dir* (expand-file-name "~/.stumpwm.d/"))
(redirect-all-output (data-dir-file "debug-output" "txt")) ;; NOTE: debug information `~/.stumpwm.d/debug-output.txt'

(set-prefix-key (kbd "s-z")) ;; NOTE: set stumpwm prefix key (super+z)

;;; IMPORTANT: general functions
(defun hostname (&rest junk)
  "Return a string representing the hostname."
  (first (split-string (machine-instance) ".")))

(defun battery-charge ()
  "Return a string representing the current battery charge."
  (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun battery-state ()
  "Return a string representing the current battery state."
  (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
    (substitute #\Space #\Newline raw-battery)))

;; SOURCE: `http://deftsp-dotfiles.googlecode.com/svn/trunk/.stumpwmrc'
;; TODO: use functions `user-homedir-pathname' and `merge-pathnames' to update it.
(defun expand-file-name (path &optional default-directory)
  "Expand file-name."
  (let ((first-char (subseq path 0 1))
        (home-dir (concat (getenv "HOME") "/"))
        (dir (if default-directory
                 (if (string= (subseq (reverse default-directory) 0 1) "/")
                     default-directory
                     (concat default-directory "/")))))
    (cond ((string= first-char "~") (concat home-dir (subseq path 2)))
          ((string= first-char "/") path)
          (dir (if (string= (subseq dir 0 1) "/")
                   (concat dir path)
                   (expand-file-name (concat dir path))))
          (t (concat home-dir path)))))

;; SOURCE: `http://deftsp-dotfiles.googlecode.com/svn/trunk/.stumpwmrc'
(defmacro replace-hook (hook fn)
  `(remove-hook, hook, fn)
  `(add-hook, hook, fn))

;;; IMPORTANT: user variables
(defvar *user-home-directory* (getenv "HOME") "User's home directory.")
(defvar *user-source-directory* (concat *user-home-directory* "/Programming/lisp/common-lisp/stumpwm") "Source directory.")
(defvar *user-quicklisp-directory* (concat *user-home-directory* "/quicklisp/dists/quicklisp/software") "Quicklisp directory path.")

;;; IMPORTANT: default applications
;; NOTE: the following two probably only work on debian
(defvar *browser* "x-www-browser" "Set the default web browser.")
(defvar *terminal* "x-terminal-emulator" "Set the default terminal emulator.")
(defvar *editor* (getenv "EDITOR") "Set the default editor.") ;; NOTE: set shell environment editor

;; ERROR: hardcoded
(defvar *file-manager* "pcmanfm" "Set the default file manager.")
(defvar *package-manager* "aptitude" "Set the default package manager.")
(defvar *system-monitor* "htop" "Set the default system monitor.")
(defvar *document-viewer* "evince" "Set the default document reader.")
;; (defvar *office-suite* "openoffice.org" "Set the default office suite.") ;; TODO: set this up
(defvar *audio-player* "ncmpcpp" "Set the default audio player.")
(defvar *video-player* "vlc" "Set the default video player.")

;;; IMPORTANT: (zenburn-inspired) color theme
;; TODO: possibly need to set *colors* so that I can use the `zenburn' face colours in the mode-line format
;; (defparameter *foreground-colour* "darkseagreen4" "Set the foreground colour.")
;; (defparameter *background-colour* "grey25" "Set the background colour.")
;; (defparameter *border-colour* "grey25" "Set the border colour.")
;; (defparameter *focus-colour* "darkseagreen1" "Set the focus colour.")
;; (defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")

;;; IMPORTANT: slime and swank
;; NOTE: requires `quicklisp'
(load (concat *user-quicklisp-directory* "/slime-20120407-cvs/swank-loader.lisp")) ;; ERROR: hardcoded
;;(require 'swank) ;; TODO: work out!!

(swank-loader:init)

(defvar *swank-p* nil)

(defcommand run-swank () ()
  "Start a (persistent) swank server on port 4005."
  (setf *top-level-error-action* :break)
  (unless *swank-p*    
    (progn
      (swank:create-server :port 4005
                           :style swank:*communication-style*
                           :dont-close t)
      (setf *swank-p* t))))

;;; IMPORTANT: contribution scripts
(set-contrib-dir (concat *user-source-directory* "/contrib")) ;; NOTE: set contrib directory

;; NOTE: load selected modules
(mapcar #'load-module '(;; "amixer"
			;; "app-menu"
			;; "aumix"
			"battery"
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
			"wifi"
			;; "window-tags"
			))

;;(setf *prefer-sysfs* nil)

;;; IMPORTANT: windows, message and input box appearances
(setf *normal-border-width* 0 ;; NOTE: the width in pixels given to the borders of regular windows
      *maxsize-border-width* 0 ;; NOTE: the width in pixels given to the borders of windows with maxsize or ratio hints
      *transient-border-width* 0 ;; NOTE: the width in pixels given to the borders of transient or pop-up windows
      *window-border-style* :thin ;; NOTE: set the window border to thin (alternatives are: `:thick' `:thin' `:tight' `:none')
      *message-window-gravity* :top-right ;; NOTE: set the message-box to the top right
      *input-window-gravity* :top-right ;; NOTE: set the input-box to the top right
      ;;*window-name-source* :title ;; NOTE: windows get their name from their title property
      *suppress-abort-messages* t ;; NOTE: suppress abort message when non-nil
      *timeout-wait* 5 ;; NOTE: how long a message will appear for (in seconds)
      )

;; NOTE: set the font for the message and input bars, and the mode line (emacs font)
(set-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-frame-outline-width 0)
(set-normal-gravity :top)
(set-maxsize-gravity :top-right)
(set-transient-gravity :top-right)
;; NOTE: window border colours
(set-msg-border-width 0)
;; (set-focus-color *focus-colour*)
;; (set-unfocus-color *unfocus-colour*)
;; NOTE: input box colours
;; (set-fg-color *foreground-colour*)
;; (set-bg-color *background-colour*)
;; (set-border-color *border-colour*)

;;; IMPORTANT: mode line
;; NOTE: mode line colours
(setf ;; *mode-line-background-color* *background-colour*
      ;; *mode-line-foreground-color* *foreground-colour*
      ;; *mode-line-border-color* *border-colour*
      *mode-line-border-width* 1 ;; NOTE: set thickness of the mode line border
      *mode-line-pad-x* 1 ;; NOTE: set the padding between the mode line text and the sides
      *mode-line-pad-y* 0 ;; NOTE: set the padding between the mode line text and the top/bottom
      ;;*mode-line-position* :top
      *mode-line-timeout* 1 ;; NOTE: update every second (if nothing else has triggered it already)
      )

(defvar *mode-line-format* "[^B%n^b] ^[^3*%d^] " "Show the group name and date/time in the mode-line.")

(setf *screen-mode-line-format*
      (list
       '(:eval *mode-line-format*)
       ;; "[^[^1*%B^]] " ;; NOTE: display battery details
       ;; "[^[^2*"
       ;; '(:eval (battery-charge))
       ;; ":"
       ;; '(:eval (battery-state))
       ;; "^]] "
       ;; "%l " ;; NOTE: show network connection details
       ;; "%w"
       ;; "%W " ;; NOTE: window list ("%v " is similar)
       ;; "[" '(:eval (run-shell-command "acpi -b" t)) "]"
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

;;; IMPORTANT: key bindings
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

(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "C-e"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-a"))
;;(undefine-key *root-map* (kbd "C-n")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-p")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-l")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-w")) ;; ERROR: does not work
;;(undefine-key *root-map* (kbd "C-k")) ;; ERROR: does not work

(defkeys-root ;; NOTE: define root-map keys
    ("s-b" "global-select")
    ("s-q" "safe-quit")
  ("s-d" "trash-window")
  ("s-s" "trash-show")
  ("s-R" "loadrc") ;; NOTE: reload run-time configuartion file
  ("C-m" "mode-line") ;; NOTE: (de)active the `mode-line'
  ("M-c" "command-mode") ;; NOTE: active `command-mode'
  ("M-b" "show-battery") ;; NOTE: show battery status
  ("M-u" "show-uptime") ;; NOTE: show uptime status
  ("M-h" "show-hostname") ;; NOTE: show hostname
  ("M-i" "show-window-properties") ;; NOTE: show current window's properties
  )

(defkeys-top ;; NOTE: define top-map keys (these don't require prefix key)
    ("s-S" '*sudo-map*)
    ("s-V" '*volume-map*)
  ;; ("s-M" '*mpd-map*)
  ("s-:" "eval")
  ("s-x" "colon")
  ("s-b" "run-browser") ;; NOTE: open (or switch to an existing instance of) *browser*
  ("s-e" "run-editor") ;; NOTE: open (or switch to an existing instance of) *editor*
  ("s-t" "run-terminal") ;; NOTE: open (or switch to an existing instance of) *terminal*
  ("s-f" "run-file-manager") ;; NOTE: open (or switch to an existing instance of) *file-manager*
  ("s-h" "run-system-monitor") ;; NOTE: open (or switch to an existing instance of) *system-monitor*
  ("s-s" "run-stumpish") ;; NOTE: open (or switch to an existing instance of) "stumpish"
  ("s-p" "run-package-manager") ;; NOTE: open (or switch to an existing insance of) *package-manager*
  ;; ("s-a" "run-audio-player") ;; NOTE: open (or switch to an existing instance of) *audio-player*
  ;; ("s-v" "run-video-player") ;; NOTE: open (or switch to an existing instance of) *video-player*
  ;; ("s-i" "run-irc") ;; NOTE: open (or switch to an existing instance of) *irc-client*
  ;; ("s-m" "run-mail") ;; NOTE: open (or switch to an existing instance of) *mail-client*
  )

(defvar *sudo-map* nil "Super-user specific key-bindings.")
(defvar *volume-map* nil "Control volume key-bindings.")

(fill-keymap *sudo-map*
             (kbd "r") "reboot"
	     (kbd "s") "shutdown"
             (kbd "h") "hibernate")

(fill-keymap *volume-map*
	     (kbd "u") "volume-up"
	     (kbd "d") "volume-down"
	     (kbd "m") "volume-toggle-mute")

;; SOURCE: `https://github.com/sabetts/stumpwm/wiki/TipsAndTricks'
;; (defun raise-urgent-window-hook (target)
;;   (gselect (window-group target))
;;   (really-raise-window target))

;; (add-hook *urgent-window-hook* 'raise-urgent-window-hook)

;;; IMPORTANT: groups (virtual desktops) and frame preferences
(defparameter *groups* '("default" "internet" "misc") "Group (virtual desktop) names.")

(defun rename-default-group ()
  "Rename 'Default' group 'default'"
  (run-commands "gselect 1")
  (setf (group-name (first (screen-groups (current-screen)))) "default"))

(defun create-groups (&rest args)
  "Create new groups."
  (dolist (group (cdr *groups*)) ;; NOTE: the `car' of the list is the "default" group
    (gnewbg group)))

;; IMPORTANT: floating group stuff
(defcommand resize-floating-window (x y) ((:number x)
					  (:number y))
  ;; TODO: check to make sure `(current-group)' *is* a floating-group (!!!)
  (float-window-move-resize (current-window) x y))

;; IMPORTANT: group configuration
(defun screen-window-count () ;; NOTE: count the windows in the screen (all the groups)
  "Return the number of window frames in the current screen (i.e. all the groups)."
  (let ((window-count 0))
    (dolist (group (screen-groups (current-screen)))
      (setq window-count (+ (length (group-windows group)) window-count)))
    window-count))

(defun group-window-count () ;; NOTE: count the windows in the current group
  "Return the number of window frames in current group."
  (length (group-windows (current-group (current-screen)))))

;; TODO: this could be cleaned up ...
(defun switch-to-non-empty-group (window)
  "If the current group is empty (i.e. there are no windows open) then move to a non-empty group.

 If the screen is empty (all groups are empty) then switch back to the default group."
  (declare (ignore window))
  (when (= (group-window-count) 0)
    (if (= (screen-window-count) 0)
        (run-commands "gselect 1")
        (progn
          (let ((grp-num 1))
            (dolist (group (sort-groups (current-screen)))
              ;; NOTE: we'll find a non-empty group
              (unless (= (length (group-windows group)) 0)
                (run-commands (format nil "gselect ~A" grp-num)))
              (incf grp-num)))))))

(add-hook *destroy-window-hook* 'switch-to-non-empty-group)

;; IMPORTANT: global window select
;; SOURCE: `https://github.com/sabetts/stumpwm/wiki/TipsAndTricks'
(defun global-window-names ()
  "Returns a list of the names of all the windows in the current screen."
  (let ((groups (sort-groups (current-screen)))
	(windows nil))
    (dolist (group groups)
      (dolist (window (group-windows group))
	;; NOTE: don't include the current window in the list
	(when (not (eq window (current-window)))
	  (setq windows (cons (window-name window) windows)))))
    windows))

(defun window-in-group (query group)
  "Returns a window matching QUERY in GROUP."
  (let ((match nil)
	(end nil)
	(name nil))
    (dolist (window (group-windows group))
      (setq name (window-name window)
	    end (min (length name) (length query)))
      ;; NOTE: never match the current window
      (when (and (string-equal name query :end1 end :end2 end)
		 (not (eq window (current-window))))
	(setq match window)
	(return)))
    match))

(define-stumpwm-type :global-window-names (input prompt)
  (if (< (screen-window-count) 2)
      (switch-to-non-empty-group 'window)
      (or (argument-pop input)
          (completing-read (current-screen) prompt (global-window-names))))) ;; NOTE: :initial-input (car (global-window-names))

(defcommand global-select (query) ((:global-window-names "Select: "))
  "Like select, but for all groups not just the current one."
  (let ((window nil))
    ;; NOTE: check each group to see if it's in
    (dolist (group (screen-groups (current-screen)))
      (setq window (window-in-group query group))
      (when window
	(switch-to-group group)
	(frame-raise-window group (window-frame window) window)
	(return)))))

;;; IMPORTANT: run applications
;; TODO: ...
(defun make-keyword (name)
  "..."
  (values (intern name "KEYWORD")))

(defvar *programs-alist* '((*browser* . nil)
                           (*terminal* . t)
                           (*editor* . nil)
                           (*file-manager* . nil)
                           (*package-manager* . t)
                           (*system-monitor* . t)))

(defun programs-list ()
  ""
  (mapcar #'(lambda (program-alist) (eval (first program-alist))) *programs-alist*))

(define-stumpwm-type :program-list (input prompt)
    (or (argument-pop input)
        (completing-read (current-screen) prompt (programs-list))))

(defcommand run-program (program) ((:program-list "Select: "))
  "Run a program from the list `*programs*'."
  (run-or-raise program `(:instance ,program)))

;; TODO: need `add-program' function
;; TODO: need `run-program' function

(defun make-program-name (symbol)
  (let ((sym (symbol-name symbol)))
    (subseq sym 1 (- (length sym) 1))))

(defun make-program-list ()
  ""
  (mapcar #'(lambda (entry) (make-program-name (first entry))) *programs-alist*))

;; TODO: need to also add group window frame preference
;; (defmacro create-application-run-command (application group)
;;   (let ((application-name (make-program-name (quote application))))
;;     `(define-frame-preference ,group `(0 t t :instance ,application))
;;     `(defcommand (make-keyword (concat "run-" ,application-name)) () ()
;;        (run-or-raise ,application `(:instance ,application)))))

;;(create-application-run-command *browser*) ;; => `run-browser'

;;(clear-window-placement-rules) ;; NOTE: clear rules

(defmacro group-frame-preference (application group key)
  (let ((app (eval application)))
    `(define-frame-preference ,group (0 t t ,key ,app))))

(group-frame-preference *file-manager* "default" :instance)
(group-frame-preference "emacs" "default" :instance) ;; NOTE: unfortunately, `*editor*' won't work
(group-frame-preference "stumpish" "default" :title)
(group-frame-preference *browser* "internet" :instance)
(group-frame-preference *terminal* "misc" :title)
(group-frame-preference "htop" "misc" :title)
(group-frame-preference "utop" "misc" :title)
(group-frame-preference "aptitude" "misc" :title)

(defun run-or-raise-app (app)
  "Run (or raise) an instance of APP with `instance' property."
  (run-or-raise app `(:instance ,app)))

(defun run-or-raise-terminal-app (cmd ttl)
  "Run an instance of CMD in `*terminal*'."
  (run-or-raise (format nil "~A -t ~A -e ~A" *terminal* ttl cmd) (list :title ttl)))

;; NOTE: ...
(defcommand run-terminal () () (run-or-raise (format nil "~A -t ~A" *terminal* "terminal") (list :title *terminal*)))

;; NOTE: application run commands
(defcommand run-editor () () (run-or-raise *editor* (list :instance "emacs"))) ;; FIX: ...
;; ---
(defcommand run-browser () () (run-or-raise-app *browser*))
(defcommand run-file-manager () () (run-or-raise-app *file-manager*))
(defcommand run-document-viewer () () (run-or-raise-app *document-viewer*))
(defcommand run-referencer () () (run-or-raise-app "referencer"))

;; NOTE: terminal apps
(defcommand run-system-monitor () () (run-or-raise-terminal-app *system-monitor* "htop"))
(defcommand run-package-manager () () (run-or-raise-terminal-app *package-manager* "aptitude"))
;; (defcommand run-audio-player () () (run-terminal-app *audio-player* "ncmpcpp"))
;; (defcommand run-video-player () () (run-terminal-app *video-player* "mplayer"))

(defcommand run-stumpish () () (run-or-raise-terminal-app "stumpish" "stumpish"))
(defcommand run-screen () () (run-or-raise-terminal-app "screen -D -R" "screen"))
(defcommand run-user-monitor () () (run-or-raise-terminal-app (concat *system-monitor* " -u " (getenv "USER")) "utop"))

;;; IMPORTANT: user commands
(defcommand reinit () () "Reload the stumpwm configuration file." (run-commands "reload" "loadrc"))
(defcommand show-battery () () "Show current battery status." (echo-string (current-screen) (run-shell-command "acpi" t)))
(defcommand show-uptime () () "Show current uptime." (echo-string (current-screen) (run-shell-command "uptime" t)))
(defcommand show-hostname () () "Show the hostname." (echo-string (current-screen) (concat "Hostname: " (hostname))))

(defcommand run-screenshot (filename) ((:string "Enter filename: "))
  "Capture current desktop with a screenshot."
  (run-shell-command (concat "import -window root \"" filename "\" &")))

(defcommand safe-quit () ()
  "Checks if any windows are open before quitting."
  (let ((win-count 0))
    (dolist (group (screen-groups (current-screen))) ;; NOTE: count the windows in each group
      (setq win-count (+ (length (group-windows group)) win-count)))
    (if (= win-count 0) ;; NOTE: if there are 0 windows then quit, else display the number of open windows
        (run-commands "quit")
      (message (format nil "You have ~d ~a open" win-count
		       (if (= win-count 1) "window" "windows"))))))

;;; IMPORTANT: (auto)mounting storage devices
;; TODO: implement something like: "udisks -mount /dev/sdb1"

;;; IMPORTANT: super user commands
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
       "Enable StumpWM to execute commands as a privileged user."
      (let ((,cmd (concat "echo '" password "' | sudo -S " ,command)))
        ,(if output
             `(run-prog-collect-output *shell-program* "-c" ,cmd)
             `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

(sudo-command reboot "reboot")
(sudo-command shutdown "shutdown -h now")
(sudo-command hibernate "pm-hibernate")

;;; IMPORTANT: key sequence
;; SOURCE: `https://github.com/sabetts/stumpwm/wiki/FAQ'
;; (defun key-press-hook (key key-seq cmd)
;;   (declare (ignore key))
;;   (unless (eq *top-map* *resize-map*)
;;     (let ((*message-window-gravity* :top-right))
;;       (message "Key sequence: ~A" (print-key-seq (reverse key-seq))))
;;     (when (stringp cmd) ;; NOTE: give the user time to read it
;;       (sleep 0.5))))

;; (replace-hook *key-press-hook* 'key-press-hook)

;;; IMPORTANT: music player daemon
;; (setf *mpd-port* 7700
;;       *mpd-volume-step* 10
;;       ;; *mpd-status-fmt* "" ;; NOTE: message display by mpd-status
;;       ;; *mpd-current-song-fmt* "" ;; NOTE: message displayed by mpd-current-song
;;       *mpd-modeline-fmt* "%S: %a - %t (%n/%p)") ;; NOTE: mode-line format for mpd

;;; IMPORTANT: volume control
(defcommand volume-up () ()
  "Increase volume level."
  (dotimes (n 10)
    (run-commands "amixer-Master-1+"))) ;; NOTE: increase master volume +10

(defcommand volume-down () ()
  "Decrease volume level."
  (dotimes (n 10)
    (run-commands "amixer-Master-1-"))) ;; NOTE: decrease master volume -10

(defcommand volume-toggle-mute () ()
  "Toggle between mute/unmute volume level."
  (run-commands "amixer-Master-toggle")) ;; NOTE: toggle master between mute/unmute

;; IMPORTANT: hidden (trash) group
(defvar *trash-group* '() "Group containing the trashed windows")

(defcommand trash-window () ()
  "Put the current window in the trash group. If it doesn't exist, create it"
  (unless (or (eq (current-group) *trash-group*)
              (not (current-window)))
    (unless *trash-group*
      (setf *trash-group* (gnewbg ".trash")))
      ;; (setf *trash-group* (gnewbg-float ".trash")))
    (move-window-to-group (current-window) *trash-group*)))

(defcommand trash-show () ()
  "Switch to the trash group if it exists, call again to return to the previous group"
  (when *trash-group*
    (if (eq (current-group) *trash-group*)
        (switch-to-group (second (screen-groups (current-screen))))
        (switch-to-group *trash-group*))))

(defun clean-trash (w)
  "Called when a window is destroyed. If it was the last window of the trash group, destroy it"
  (let ((current-group (window-group w)))
    (when *trash-group*
      (when (and (eq current-group *trash-group*)
                 (not (group-windows current-group)))
        (if (eq (current-group) *trash-group*)
            (let ((to-group (second (screen-groups (current-screen)))))
              (switch-to-group to-group)
              (kill-group *trash-group* to-group))
            (kill-group *trash-group* (current-group)))
        (setf *trash-group* nil)))))

(add-hook *destroy-window-hook* 'clean-trash)

;;; IMPORTANT: startup applications (should be called during initialization)
;; (defun launch-mpd (&rest junk)
;;   "Start music player daemon, `mpd', server."
;;   (run-shell-command "mpd"))

(defun launch-nm-applet (&rest junk)
  "Start the network manager applet, `nm-applet'."
  (run-shell-command "nm-applet"))

(defun launch-lxpanel (&rest junk)
  "Start an instance of `lxpanel'."
  (run-shell-command "lxpanel"))

(defun mwsb-start-hook ()
  "Launch initiation process. This function is called the first time StumpWM is launched."
  ;; ---
  ;; (launch-lxpanel) ;; NOTE: start `lxpanel' instance
  ;; (launch-nm-applet) ;; NOTE: start `nm-applet' instance
  ;; (launch-mpd) ;; NOTE: start mpd server
  ;; (mpd-connect) ;; NOTE: start mpd connection
  ;; ---
  (run-swank) ;; NOTE: start the swank server
  (run-editor) ;; NOTE: start the editor
  ;; (run-browser) ;; NOTE: start the browser
  ;; (run-system-monitor) ;; NOTE: start the system monitor
  ;; ---
  ;; TODO: this only needs to be called once
  (rename-default-group)
  (create-groups)
  )

(replace-hook *start-hook* 'mwsb-start-hook)

(when *initializing*
  ;; (message "^2*Welcome to The ^BStump^b ^BW^bindow ^BM^banager!
  ;; Press ^5*~a ?^2* for help." (print-key *escape-key*))
  )

;;; init.lisp ends here

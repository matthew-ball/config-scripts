;;; init.lisp --- Configuration for StumpWM environment

;; Copyright (C) 2008-2014  Matthew Ball

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
;;; This configuration sets user-defined variables, and modifies the default settings.

;;; Code:

;;; IMPORTANT: common lisp implementation
;debian=sbcl

;;; IMPORTANT: initial config
(in-package :stumpwm) ;; NOTE: declare the package scope

(setf *default-package* :stumpwm ;; NOTE: set default package to be stumpwm
      *startup-message* nil ;; NOTE: suppress the startup message
      ;; *debug-level* 5 ;; NOTE: enable debugging (WARNING: creates massive text dumps)
      *shell-program* (getenv "SHELL") ;; NOTE: set the default shell
      *mouse-focus-policy* :sloppy) ;; NOTE: focus follows mouse

(redirect-all-output (data-dir-file "debug" "lisp")) ;; NOTE: debug information

(set-prefix-key (kbd "s-z")) ;; NOTE: set stumpwm prefix key (super+z)

;;; IMPORTANT: general functions
(defun host-name ()
  "Return a string representing the hostname."
  (first (split-string (machine-instance) ".")))

(defun system-name ()
  "Return symbol representing the system name."
  (let ((system-name (run-shell-command "lsb_release -i -s" t)))
    (values (intern (string-upcase (read-line (make-string-input-stream system-name)))))))

(defun battery-charge ()
  "Return a string representing the current battery charge."
  (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun battery-state ()
  "Return a string representing the current battery state."
  (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun cut-beyond (sequence max-length)
  "Cut a sequence beyond a specified length (because `subseq' is fickle)"
  (if (> (length sequence) max-length)
      (subseq sequence 0 max-length)
    sequence))

(defmacro replace-hook (hook fn)
  `(remove-hook, hook, fn)
  `(add-hook, hook, fn))

;; SOURCE: `http://deftsp-dotfiles.googlecode.com/svn/trunk/.stumpwmrc'
;; TODO: use functions `user-homedir-pathname' and `merge-pathnames' to update it.
;; (defun expand-file-name (path &optional default-directory)
;;   "Expand file-name."
;;   (let ((first-char (subseq path 0 1))
;;         (home-dir ;;(user-homedir-pathname)
;; 		  (concat (getenv "HOME") "/"))
;;         (dir (if default-directory
;;                  (if (string= (subseq (reverse default-directory) 0 1) "/")
;;                      default-directory
;; 		   (concat default-directory "/")))))
;;     (cond
;;      ((string= first-char "~") (concat home-dir (subseq path 2)))
;;      ((string= first-char "/") path)
;;      ;; TODO: maybe should ask for root password here (?)
;;      (dir (if (string= (subseq dir 0 1) "/")
;; 	      (concat dir path)
;; 	    (expand-file-name (concat dir path))))
;;      (t (concat home-dir path)))))

;;; IMPORTANT: user variables and default applications
;; NOTE: get from shell environment
(defvar *user-home-directory* (getenv "HOME") "User's home directory.")
(defvar *user-source-directory* (getenv "STUMPWM_SRC_DIR") "StumpWM source directory path.")
(defvar *user-quicklisp-directory* (getenv "QUICKLISP_DIR") "Quicklisp directory path.")
(defvar *user-slime-directory* (getenv "SLIME_DIR") "Slime directory.")
(defvar *user-projects-directory* (getenv "USER_PROJECTS_DIR") "User's projects directory.")
;; 
(defvar *browser* (getenv "BROWSER") "Default web browser.")
(defvar *terminal* (getenv "TERMINAL") "Default terminal emulator.")
(defvar *editor* (getenv "EDITOR") "Default editor.")
(defvar *file-manager* (getenv "FILE_MANAGER") "Default file manager.")
(defvar *package-manager* (getenv "PACKAGE_MANAGER") "Default package manager.")
(defvar *system-monitor* (getenv "SYSTEM_MONITOR") "Default system monitor.")
(defvar *document-viewer* (getenv "DOCUMENT_VIEWER") "Default document reader.")
(defvar *office-suite* (getenv "OFFICE_SUITE") "Default office suite.")
(defvar *audio-player* (getenv "AUDIO_PLAYER") "Default audio player.")
(defvar *video-player* (getenv "VIDEO_PLAYER") "Default video player.")

;;; IMPORTANT: (zenburn-inspired) color theme
;; TODO: set *colors* so that I can use the `zenburn' face colours in the mode-line format
;; (defparameter *foreground-colour* "darkseagreen4" "Set the foreground colour.")
;; (defparameter *background-colour* "grey25" "Set the background colour.")
;; (defparameter *border-colour* "grey25" "Set the border colour.")
;; (defparameter *focus-colour* "darkseagreen1" "Set the focus colour.")
;; (defparameter *unfocus-colour* "grey25" "Set the unfocus colour.")

;;; IMPORTANT: slime and swank
;; NOTE: requires `quicklisp'
(load (format nil "~A/swank-loader.lisp" *user-slime-directory*))
;; (load (concat *user-quicklisp-directory* "/slime-20130720-cvs/swank-loader.lisp")) ;; ERROR: hardcoded

(swank-loader:init)

(defvar *swank-p* nil "Predicate representing whether or not a common lisp swank server is active.")

(defcommand run-swank () ()
  "Start a (persistent) swank server on port 4005."
  (setf *top-level-error-action* :break)
  (unless *swank-p*
    (progn
      (swank:create-server :port 4005
			   :style swank:*communication-style*
			   :dont-close t)
      (setf *swank-p* t))))

;; NOTE: use `xfontsel' to discover fonts
;; (set-font "-*-helvetica-bold-r-normal-*-12-*-*-*-*-*-*-*")

(set-frame-outline-width 1)
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

;;; IMPORTANT: windows, message, input box appearances and mode line
(setf *normal-border-width* 1 ;; NOTE: the width in pixels given to the borders of regular windows
      *maxsize-border-width* 1 ;; NOTE: the width in pixels given to the borders of windows with maxsize or ratio hints
      *transient-border-width* 1 ;; NOTE: the width in pixels given to the borders of transient or pop-up windows
      *window-border-style* :thin ;; NOTE: set the window border to thin (alternatives are: `:thick' `:thin' `:tight' `:none')
      *message-window-gravity* :top-right ;; NOTE: set the message-box to the top right
      *input-window-gravity* :top-right ;; NOTE: set the input-box to the top right
      ;;*window-name-source* :title ;; NOTE: windows get their name from their title property
      ;;*window-format* "%m%n%s%c" ;; NOTE: show application `instance' instead of application `title'
      *suppress-abort-messages* t ;; NOTE: suppress abort message when non-nil
      *timeout-wait* 5 ;; NOTE: how long a message will appear for (in seconds)
      *input-history-ignore-duplicates* t ;; NOTE: ;; don't add duplicate commands to the command history
      ;; *mode-line-pad-x* 1 ;; NOTE: set the padding between the mode line text and the sides
      ;; *mode-line-pad-y* 1 ;; NOTE: set the padding between the mode line text and the top/bottom
      ;; *mode-line-border-width* 1 ;; NOTE: set thickness of the mode line border
      ;; *mode-line-timeout* 1 ;; NOTE: update every second (if nothing else has triggered it already)
      ;; *mode-line-background-color* *background-colour*
      ;; *mode-line-foreground-color* *foreground-colour*
      ;; *mode-line-border-color* *border-colour*
      ;; *mode-line-position* :top
      )

;;; IMPORTANT: contribution scripts
;; TODO: new versions of StumpWM no longer come with a contrib/ directory
;; TODO: so it has to be pulled in as a separate project
;; (set-contrib-dir (concat *user-source-directory* "/contrib")) ;; NOTE: set contrib directory
(set-contrib-dir (format nil "~A/Public/contrib/" *user-home-directory*)) ;; NOTE: set contrib directory

;; NOTE: load selected modules
(mapcar #'load-module '(;; "amixer"
			;; "app-menu"
			;; "aumix"
			;; "battery"
			;; "battery-portable"
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
			;; "stumptray" ;; WARNING: additional requirements ...
			;; "surfraw"
			;; "undocumented"
			;; "wifi"
			;; "window-tags"
			))

;; (setf *prefer-sysfs* nil)

;; (setf *disk-usage-paths* '("/"
;; 			   "/home")) ;; NOTE: see ../contrib/disk.lisp

(defun mode-line-battery-details ()
  "Return string of battery details."
  (format nil "^[^~A*%B^]" (cond
			    ((string-equal (battery-state) " Discharging ") 1) ;; NOTE: red string
			    ((string-equal (battery-state) " Charging ") 4) ;; NOTE: blue string
			    (t 7) ;; NOTE: default string
			    )))

;; TODO: this can be cleaned up quite substantially
(defun mode-line-wireless-details ()
  "Return string of wireless details."
  (let* ((name (run-shell-command "nmcli con status" t))
	 (wireless (run-shell-command "cat /proc/net/wireless" t))
	 (start-wireless (+ (search "wlan0:" wireless) 14))
	 (start-name (+ (search "MASTER-PATH" name) 44)))
    (format nil "~A: ~A%%"
	    (remove #\Newline (subseq name start-name (+ start-name 10)))
	    (subseq wireless start-wireless (+ start-wireless 2)))))

;;; IMPORTANT: mode line
(setf *screen-mode-line-format*
      (list
       ;; "^B[^b%n^B]^b "
       ;;"^B%M %N^b - " ;; NOTE: display memory usage (with bar)
       ;; "^B%M^b" ;; NOTE: ... (without bar)
       ;;"^B%c %C %t^b - " ;; NOTE: display CPU usage and temperature (with bar)
       ;; "- ^B%c %t^b" ;; NOTE: ... (without bar)
       ;;"^B%D^b" ;; NOTE: display disk usage
       ;; ----
       "[^B%n^b] %d ^B%M^b^B%c^b"
       ;; '(:eval (cut-beyond "[^B%n^b] %d ^B%M^b^B%c %t^b" 50))
       ;; ----
       ;; TODO: this should be a string which changes colour depending on current state of battery
       ;; "^[^7*%B^]" ;; NOTE: display battery details
       ;; '(:eval (mode-line-battery-details)) ;; ...
       ;; "[^[^2*"
       ;; '(:eval (battery-charge))
       ;; ":"
       ;; '(:eval (battery-state))
       ;; "^]] "
       ;; " %l" ;; NOTE: show network connection details
       ;; "%w"
       ;; "%W " ;; NOTE: window list ("%v " is similar)
       ;; "[" '(:eval (run-shell-command "acpi -b" t)) "]"
       ;; "["
       ;; "%b" ;; NOTE: display battery details
       ;; " %I" ;; NOTE: display wireless details
       ;; " "
       ;; '(:eval (mode-line-wireless-details)) ;; ...
       ;; "^B%m^b" ;; NOTE: display mpd details
       ;; "]"
       ;; " ^B%g^b" ;; NOTE: display group name
       ;; "^B%W^b" ;; NOTE: display current and available frames
       ))

;; (when (not (head-mode-line (current-head))) ;; NOTE: when the `mode-line' is disabled, turn it on
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
    ("s-q" "safe-quit")
    ("s-Q" "logout")
    ("s-d" "trash-window")
  ("s-s" "trash-show")
  ("s-R" "reinit") ;; NOTE: reload run-time configuartion file
  ("C-m" "mode-line") ;; NOTE: (de)active the `mode-line'
  ("M-c" "command-mode") ;; NOTE: active `command-mode'
  ("M-b" "show-battery") ;; NOTE: show battery status
  ("M-u" "show-uptime") ;; NOTE: show uptime status
  ("M-h" "show-host-name") ;; NOTE: show host name
  ("M-s" "show-system-name") ;; NOTE: show system name
  ("M-i" "show-window-properties")) ;; NOTE: show current window's properties

(defkeys-top ;; NOTE: define top-map keys (these don't require prefix key)
    ("s-S" '*sudo-map*)
    ("s-V" '*volume-map*)
  ("s-M" '*mpd-map*)
  ("s-G" "vgroups")
  ("s-B" "global-select")
  ("s-:" "eval")
  ("s-x" "colon")
  ("s-b" "run-browser") ;; NOTE: open (or switch to an existing instance of) *browser*
  ("s-e" "run-editor") ;; NOTE: open (or switch to an existing instance of) *editor*
  ("s-t" "run-terminal") ;; NOTE: open (or switch to an existing instance of) *terminal*
  ("s-f" "run-file-manager") ;; NOTE: open (or switch to an existing instance of) *file-manager*
  ("s-h" "run-system-monitor") ;; NOTE: open (or switch to an existing instance of) *system-monitor*
  ("s-s" "run-stumpish") ;; NOTE: open (or switch to an existing instance of) "stumpish"
  ("s-p" "run-package-manager") ;; NOTE: open (or switch to an existing insance of) *package-manager*
  ("s-a" "run-audio-player") ;; NOTE: open (or switch to an existing instance of) *audio-player*
  ("s-v" "run-video-player")) ;; NOTE: open (or switch to an existing instance of) *video-player*

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

;; IMPORTANT: hidden (trash) group
;; SOURCE: `https://github.com/stumpwm/stumpwm/wiki/StumpPatches'
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

(defun clean-trash (window)
  "Called when a window is destroyed. If it was the last window of the trash group, destroy it"
  (let ((current-group (window-group window)))
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

;;; IMPORTANT: groups (virtual desktops)
(defparameter *default-group* '("default") "Default StumpWM group object.")
(defparameter *tiling-groups* '("internet" "misc") "Tiling group objects.")
(defparameter *floating-groups* nil "Floating group objects.")
(defparameter *groups* (append *default-group* *tiling-groups* *floating-groups*) "StumpWM group (virtual desktop) object names.")

;; NOTE: the following two functions probably already exist (or at least, something very much like them must exist)
;; this implementation is poor and relies on hardcoded groups.
(defun tiling-group-p (group)
  "Predicate returning whether or not GROUP is a tiling group."
  (if (member group *tiling-groups* :test #'string-equal)
      t
    nil))

(defun floating-group-p (group)
  "Predicate returning whether or not GROUP is a floating group."
  (if (member group *floating-groups* :test #'string-equal)
      t
    nil))

(defun rename-default-group ()
  "Rename 'Default' group 'default'."
  (run-commands "gselect 1")
  (setf (group-name (first (screen-groups (current-screen)))) (first *groups*)))

(defun create-groups ()
  "Create new groups."
  ;; (unless *trash-group*
  ;;   (setf *trash-group* (gnewbg ".trash")))
  (unless (eq (cdr *groups*) nil) ;; NOTE: the `car' of the list is the "default" group
    (dolist (group (cdr *groups*))
      (cond
       ((tiling-group-p group) (gnewbg group))
       ((floating-group-p group) (gnewbg-float group))
       ;;(t ...) ;; ERROR: something needs to be done in this case, even if it (theoretically) will never get reached
       ))))

;; WARNING: not sure how this will work with multiple screens
(defun screen-window-count () ;; NOTE: count the windows in the screen (all the groups)
  "Return the number of window frames in the current screen (i.e. all the groups)."
  (let ((window-count 0))
    (dolist (group (screen-groups (current-screen)))
      (unless (string-equal (group-name group) ".trash")
	(setq window-count (+ (length (group-windows group)) window-count))))
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
              (unless (or (= (length (group-windows group)) 0) (string-equal (group-name group) ".trash"))
                (run-commands (format nil "gselect ~A" grp-num)))
              (incf grp-num)))))))

(add-hook *destroy-window-hook* 'switch-to-non-empty-group)

;; IMPORTANT: mouse interaction
;; NOTE: *root-click-hook* and *mode-line-click-hook*

;; (defun root-dimensions (screen code x y)
;;   "..."
;;   (message (format nil "Mouse button ~A, (x:~A, y:~A)" code x y)))

;; (add-hook *root-click-hook* 'root-dimensions)

;; IMPORTANT: mode-line group scrolling
(defun mode-line-scroll (&rest args)
  "Scroll between groups by clicking on the mode-line."
  (cond
   ((eq (second args) 1)
    (run-commands "gnext")) ;; NOTE: left mouse button
   ((eq (second args) 2)
    (run-commands "gother")) ;; NOTE: middle mouse click
   ((eq (second args) 3)
    (run-commands "gprev")))) ;; NOTE: right mouse button

(add-hook *mode-line-click-hook* 'mode-line-scroll)

;; IMPORTANT: floating group stuff
;; (defcommand resize-floating-window (x y) ((:number x) (:number y))
;;   "Resize floating group application window."
;;   ;; TODO: (unless (floating-group-p (group-name (current-group))) ...
;;   (float-window-move-resize (current-window) x y))

;; SOURCE: `http://paste.debian.net/72809'
;; (defcommand move-window-right (val) (:number)
;;   "Move current floating window right by VAL."
;;   (float-window-move-resize (current-window)
;;                             :x (+ (window-x (current-window)) val)))

;; (defcommand move-window-down (val) (:number)
;;   "Move current floating window down by VAL."
;;   (float-window-move-resize (current-window)
;;                             :y (+ (window-y (current-window)) val)))

;; (define-key *float-group-top-map* (kbd "s-Left") "move-window-right -1")
;; (define-key *float-group-top-map* (kbd "s-Left") "move-window-right 1")

;; (define-key *top-map* (kbd "s-Left") "move-window-right -10")
;; (define-key *top-map* (kbd "s-Right") "move-window-right 10")
;; (define-key *top-map* (kbd "s-Up") "move-window-down -10")
;; (define-key *top-map* (kbd "s-Down") "move-window-down 10")

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
      ;; NOTE: don't scan over hidden groups (they're hidden for a reason)
      (unless (string-equal ".trash" (group-name group))
	(setq window (window-in-group query group))
	(when window
	  (switch-to-group group)
	  (frame-raise-window group (window-frame window) window)
	  (return))))))

;;; IMPORTANT: frame preferences
(defmacro group-frame-preference (application group key)
  (let ((app (eval application)))
    `(define-frame-preference ,group (0 t t ,key ,app))))

;;(clear-window-placement-rules) ;; NOTE: clear rules

;; TODO: too much hard-coding
(defun define-window-placement-rules ()
  "Define placement rules for windows."
  (group-frame-preference *file-manager* "default" :instance)
  (group-frame-preference "emacs" "default" :instance) ;; NOTE: unfortunately, `*editor*' won't work
  (group-frame-preference "stumpish" "default" :title)
  ;; (group-frame-preference *browser* "internet" :instance)
  (group-frame-preference "chromium" "internet" :instance)
  (group-frame-preference "terminal" "misc" :title)
  (group-frame-preference "system-monitor" "misc" :title)
  (group-frame-preference "user-monitor" "misc" :title)
  (group-frame-preference "package-manager" "misc" :title)
  (group-frame-preference "screen" "misc" :title)
  ;; (group-frame-preference "xfdesktop" ".trash" :instance) ;; TODO: group frame preference for XFCE
  )

;;; IMPORTANT: run applications
(defun make-keyword (name)
  "Turn string `NAME' into a Common Lisp keyword."
  (values (intern name "STUMPWM")))

(defmacro create-application (name command &optional (property `(list :instance ,name)) (group "default"))
  "Create a general framework for the running (raising) of applications."
  `(define-frame-preference ,group (0 t t ,key ,name))
  `(defcommand ,(make-keyword (format nil "run-~a" `,name)) () ()
     "Run (or raise) application."
     (run-or-raise ,command ,property)))

(defun terminal-command (command title)
  (format nil "~S -T ~S -e ~S" (getenv "TERMINAL") title command))

(create-application "editor" (getenv "EDITOR") (list :instance "emacs"))
(create-application "browser" (getenv "BROWSER") (list :instance "chromium"))
(create-application "file-manager" (getenv "FILE_MANAGER"))
(create-application "document-viewer" (getenv "DOCUMENT_VIEWER"))
(create-application "office-suite" (getenv "OFFICE_SUITE"))
(create-application "terminal" (format nil "~S -T ~S" (getenv "TERMINAL") "terminal") (list :title "terminal"))
(create-application "screen" (terminal-command "screen -D -R" "screen"))
(create-application "system-monitor" (terminal-command (getenv "SYSTEM_MONITOR") "system-monitor"))
(create-application "user-monitor" (terminal-command (format nil "~A -u ~A" (getenv "SYSTEM_MONITOR") (getenv "USER")) "user-monitor"))
(create-application "package-manager" (terminal-command (getenv "PACKAGE_MANAGER") "package-manager"))
;; (create-application "audio-player" (terminal-command (getenv "AUDIO_PLAYER") "audio-player"))
;; (create-application "media-player" (getenv "MEDIA_PLAYER"))
;; (create-application "video-player" (getenv "VIDEO_PLAYER"))
;; (create-application "stumpish" (terminal-command "stumpish" "stumpish")) ;; FIX: set up `stumpish'

;;; IMPORTANT: user commands
(defcommand reinit () () "Reload the stumpwm configuration file." (run-commands "reload" "loadrc"))
(defcommand show-battery () () "Show current battery status." (echo-string (current-screen) (run-shell-command "acpi" t)))
(defcommand show-uptime () () "Show current uptime." (echo-string (current-screen) (run-shell-command "uptime" t)))
(defcommand show-host-name () () "Show the host name." (echo-string (current-screen) (concat "Host name: " (host-name))))
(defcommand show-system-name () () "Show the system name." (echo-string (current-screen) (concat "System name: " (string-downcase (symbol-name (system-name))))))

(defcommand run-screenshot (filename) ((:string "Enter filename: "))
  "Capture current desktop with a screenshot."
  (run-shell-command (concat "import -window root \"" filename "\" &")))

(defcommand run-ssh (connection) ((:string "Enter connection address: "))
  "Connect via ssh to CONNECTION."
  (run-or-raise-terminal-app (format nil "ssh ~A" connection) "ssh"))

(defcommand logout () ()
  (run-shell-command "xfce4-session-logout"))

(defcommand safe-quit () ()
  "Checks if any windows are open before quitting."
  (let ((win-count 0))
    (dolist (group (screen-groups (current-screen))) ;; NOTE: count the windows in each group
      (unless (string-equal (group-name group) ".trash")
	(setq win-count (+ (length (group-windows group)) win-count))))
    (if (= win-count 0) ;; NOTE: if there are 0 windows then quit, else display the number of open windows
	(run-commands "quit")
      (message (format nil "You have ~d ~a open" win-count
		       (if (= win-count 1) "window" "windows"))))))

;;; IMPORTANT: super user commands
(define-stumpwm-type :password (input prompt)
  (let ((history *input-history*)
        (arg (argument-pop input))
        (fn (symbol-function 'draw-input-bucket)))
    (unless arg
      (unwind-protect
           (setf (symbol-function 'draw-input-bucket)
                 ;; (lambda (screen prompt input &optional errorp)
		 (lambda (screen prompt input)
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
;; WARNING: this (understandably) slows the stumpwm responsiveness
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

;;; IMPORTANT: startup applications (should be called during initialization)
;; (defun launch-mpd ()
;;   "Start music player daemon, `mpd', server."
;;   (run-shell-command "mpd"))

(defun launch-emacs-daemon ()
  "Launch an emacs daemon process."
  (run-shell-command "emacs --daemon"))

(defun launch-xfce-panel ()
  "Start an instance of `xfce4-panel'."
  (run-shell-command "xfce4-panel --disable-wm-check"))

;; (defun launch-nm-applet ()
;;   "Start nm-applet instance with ConsoleKit."
;;   (run-shell-command "ck-launch-session nm-applet"))

(defun mwsb-start-hook ()
  "Launch initiation process. Start the user environment; launch anything which is user-specific here (such as panels, music servers, etc).

This function is only called the first time StumpWM is launched."
  (run-swank) ;; NOTE: start the swank server
  (launch-emacs-daemon) ;; NOTE: start the emacs daemon service
  (launch-xfce-panel) ;; NOTE: start panel
  ;; (launch-nm-applet) ;; NOTE: start nm-applet
  ;; (launch-mpd) ;; NOTE: start mpd server
  ;; (mpd-connect) ;; NOTE: start mpd connection
  ;; (run-editor) ;; NOTE: start the editor
  ;; (run-browser) ;; NOTE: start the browser
  ;; (run-system-monitor) ;; NOTE: start the system monitor
  ;; NOTE: the following modifies the StumpWM environment with user-specific settings
  (rename-default-group)
  (create-groups)
  (define-window-placement-rules))

(replace-hook *start-hook* 'mwsb-start-hook)

(when *initializing*
  ;; (message "^2*Welcome to The ^BStump^b ^BW^bindow ^BM^banager!
  ;; Press ^5*~a ?^2* for help." (print-key *escape-key*))
  )

;;; init.lisp ends here

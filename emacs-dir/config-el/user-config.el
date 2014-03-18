;;; user-config.el --- Configuration for user settings/options

;; Copyright (C) 2008-2014  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: configuration

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

;; Configuration for user settings and options.

;;; Code:

;;; IMPORTANT: the insidious big brother database
;; SOURCE: `http://www.emacswiki.org/emacs/BbdbMode'
;; (autoload 'bbdb "bbdb" "" t)

;; (after "bbdb"
;;  (bbdb-initialize 'gnus 'message)

;;  (setq bbdb-file "~/.emacs.d/contacts-file.el"))

;;; IMPORTANT: make `ido' available everywhere
;; SOURCE: `https://github.com/technomancy/ido-ubiquitous'
(after "ido"
  (require 'ido-ubiquitous)
  (ido-ubiquitous-mode t))

;;; IMPORTANT: improve `ido's flex matching
;; SOURCE: `https://github.com/lewang/flx'
;; ERROR: there's a bug with flx, such that it doesn't refresh/redisplay the `frame-title' - minor issue
(after "ido"
  (require 'flx)
  (flx-ido-mode t))

;;; IMPORTANT: smex mode
;; SOURCE: `http://emacswiki.org/emacs/Smex'
(autoload 'smex "smex" "Super-charge ido-mode." t)

(after "smex"
  (setq smex-save-file (concat user-emacs-directory "smex-items")
        smex-key-advice-ignore-menu-bar t)

  (smex-initialize)) ;; NOTE: super-charge `ido-mode'

;;; IMPORTANT: browse kill ring
;; SOURCE: `http://www.emacswiki.org/BrowseKillRing'
(autoload 'browse-kill-ring "browse-kill-ring" "Browse the `kill-ring'." t)

(after "browse-kill-ring"
  (browse-kill-ring-default-keybindings))

;;; IMPORTANT: gist
;; SOURCE: `https://github.com/defunkt/gist.el'
(autoload 'gist-buffer "gist" "Integrate with Github." t)

;;; IMPORTANT: git integration
;; SOURCE: `http://www.emacswiki.org/emacs/Magit'
(autoload 'magit-status "magit" "Version control with Git." t) ;; NOTE: magit for use with github

(after "magit"
  (setq magit-save-some-buffers t ;; NOTE: ask me to save buffers before running magit-status

	magit-process-popup-time 4) ;; NOTE: popup the process buffer if command takes too long

  ;; NOTE: full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;;; IMPORTANT: undo tree
;; SOURCE: `http://www.emacswiki.org/emacs/UndoTree'
(autoload 'global-undo-tree-mode "undo-tree" "Visualize the current buffer's undo tree." t)

;; NOTE: persistent undo history
;; (after "undo-tree"
;;   (setq undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist `((".*" . ,(concat user-emacs-directory "undo")))))

(global-undo-tree-mode) ;; NOTE: enable undo-tree mode

;;; IMPORTANT: emacs relay chat
;; SOURCE: `http://emacswiki.org/emacs/ERC'
;; SOURCE: `http://www.emacswiki.org/emacs/ErcSSL'
(autoload 'erc-tls "erc" "" t) ;; NOTE: this is to use SSL

(after "erc"
  (require 'tls)
  (require 'erc-spelling)
  ;; (require 'erc-hl-nicks)
  ;; (require 'erc-stamp)
  ;; (require 'erc-join)
  ;; (require 'erc-netsplit)
  ;; (require 'erc-ring)
  ;; (require 'erc-goodies)
  ;; (require 'erc-track)
  ;; (require 'erc-button)
  ;; (require 'erc-match)
  ;; (require 'erc-fill)
  ;; (require 'erc-log)
  ;; (require 'erc-pcomplete)
  ;; (require 'erc-notify)
  ;; (require 'erc-capab) ;; TODO: investigate `capab-identity'
  ;; (require 'erc-goodies)

  ;; IMPORTANT: erc modules
  (erc-spelling-enable)
  ;; (erc-button-enable)
  ;; (erc-ring-enable)
  ;; (erc-netsplit-enable)
  ;; (erc-match-enable)
  ;; (erc-autojoin-enable)
  ;; (erc-scrolltobottom-enable)
  ;; (erc-hl-nicks-enable)
  ;; (erc-timestamp-mode t)
  ;; (erc-fill-disable)

  ;; IMPORTANT: erc capab
  ;; (eval-after-load "erc-capab" '(erc-capab-identify-mode t))

  ;; IMPORTANT: erc match
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcMatch'
  (setq erc-keywords '() ;; NOTE: highlight specific keywords
        erc-current-nick-highlight-type 'nick ;; NOTE: ...
        erc-pal-highlight-type 'nick ;; NOTE: nicknames in a message
        erc-fool-highlight-type 'all ;; NOTE: highlight entire message
        erc-pals '(;;"twb"
                   ;;"k-man"
                   ;;"macrobat"
                   ;;"tali713"
                   ;; "syrinx"
                   ;;"sabetts"
                   ;; "rww"
                   ;;"dax"
                   ;; "LjL"
                   ;; "ldunn"
                   ;;"moocow"
                   ;; "mc44"
                   ;; "IdleOne"
                   ;;"jussi"
                   ;;"topyli"
                   ) ;; NOTE: highlight pals
        erc-fools '("ubottu" "floodBot1" "floodBot2" "floodBot3" "fsbot" "rudybot" "birny" "lisppaste" "ubnotu") ;; NOTE: highlight fools
        erc-dangerous-hosts '()) ;; NOTE: mark any dangerous hosts

  (remove-hook 'erc-text-matched-hook 'erc-hide-fools) ;; NOTE: keep messages from `erc-fools'

  ;;; IMPORTANT: erc autoaway
  ;; (require 'erc-autoaway)

  ;; IMPORTANT: erc notify
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcNotify'
  ;;(setq erc-notify-list erc-pals)
  ;;(erc-notify-mode t)

  ;; IMPORTANT: erc logging
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcLogging'
  (require 'erc-log)

  (setq erc-log-channels-directory "~/.emacs.d/erc/logs/" ;; FIX: hard-coded ...
        erc-save-buffer-on-part t ;; NOTE: save log file automatically when parting or quitting a channel
        erc-save-queries-on-quit t
        erc-log-write-after-send t
        erc-log-write-after-insert t
        ;;erc-log-insert-log-on-open t
        erc-log-file-coding-system 'utf-8)

  (erc-log-enable)

  ;; IMPORTANT: erc ignore
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcIgnoring'
  ;; (setq-default erc-ignore-list '())

  ;; IMPORTANT: erc completion
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcCompletion'
  ;; (require 'erc-pcomplete)

  ;; (erc-pcomplete-enable)

  ;; IMPORTANT: erc user variables
  ;; TODO: use variables in here ...
  (setq erc-nick (getenv "USER")
        erc-nick-uniquifier "_"
        erc-server "irc.freenode.net" ;; NOTE: freenode IRC server
        ;; erc-user-full-name user-full-name
        ;; erc-email-userid user-mail-address
        ;; erc-fill-column 90
        erc-format-nick-function 'erc-format-@nick
        erc-port 7000 ;; NOTE: `erc-tls' port (for ssl)
        erc-current-nick-highlight-type 'all ;; NOTE: highlight the entire message where current nickname occurs
        erc-button-google-url "http://www.google.com/search?q=%s"
        erc-fill-prefix "       " ;; NOTE: ... prefix column on the left (same size as the `timestamp-format' variable above)
        erc-fill-mode nil ;; NOTE: again, disable ERC fill (not sure why I have done it in multiple places)
        erc-timestamp-format "[%H:%M] " ;; NOTE: put timestamps on the left
        erc-timestamp-right-column 61
        erc-timestamp-only-if-changed-flag nil ;; NOTE: always show timestamp
        erc-insert-timestamp-function 'erc-insert-timestamp-left ;; NOTE: insert timestamp in the left column
        erc-track-showcount t ;; NOTE: show count of unseen messages
        erc-kill-buffer-on-part t ;; NOTE: kill buffers for channels after /part
        erc-kill-queries-on-quit t ;; NOTE: kill buffers for queries after quitting the server
        erc-kill-server-buffer-on-quit t ;; NOTE: kill buffers for server messages after quitting the server
        erc-interpret-mirc-color t ;; NOTE: interpret mIRC-style colour commands in IRC chats
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477") ;; NOTE: do not track these messages
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT") ;; NOTE: ignore JOIN, NICK, PART and QUIT messages
        ;; erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-mode-line-format "%t %a" ;; NOTE: display only the channel name on the mode-line
        erc-header-line-format nil ;; NOTE: turn off the topic (header) bar
	;; erc-header-line-uses-tabbar-p t ;; TEST: ...
        header-line-format nil ;; NOTE: turn off the topic (header) bar
        erc-input-line-position -1 ;; NOTE: keep input at the last line
        erc-max-buffer-size 20000 ;; NOTE: truncate buffers (so they don't hog core)
        erc-truncate-buffer-on-save t
        erc-prompt ;; NOTE: channel specific prompt ...
        (lambda () (if (and (boundp 'erc-default-recipients) (erc-default-target))
                       (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
                     (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
        erc-join-buffer 'bury
        erc-autojoin-channels-alist '((".*\\.freenode.net"
                                       "#emacs"
                                       "#stumpwm"
                                       ;; "#lisp"
				       ;; "#gnus"
				       ;; "#org-mode"
				       ;; "#ubuntu-fr-offtopic"
                                       ;; "#debian-offtopic"
				       ;; ---
                                       "#ubuntu-offtopic"
				       "#ubuntu-irc"
                                       "#ubuntu-ops"
                                       "#ubuntu-ops-team"
                                       )))

  ;; (defun erc-disable-auto-fill-mode ()
  ;;   ""
  ;;   (auto-fill-mode 0))

  ;;(add-hook 'erc-mode-hook 'erc-disable-auto-fill-mode)
  (setq erc-modules (delq 'fill erc-modules)) ;; NOTE: disable `erc-fill-mode'

  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)

  (setq erc-remove-parsed-property nil))

;;; IMPORTANT: irssi-like /window command
;; TODO: this is horrible
(defun erc-cmd-WINDOW (window)
  "..."
  (let* ((number 0)
	 (channels (mapcar #'(lambda (element) `(,(incf number) . ,element))
			   (reverse (cdr (car erc-autojoin-channels-alist))))))
    (switch-to-buffer (cdr (assoc window channels)))))

(after "erc-goodies"
  (erc-scrolltobottom-enable)

  (add-to-list 'erc-noncommands-list 'erc-cmd-SHOW)
  (add-to-list 'erc-noncommands-list 'erc-cmd-MAN)
  (add-to-list 'erc-noncommands-list 'erc-cmd-WOMAN)
  ;;(add-to-list 'erc-noncommands-list 'erc-cmd-WINDOW)
  )

;;; IMPORTANT: conference mode
;; ERROR: this doesn't work
;; (after "erc"
;;   (defvar erc-conference-p nil "If non-nil disable conference mode. If nil enable conference mode.")
;;   ;; (setq erc-conference-p nil)

;;   (defun erc-cmd-CONFERENCE (&optional force)
;;     "Function for enabling/disabling user JOIN, PART and QUIT messages."
;;     (if (and (boundp 'erc-conference-p) erc-conference-p)
;; 	(progn
;; 	  (setq erc-conference-p nil
;; 		erc-hide-list (default-value 'erc-hide-list))
;; 	  (erc-display-line (erc-make-notice "Conference mode disabled.") 'active))
;;       (progn
;;         (make-local-variable 'erc-hide-list)
;;         (make-local-variable 'erc-conference-p) 
;;         (setq erc-conference-p t
;;               erc-hide-list '("JOIN" "PART" "QUIT"))
;;         (erc-display-line (erc-make-notice "Conference mode enabled.") 'active)))
;;     t))

(defun erc-enable-conference-mode ()
  (interactive)
  (setq erc-hide-list (quote ("JOIN" "QUIT" "MODE")))
  (setq erc-minibuffer-ignored t)
  (message "Now ignoring JOINs, QUITs, and MODEs"))

(defun erc-disable-conference-mode ()
  (interactive)
  (setq erc-hide-list nil)
  (setq erc-minibuffer-ignored nil)
  (message "Now showing everything"))

;;; IMPORTANT: erc commands
(defun erc-cmd-ACCESSLIST ()
  "Display the `access-list' for the current channel."
  (erc-message "PRIVMSG"
	       (format "ChanServ ACCESS %s LIST"
		       (erc-default-target))))

;; SOURCE: `http://www.emacswiki.org/emacs/ErcUname'
(defun erc-cmd-UNAME (&rest ignore)
  "Display the result of running `uname -a' to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          "[ \n]+$" "" (shell-command-to-string "uname -a"))))
    (erc-send-message
     (concat "{uname -a} [" uname-output "]"))))

;; SOURCE: `http://www.emacswiki.org/emacs/ErcUptime'
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related information, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
	  ", load average: " "] {Load average} ["
	  ;; NOTE: collapse spaces, remove
	  (replace-regexp-in-string
	   " +" " "
	   ;; NOTE: remove beginning and trailing whitespace
	   (replace-regexp-in-string
	    "^ +\\|[ \n]+$" ""
	    (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

;; SOURCE: `http://www.emacswiki.org/emacs/ErcShow'
(defun erc-cmd-SHOW (&rest form)
  "Evaluate FORM and send the result and the original form as: FORM => (eval FORM)."
  (let ((string
         (with-temp-buffer
           (mapc #'(lambda (f) (insert f " ")) form)
           (goto-char (point-min))
           (setq form (read (current-buffer)))
           (let ((res (condition-case err
                          (eval form)
                        (error
                         (format "Error: %s" err)))))
             (insert (format " => %s" res)))
           (buffer-substring-no-properties
            (point-min) (1- (point-max))))))
    (erc-send-message string)))

(defun erc-cmd-HOWMANY (&rest ignore)
  "Display how many users (and ops) the current channel has."
  (erc-display-message
   nil 'notice (current-buffer)
   (let ((hash-table (with-current-buffer (erc-server-buffer) erc-server-users))
	 (users 0)
	 (ops 0))
     (maphash (lambda (k v)
		(when (member (current-buffer) (erc-server-user-buffers v))
		  (incf users))
		(when (erc-channel-user-op-p k)
		  (incf ops)))
	      hash-table)
     (format "There are %s users (%s ops) in the current channel" users ops))))

;; SOURCE: `http://www.emacswiki.org/emacs/ErcChanop'
(defun erc-cmd-OPME ()
  "Request ChanServ to put me into operator status."
  (erc-message "PRIVMSG"
	       (format "ChanServ OP %s %s"
		       (erc-default-target)
		       (erc-current-nick)) nil))

;; SOURCE: `http://www.emacswiki.org/emacs/ErcChanop'
(defun erc-cmd-DEOPME ()
  "Deop myself from current channel."
  (erc-cmd-DEOP
   (format "%s"
	   (erc-current-nick))))

;; SOURCE: `http://www.emacswiki.org/emacs/EmacsChannelMaintenance'
;; (defun erc-cmd-BAN (nick)
;;   "..."
;;   (let* ((chan (erc-default-target))
;;          (who (erc-get-server-user nick))
;;          (host (erc-server-user-host who))
;;          (user (erc-server-user-login who)))
;;     (erc-send-command (format "MODE %s +b *!%s@%s" chan user host))))

;; (defun erc-cmd-UNBAN (nick)
;;   "..."
;;   (let* ((chan (erc-default-target))
;; 	 (who (erc-get-server-user nick))
;; 	 (host (erc-server-user-host who))
;; 	 (user (erc-server-user-login who)))
;;     (erc-send-command (format "MODE %s -b *!%s@%s" chan user host))))

(defun erc-cmd-BAN (nick)
  "Ban user NICK from channel specified by `erc-default-target'."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick))
	 (host (erc-server-user-host who))
	 ;(user (erc-server-user-login who))
	 )
    (erc-send-command (format "MODE %s +b *!*@%s" chan host))))

(defun erc-cmd-UNBAN (nick)
  "Unban user NICK from channel specified by `erc-default-target'."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick))
	 (host (erc-server-user-host who))
	 ;(user (erc-server-user-login who))
	 )
    (erc-send-command (format "MODE %s -b *!*@%s" chan host))))

;; (defun erc-cmd-MUTE (nick)
;;   (let ((chan (erc-default-target))
;; 	(host (erc-get-server-user nick)))
;;     (erc-send-command (format "QUIET %s *!*@%s" chan host))))

;; (defun erc-cmd-UNMUTE (nick)
;;   (let ((chan (erc-default-target))
;; 	(who (erc-get-server-user nick)))
;;     (erc-send-command (format "UNQUIET %s *!*@%s" chan host))))

(defun erc-cmd-KICK (nick)
  "Kick NICK from channel."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick)))
    (erc-send-command (format "KICK %s %s (Kicked)" chan nick))))

(defun erc-cmd-REMOVE (nick)
  "Remove user NICK from current ERC channel."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick)))
    (erc-send-command (format "REMOVE %s %s Removed" chan nick))))

(defun erc-cmd-BANREMOVE (nick)
  "Remove and ban user NICK from current ERC channel."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick)))
    ;;(erc-cmd-OPME)
    ;;(sit-for 1)
    (erc-cmd-REMOVE nick)
    (erc-cmd-BAN nick)
    ;;(erc-cmd-DEOPME)
    ))

(defvar *greetings-list* nil "List of welcoming greetings.")

(setq *greetings-list* '("Hello"
			 "Hi"
			 "Howdy"
			 "Greetings"
			 "G'day"
			 "Hey"
			 "Sup"
			 "Good morning"
			 "Good afternoon"
			 "Good evening"))

(defun erc-cmd-GREETING (nick)
  "Welcome user NICK with a random greeting."
  (let ((greet (nth (random (length *greetings-list*)) *greetings-list*)))
    (erc-send-message (format "%s %s" greet nick))))

;;; IMPORTANT: macros for "custom" ERC commands
(defmacro erc-user-message (command message)
  "Macro to create \"custom\" messages to an IRC user in an `erc-mode' session."
  (let ((func (intern (concat "erc-cmd-" command)))
	(doc (format "Send the command \"%s\" in an `erc-mode' buffer." command))
	(string message))
    `(defun ,func (name &rest junk)
       ,doc
       (erc-send-message (concat name ": " ,string)))))

(defmacro erc-user-action (action verb message)
  "Macro to create \"custom\" actions to an IRC user in an `erc-mode' session."
  (let ((func (intern (concat "erc-cmd-" action)))
	(doc (format "Send the action \"%s\" in an `erc-mode' buffer." action))
	(string message))
    `(defun ,func (name &rest junk)
       ,doc
       (erc-send-action (erc-default-target)
			(concat ,verb " " name " " ,string)))))

(erc-user-message "NICKSERV" "Freenode's NickServ allows a user to register a nickname. See: /msg NickServ help")
(erc-user-message "MEMOSERV" "Freenode's MemoServ allows a user to send messages to registered users. See: /msg MemoServ help")
(erc-user-message "CHANSERV" "Freenode's ChanServ gives normal users the ability to maintain control of a channel. See: /msg ChanServ help")
(erc-user-message "GUIDELINES" "The guidelines for using the Ubuntu channels can be found here: http://wiki.ubuntu.com/IRC/Guidelines")
(erc-user-message "LANGUAGE" "Please watch your language in this channel, thank you.")
(erc-user-message "EMACS" "GNU Emacs is a powerful lisp environment and text editor. See: http://www.gnu.org/software/emacs/")
(erc-user-message "STUMPWM" "StumpWM is a tiling window manager for X11 written in common lisp. See: http://www.nongnu.org/stumpwm/")
;;(erc-user-message "CONKEROR" "Conkeror is a highly extensible web browser based on Firefox. See: http://conkeror.org/")
(erc-user-message "ORGMODE" "Org-mode is for keeping notes, maintaining TODO lists, project planning, and writing. See: http://orgmode.org/")

(defun erc-cmd-GENTLEMEN ()
  "Send calm down message."
  (erc-send-message "Gentlemen, you can't fight here. This is the war room!"))

;; SOURCE: `fsbot' in #emacs
(erc-user-action "GNU" "takes" "aside and explains why GNU/Linux is the proper term for the operating system commonly referred to as Linux. See: http://www.gnu.org/gnu/linux-and-gnu.html")

;; IMPORTANT: freenode <*>Serv interaction commands
;; SOURCE: `http://keramida.wordpress.com/2008/11/04/extending-erc-with-emacs-lisp/'
(defun erc-cmd-CS (&rest args)
  "Short alias for `/chanserv ARGS'."
  (let ((command-args (append (list "CHANSERV") args)))
    (let ((chanserv-command (mapconcat #'identity command-args " ")))
      (erc-send-command chanserv-command))))

(defun erc-cmd-MS (&rest args)
  "Short alias for `/memoserv ARGS'."
  (let ((command-args (append (list "MEMOSERV") args)))
    (let ((memoserv-command (mapconcat #'identity command-args " ")))
      (erc-send-command memoserv-command))))

(defun erc-cmd-NS (&rest args)
  "Short alias for `/nickserv ARGS'."
  (let ((command-args (append (list "NICKSERV") args)))
    (let ((nickserv-command (mapconcat #'identity command-args " ")))
      (erc-send-command nickserv-command))))

;; NOTE: Freenode `NickServ' commands:
;; GHOST           Reclaims use of a nickname.
;; GROUP           Adds a nickname to your account.
;; IDENTIFY        Identifies to services for a nickname.
;; INFO            Displays information on registrations.
;; LISTCHANS       Lists channels that you have access to.
;; REGISTER        Registers a nickname.
;; RELEASE         Releases a services enforcer.
;; SET             Sets various control flags.
;; UNGROUP         Removes a nickname from your account.
;;  
;; Other commands: ACC, ACCESS, CERT, DROP, HELP, LISTOWNMAIL, 
;;                 LOGOUT, REGAIN, SETPASS, STATUS, TAXONOMY, 
;;                 VACATION, VERIFY

(defvar nickserv-commands-list (list "GHOST" "GROUP" "IDENTIFY" "INFO" "LISTCHANS" "REGISTER" "RELEASE" "SET" "UNGROUP" "ACC" "ACCESS" "CERT" "DROP" "HELP" "LISTOWNMAIL" "LOGOUT" "REGAIN" "SETPASS" "STATUS" "TAXONOMY" "VACATION" "VERIFY") "List of Freenode's `NickServ' commands.")

;; (defmacro erc-user-cmd (command)
;;   "..."
;;   (let ((func (insert (concat "erc-cmd-" command)))
;; 	(doc (format "Send the command \"%s\" to a server process in an `erc-mode' buffer." command)))
;;     `(defun ,func (&rest junk)
;;        ,doc
;;        (erc-message "PRIVMSG" (format "%s help" ,command)))))

;;(erc-user-cmd "NICKSERV")

;; (defun erc-cmd-NS ()
;;   ""
;;   (let ((choice (ido-completing-read "Select command: " nickserv-commands-list)))
;;     (erc-message "PRIVMSG"
;;                  (format "NickServ help %s"
;;                          choice))))

;; IMPORTANT: freenode user and channel modes
;; (defvar freenode-user-modes-list (list "D" "g" "i" "Q" "R" "w" "z"))
;; (defvar freenode-channel-modes-list (list "b" "C" "c" "e" "f" "F" "g" "i" "I" "j" "k" "l" "L" "m" "n" "p" "P" "q" "Q" "r" "s" "t" "z"))

;; (defun user-mode-command (user-flag)
;;   "..."
;;   )

;; (defun channel-mode-command (mode-flag &optional user)
;;   "..."
;;   (let ((channel (erc-default-target)))
;;     (erc-server-send (concat "MODE " channel mode-flag user))
;;     ))

;; NOTE: Freenode `MemoServ' commands:
;; DEL             Alias for DELETE
;; DELETE          Deletes memos.
;; FORWARD         Forwards a memo.
;; HELP            Displays contextual help information.
;; IGNORE          Ignores memos.
;; LIST            Lists all of your memos.
;; READ            Reads a memo.
;; SEND            Sends a memo to a user.
;; SENDOPS         Sends a memo to all ops on a channel.

(defvar memoserv-commands-list (list "DEL" "DELETE" "FORWARD" "HELP" "IGNORE" "LIST" "READ" "SEND" "SENDOPS") "List of Freenode's `MemoServ' commands.")

;; (defun erc-cmd-MS (&rest junk)
;;   "Send `MemoServ' command to server process in an `erc-mode' buffer."
;;   (let ((choice (ido-completing-read "Select command: " memoserv-commands-list)))
;;     (erc-message "PRIVMSG" (concat "MemoServ " choice " help") nil)))

;; TODO: add a bunch of commands to automagically handle ChanServ stuff
;; ...
;; (defun erc-cmd-CS (&rest junk)
;;   )

;;; IMPORTANT: "Custom" `erc-mode' interactions with outside environment
(defun erc-cmd-MAN (program &rest args)
  "Open the `man' page for PROGRAM."
  (man program))

(defun erc-cmd-WOMAN (program &rest args)
  "Open the `woman' page for PROGRAM."
  (woman program))

;; IMPORTANT: when connecting ask me for a password
(defun erc-tls-connect-server (server &rest junk)
  "Ask for a password before connecting to SERVER."
  (let ((password (read-passwd "Enter IRC Password: ")))
    (erc-tls :server server :port erc-port :nick erc-nick :password password)))

;; IMPORTANT: this is where the ERC settings are activated
(defun erc-start-or-switch (&rest junk)
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (save-excursion
    (if (get-buffer "irc.freenode.net:7000") ;; NOTE: if ERC is already active ...
	;; TODO: if connected to IRC and there has been no activity, execute `custom-erc-switch-buffer'
	(erc-track-switch-buffer 1) ;; NOTE: ... switch to last active buffer ...
      (erc-tls-connect-server "irc.freenode.net")))) ;; NOTE: ... else, start an `erc-tls' session on `irc.freenode.net'

(defvar custom-erc-channel-list nil "List of channels to connect to.")

(setq custom-erc-channel-list ;; NOTE: currently this is hard-coded for "freenode" convert this to an alist
      (list "#ubuntu"
	    "#ubuntu+1"
	    "#ubuntu-server"
	    "#ubuntu-au"
	    "#ubuntu-au-chat"
	    "#ubuntu-offtopic"
	    "#ubuntu-discuss"
	    "#ubuntu-irc"
	    "#ubuntu-programming"
	    "#ubuntu-bots-devel"
	    "#ubuntu-bots"
	    "#ubuntu-app-devel"
	    "#ubuntu-devel"
	    "#ubuntu-bugs"
	    "#ubuntuforums"
	    "#ubuntu-ops"
	    "#ubuntu-ops-team"
	    "#ubuntu-release-party"
	    "#ubuntu-classroom"
	    "#ubuntu-classroom-chat"
	    "#ubuntu-fr"
	    "#ubuntu-fr-offtopic"
	    "#freenode"
            "#bash"
            "#gnus"
	    "#hurd"
            "#sbcl"
	    "#debian"
	    "#debian-offtopic"
	    "#emacs"
	    "#org-mode"
	    "#stumpwm"
	    "#conkeror"
	    "#screen"
	    "#irssi"
	    "#lisp"
	    "#scheme"
	    "#guile"
            "#clojure"
	    "#haskell"
	    "#latex"
            ;; social channels ...
	    "#reddit"
            "#anucssa"
	    "#defocus"
	    "##club-ubuntu"
	    "##math"
	    "##programming"
	    "##economics"
            "##linguistics"
	    "##philosophy"))

(defun custom-erc-join-channel (&rest junk)
  "Select a channel from a list of channels to join.

NOTE: This is currently hard-coded to strictly use channels on \"irc.freenode.net\"."
  (interactive)
  (let ((channel (ido-completing-read "Enter channel: " custom-erc-channel-list)))
    (if (get-buffer channel) ;; TODO: check to see if channel is already a open channel ...
	(switch-to-buffer channel)) ;; NOTE: ... and if so, just switch to buffer
    (erc-cmd-JOIN channel))) ;; NOTE: need to be in an existing ERC session for this command to work

(defun custom-erc-switch-buffer (&rest junk)
  "Switch to an existing `erc-mode' buffer."
  (interactive)
  (when (get-buffer "irc.freenode.net:7000")
    (switch-to-buffer
     (ido-completing-read
      "Switch to ERC channel: " 
      (save-excursion
	(delq nil (mapcar (lambda (buf)
			    (when (buffer-live-p buf)
			      (with-current-buffer buf
				(and (eq major-mode 'erc-mode)
				     (buffer-name buf)))))
			  (buffer-list))))))))

;; NOTE: `erc' key-bindings
(after "erc"
  (define-key erc-mode-map (kbd "C-c b") 'custom-erc-join-channel))

;;; IMPORTANT: gnus
;; SOURCE: `http://emacswiki.org/emacs/CategoryGnus'
;; SOURCE: `http://emacswiki.org/emacs/Gnus'
(autoload 'gnus "gnus" "Read mail and news with GNU Emacs." t)

;; TODO:
;; if: there is no file at ~/.authinfo
;; then: create new file ~/.authinfo generating the file:
;;  machine imap.gmail.com login user-primary-email-address port 993
;;  machine smtp.gmail.com login user-primary-email-address port 587
;; else: 
;; 1. use that information (i.e. start gnus)
;; 2. re-write the file to disk (i.e. something has changed)

(after "gnus"
  (require 'smtpmail)
  ;; (require 'nnimap)
  ;; (require 'starttls)

  ;; IMPORTANT: encryption
  ;; SOURCE: `http://emacswiki.org/emacs/EasyPG'
  ;; TODO: configuration encryption

  ;; IMPORTANT: personal settings
  (setq user-mail-address user-primary-email-address ;; NOTE: user primary email address
        ;; user-mail-address "mathew.ball@gmail.com" ;; NOTE: user mail address
        ;; user-full-name "Matthew Ball" ;; NOTE: user full-name
	gnus-use-full-window nil ;; NOTE: don't ruin my frame!
        mail-aliases t ;; NOTE: enable mail aliases (NOTE: uses `mail-personal-alias-file'
        auth-source-save-behavior nil
        gnus-inhibit-startup-message t
        gnus-agent-expire-all t  ;; NOTE: allow uncaching of unread articles
        gnus-agent-article-alist-save-format 2 ;; NOTE: compress cache
        ;; mail-personal-alias-file "~/.conf-scripts/mailrc" ;; NOTE: change directory where mail aliases are located
        ;; nnimap-authinfo-file "~/.conf-scripts/passwords/authinfo" ;; NOTE: change directory where authentication information is found
        message-from-style 'angles ;; NOTE: specifies how the "From" header appears
        read-mail-command 'gnus
        message-send-mail-function 'smtpmail-send-it ;; NOTE: for gnus (message-mode)
        send-mail-function 'smtpmail-send-it) ;; NOTE: not for gnus (mail-mode)

  ;; TODO: can these be set in `general-config.el' (???)
  (setq custom-mail-dir (expand-file-name user-mail-directory)) ;; NOTE: set directory for mail
  (setq custom-news-dir (expand-file-name user-news-directory)) ;; NOTE: set directory for news

  ;; (setq custom-mail-dir "~/Mail/") ;; NOTE: set directory for mail
  ;; (setq custom-news-dir "~/News/") ;; NOTE: set directory for news

  ;; IMPORTANT: gnus settings
  (setq gnus-select-method '(nnml "")
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\(\\|$\\)\\|^[\"]\"[#'()]"
        gnus-invalid-group-regexp "[:`'\"]\\|^$"
        gnus-permanently-visible-groups "mail"
        gnus-thread-hide-subtree t
        gnus-fetch-old-headers t
        gnus-thread-ignore-subject t
        gnus-always-read-dribble-file t ;; NOTE: don't bugger me with dribbles
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;; NOTE: threads
        gnus-posting-styles '((".*" (name "Matthew Ball")) ;; TODO: change email addresses
                              ("gmail" (address "mathew.ball@gmail.com"))
                              ("anumail" (address "u4537508@anu.edu.au"))))

  (setq gnus-check-new-newsgroups nil ;; NOTE: suppress checking for new groups
        gnus-save-newsrc-file nil ;; NOTE: turn off writing the `.newsrc' file
        gnus-read-newsrc-file nil ;; NOTE: ignore the `.newsrc' file
        gnus-interactive-exit nil
        gnus-save-killed-list nil ;; NOTE: do not save a list of killed groups to startup file
        )

  (setq message-kill-buffer-on-exit t) ;; NOTE: kill the mail buffer after sending message

  ;; IMPORTANT: visible headers
  (setq gnus-visible-headers
        (concat "^From:\\|^Subject:\\|^Newsgroups:"
                "\\|^Organization:"
                "\\|^To:\\|^Cc:\\|^Date:"))

  ;; IMPORTANT: imap setup
  (setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p" ;; NOTE: set ssl
        imap-log t ;; NOTE: log the imap session
        imap-store-password t ;; NOTE: store the session password
        gnus-secondary-select-methods
        '((nnimap "gmail" ;; NOTE: gmail login
                  (nnimap-address "imap.gmail.com") ;; NOTE: being the "gmail" account, this hard-coding is ok?
                  (nnimap-server-port 993)
                  (nnimap-authinfo-file "~/.authinfo")
                  (nnimap-authenticator login)
                  (nnimap-expunge-on-close 'never)
                  (nnimap-stream ssl))
          ;; (nnimap "anumail" ;; NOTE: anumail login (ERROR: this does not work)
          ;; 	(nnimap-address "anumail.anu.edu.au")
          ;; 	(nnimap-server-port 993)
          ;; 	;; (nnimap-authinfo-file "~/.authinfo")
          ;; 	;; (nnimap-authenticator login)
          ;; 	;; (nnimap-expunge-on-close 'never)
          ;; 	(nnimap-stream ssl))
          ))


  ;; IMPORTANT: smtp setup (single account)
  (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "mathew.ball@gmail.com" nil))) ;; TODO: replace email address

  ;; IMPORTANT: smtp setup (multiple accounts) (ERROR: this does not work)
  ;; (defvar smtp-accounts ;; available smtp accounts
  ;;   '((ssl "mathew.ball@gmail.com" "smtp.gmail.com" 587 "key" nil)
  ;;     (ssl "u4537508@anu.edu.au.com" "smtphost.anu.edu.au" 465 "key" nil)))

  ;; (setq starttls-use-gnutls t
  ;;       starttls-gnutls-program "gnutls-cli"
  ;;       starttls-extra-arguments '("--insecure"))

  ;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  ;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "mathew.ball@gmail.com" nil))
  ;;       smtpmail-default-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-service 587
  ;;       ;; smtpmail-local-domain "mail.bigpond.com"
  ;;       smtpmail-debug-verb t
  ;;       smtpmail-debug-info t) ;; to debug

  ;; (defun set-smtp-plain (server port)
  ;;   "Set related SMTP variables for supplied parameters."
  ;;   (setq smtpmail-smtp-server server
  ;; 	smtpmail-smtp-service port
  ;; 	;; smtpmail-auth-credentials "~/.authinfo" ;; I have not set this up
  ;; 	smtpmail-starttls-credentials nil)
  ;;   (message "Setting SMTP server to `%s:%s'."
  ;; 	    server port address))

  ;; (defun set-smtp-ssl (server port key cert)
  ;;   "Set related SMTP and SSL variables for supplied parameters."
  ;;   (setq starttls-use-gnutls t
  ;; 	starttls-gnutls-program "gnutls-cli"
  ;; 	starttls-extra-arguments nil
  ;; 	smtpmail-smtp-server server
  ;; 	smtpmail-smtp-service port
  ;; 	smtpmail-starttls-credentials (list (list server port key cert))
  ;; 	;; smtpmail-auth-credentials "~/.authinfo" ;; I have not set this up
  ;; 	)
  ;;   (message "Setting SMTP server to `%s:%s' (SSL enabled)."
  ;; 	   server port address))

  ;; (defun change-smtp ()
  ;;   "Change the SMTP server according to the current from line."
  ;;   (save-excursion
  ;;     (loop with from = (save-restriction
  ;; 			(message-narrow-to-headers)
  ;; 			(message-fetch-field "from"))
  ;; 	  for (acc-type address . auth-spec) in smtp-accounts
  ;; 	  when (string-match address from)
  ;; 	  do (cond
  ;; 	      ((eql acc-type 'plain)
  ;; 	       (return (apply 'set-smtp-plain auth-spec)))
  ;; 	      ((eql acc-type 'ssl)
  ;; 	       (return (apply 'set-smtp-ssl auth-spec)))
  ;; 	      (t (error "Unrecognized SMTP account type: `%s'." acc-type)))
  ;; 	  finally (error "Cannot interfere SMTP information."))))

  ;; IMPORTANT: email config
  ;; (add-hook 'message-send-hook 'change-smtp) ;; change smtp server appropriately
  ;; (add-hook 'message-mode-hook (function (lambda () (local-set-key (kbd "<tab>") 'bbdb-complete-name)))) ;; NOTE: add tab completion to name in the "To:" field

  ;; (remove-hook 'gnus-summary-prepare-exit-hook
  ;; 	     'gnus-summary-expire-articles)

  ;; IMPORTANT: html display
  (setq mm-text-html-renderer 'w3m)
  (setq mm-inline-text-html-with-images t)
  (setq mm-inline-text-html-with-w3m-keymap nil)

  ;; IMPORTANT: mode-line
  ;; (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n") ;; NOTE: set mode-line

  ;; IMPORTANT: rss config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ;; NOTE: topic mode - tree view - is always active

  ;; (eval-after-load "gnus-sum" ;; NOTE: set the default value of mm-discouraged-alternatives
  ;;   '(add-to-list 'gnus-newsgroup-variables '(mm-discouraged-alternatives . '("text/html" "image/.*"))))

  ;; NOTE: display 'text/html' parts in nnrss groups
  ;; (add-to-list 'gnus-parameters '("\\`nnrss:" (mm-discouraged-alternatives nil)))

  ;; IMPORTANT: gnus parameters
  ;; (setq gnus-parameters
  ;;       '(("mail\\..*"
  ;; 	 (gnus-show-threads nil)
  ;; 	 (gnus-use-scoring nil)
  ;; 	 (gnus-summary-line-format "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
  ;; 	 (gcc-self . t)
  ;; 	 (display . all))

  ;; 	("^nnimap:\\(foo.bar\\)$"
  ;; 	 (to-group . "\\1"))

  ;; 	("mail\\.me"
  ;; 	 (gnus-use-scoring t))

  ;; 	("list\\..*"
  ;; 	 (total-expire . t)
  ;; 	 (broken-reply-to . t))))

  ;; (add-hook 'gnus-summary-mode-hook
  ;;           (lambda () (when (string-match "^nnrss:.*" gnus-newsgroup-name)
  ;; 		  (progn
  ;; 		    (make-local-variable 'gnus-show-threads)
  ;; 		    (make-local-variable 'gnus-article-sort-functions)
  ;; 		    (make-local-variable 'gnus-use-adaptive-scoring)
  ;; 		    (make-local-variable 'gnus-use-scoring)
  ;; 		    (make-local-variable 'gnus-score-find-score-files-function)
  ;; 		    (make-local-variable 'gnus-summary-line-format)
  ;; 		    (setq gnus-show-threads nil
  ;; 			  gnus-article-sort-functions 'gnus-article-sort-by-date
  ;; 			  gnus-use-adaptive-scoring nil
  ;; 			  gnus-use-scoring t
  ;; 			  gnus-score-find-score-files-function 'gnus-score-find-single)))))

  ;; (defun browse-nnrss-url (arg)
  ;;   "Browse RSS url."
  ;;   (interactive "p")
  ;;   (let ((url (assq nnrss-url-field (mail-header-extra (gnus-data-header (assq (gnus-summary-article-number) gnus-newsgroup-data))))))
  ;;     (if url
  ;; 	(browse-url (cdr url))
  ;;       (gnus-summary-scroll-up arg))))

  ;; (add-hook 'gnus-summary-mode-hook (lambda () (define-key gnus-summary-mode-map (kbd "C-<return>") 'browse-nnrss-url)))

  ;; (add-to-list 'nnmail-extra-headers nnrss-url-field)
  )

;;; IMPORTANT: auto-complete mode
;; SOURCE: `http://emacswiki.org/emacs/AutoComplete'
(require 'auto-complete)
;;(autoload 'auto-complete "auto-complete" "..." t)

(after "auto-complete"
  (require 'auto-complete-config)
  (ac-config-default)

  ;; (global-auto-complete-mode t)

  (setq ;;ac-auto-start nil ;; NOTE: start auto-complete after five characters (modified)
        ;;ac-ignore-case t ;; NOTE: always ignore case
	ac-expand-on-auto-complete t ;; NOTE: expand common portions
	ac-dwim nil ;; NOTE: get pop-ups with docs even if unique
        ;;ac-auto-show-menu t ;; NOTE: automatically show menu
  	;;ac-use-menu-map t ;; NOTE: use menu map
  	;;ac-trigger-key "TAB" ;; NOTE: use TAB for trigger
        ;;ac-source-yasnippet t
	)

  ;; (set-face-background 'ac-candidate-face "lightgray")
  ;; (set-face-underline 'ac-candidate-face "darkgray")
  ;; (set-face-background 'ac-selection-face "steelblue")

  (set-default 'ac-sources '(ac-source-features
			     ac-source-functions
			     ac-source-yasnippet
			     ac-source-variables
			     ac-source-symbols
			     ac-source-abbrev
			     ac-source-imenu
			     ac-source-dictionary
			     ac-source-words-in-buffer
			     ac-source-words-in-same-mode-buffers
			     ac-source-words-in-all-buffer)))

;;; IMPORTANT: emacs snippets
;; SOURCE: `http://www.emacswiki.org/emacs/Yasnippet'
(autoload 'yas-minor-mode "yasnippet" "Emacs snippets." t)
;;(require 'yasnippet)

(after "yasnippet"
  ;;(yas--initialize)
  ;;(yas-reload-all)
  ;;(yas-load-directory "~/.emacs.d/elpa/yasnippet-20130505.2115/snippets/")
  (yas-load-directory "~/.emacs.d/snippets/" t) ;; NOTE: use just-in-time

  ;; (add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
  (yas-global-mode)
  
  ;;(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)
  )

;; (defun yas-ido-expand () ;; NOTE: Completing point by some yasnippet key
;;   "Lets you select (and expand) a yasnippet key"
;;   (interactive)
;;   (let ((original-point (point)))
;;     (while (and
;;             (not (= (point) (point-min) ))
;;             (not
;;              (string-match "[[:space:]\n]" (char-to-string (char-before)))))
;;       (backward-word 1))
;;     (let* ((init-word (point))
;;            (word (buffer-substring init-word original-point))
;;            (list (yas-active-keys)))
;;       (goto-char original-point)
;;       (let ((key (remove-if-not
;;                   (lambda (s) (string-match (concat "^" word) s)) list)))
;;         (if (= (length key) 1)
;;             (setq key (pop key))
;;           (setq key (ido-completing-read "key: " list nil nil word)))
;;         (delete-char (- init-word original-point))
;;         (insert key)
;;         (yas-expand)))))

;;; IMPORTANT: smart completion
;; TODO: this guy probably needs to be generalised a bit more (though, he works for now)
(defalias 'smart-completion '(lambda () (if (fboundp 'auto-complete)
				       (auto-complete nil)
				     (dabbrev-expand nil))))

;;; IMPORTANT: smart tab
(defun smart-tab () ;; NOTE: implement a smarter TAB
  "This smart tab is minibuffer compliant: it acts as usual in the minibuffer.

If mark is active, indents region. Else if point is at the end ofa symbol, expands it. Else indents the current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (smart-completion))
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
          (smart-completion)
	(indent-for-tab-command)))))

;;; IMPORTANT: default browser
(setq browse-url-new-window-flag t
      browse-url-browser-function 'choose-browser ;; NOTE: ask which browser to use
      browse-url-generic-program (getenv "BROWSER")) ;; NOTE: use the system's $BROWSER environment variable

(defun choose-browser (url &rest junk) ;; NOTE: select which browser to use (i.e. internal or external)
  "Navigate a web browser to URL.

Although this is interactive, call this with \\[browse-url]."
  (interactive "sURL: ")
  (if (y-or-n-p "Use w3m web browser? ")
      (w3m-browse-url url t)
    (browse-url-generic url)))

(defvar *internet-search-urls* '("http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
				 "http://en.wikipedia.org/wiki/Special:Search?search="))

(defun search-internet (arg)
  "Searches the internet using the ARGth custom URL for the marked text.

If a region is not selected, prompts for the string to search on.

The prefix number ARG indicates the Search URL to use. By default the search URL at position 1 will be used."
  (interactive "p")

  ;; NOTE: some sanity check
  (if (> arg (length *internet-search-urls*))
      (error "There is no search URL defined at position %s." arg))

  (let ((query ;; NOTE: set the search query first
	 (if (region-active-p)
	     (buffer-substring (region-beginning) (region-end))
	   (read-from-minibuffer "Search: ")))

	;; NOTE: now get the base URL to use for the search
	(base-url (nth (1- arg) *internet-search-urls*)))

    ;; NOTE: add the query parameter
    (let ((url
	   (if (string-match "%s" base-url)
	       ;; NOTE: if the base URL has a %s embedded, then replace it ...
	       (replace-match query t t base-url)
	     ;; NOTE: ... else just append the query string at end of the URL
	     (concat base-url query))))
      
      (message "Search: %s @ %s" query url)
      ;; NOTE: browse the URL
      (browse-url url))))

;;; IMPORTANT: w3m
;; SOURCE: `http://www.emacswiki.org/emacs/emacs-w3m'
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMTabs'
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMHintsAndTips'
(setq w3m-key-binding 'info) ;; NOTE: this needs to be set before loading

(autoload 'w3m "w3m" "Browse the internet with w3m." t)

(after "w3m"
  (require 'w3m-cookie)
  (require 'w3m-lnum)
  (require 'w3m-filter)
  ;; (require 'w3m-antenna)
  (require 'w3m-ccl)

  ;; NOTE: w3m interface and cookies
  (w3m-lnum-mode 1) ;; NOTE: enable Conkeror-like numbered links

  ;; NOTE: w3m antenna
  ;; (w3m-antenna-mode 1)
  ;; (setq w3m-antenna-file (concat (expand-file-name user-emacs-directory) "w3m/antenna"))

  ;; NOTE: w3m filter
  ;; (w3m-filter-mode 1)

  (setq url-automatic-caching t
        ;; w3m-key-binding 'info
        w3m-home-page "www.emacswiki.org"
        ;; w3m-default-display-inline-images t ;; NOTE: display images by default
        w3m-use-toolbar nil
        w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8
        w3m-use-cookies t ;; NOTE: use cookies in w3m
        ;; w3m-default-directory (concat (expand-file-name user-emacs-directory) "w3m")
        ;; w3m-use-title-buffer-name t
        w3m-default-save-directory (concat (expand-file-name user-emacs-directory) "w3m")
        w3m-bookmark-file (concat (expand-file-name user-emacs-directory "w3m/bookmark.html"))
        w3m-arrived-file (concat (expand-file-name user-emacs-directory) "w3m/arrived")
        w3m-cookie-file (concat (expand-file-name user-emacs-directory) "w3m/cookie") ;; NOTE: save cookies to ~/.emacs.d/w3m/cookie
        w3m-cookie-accept-bad-cookies t
        w3m-cookie-accept-domains '("www.emacswiki.org"
                                    "www.google.com"
                                    "www.wikipedia.org"
                                    "www.github.com"
                                    "http://plato.stanford.edu"))

  ;; NOTE: `youtube-dl' and `mplayer'
  ;; TODO: this doesn't work just yet
  (defvar youtube-videos-directory nil "Directory location to save YouTube videos.")

  (setq youtube-videos-directory "~/Videos/youtube/")

  ;; IMPORTANT: w3m session
  ;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSession'
  ;; (require 'w3m-session)

  ;; (setq w3m-session-file "~/.emacs.d/w3m/session")

  (progn
    (unless (fboundp 'desktop)
      (require 'desktop))
    (add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))

    (add-hook 'w3m-mode-hook 'w3m-register-desktop-save)) ;; NOTE: add w3m-buffers to desktop-save

  ;; NOTE: w3m mode hooks
  (defun desktop-display (url)
    "Remove trailing whitespace is w3m buffers."
    (let ((buffer-read-only nil))
      (delete-trailing-whitespace)))

  (add-hook 'w3m-display-hook 'desktop-display)
  
  ;; (add-hook 'w3m-display-hook
  ;;           (lambda (url) ;; NOTE: remove trailing whitespace in w3m buffer
  ;;             (let ((buffer-read-only nil))
  ;;               (delete-trailing-whitespace))))

  ;; IMPORTANT: w3m search
  ;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSearch'
  (setq w3m-search-engine-alist
        '(("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
	  ("cliki" "http://www.cliki.net/site/search?query=%s" utf-8)
          ;; ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" utf-8)
          ("emacswiki" "http://www.google.com/cse?cx=004774160799092323420%%3A6-ff2s0o6yi&q=%s" utf-8)
          ("wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
          ("stanford" "http://plato.stanford.edu/search/searcher.py?query=%s" utf-8))))

(defun w3m-youtube-video ()
  "..."
  (interactive)
  (let* ((video (browse-url-url-at-point))
         (output (format "%s/%s.mp4" youtube-videos-directory video)))
    (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" output video)
    (emms-play-file output)))

;; (defun w3m-youtube-view ()
;;   "View a YouTube link with youtube-dl and mplayer."
;;   (interactive)
;;   (let ((url (thing-at-point-url-at-point)))
;;     (let* ((vid (w3m-youtube-video-name url))
;;            (out (format "%s/%s.mp4" w3m-default-save-directory vid)))
;;       (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" out url)
;;       (start-process "mplayer" nil "mplayer" "-quiet" out))))

;; (defun w3m-youtube-video-name (url)
;;   "Return the video code for a youtube video."
;;     (string-match "[^v]*v.\\([^&]*\\)" url)
;;     (match-string 1 url))

;; (defun w3m-youtube-view ()
;;   "View a YouTube link with youtube-dl and mplayer."
;;   (interactive)
;;   (let ((url (or (w3m-anchor) (w3m-image))))
;;     (string-match "[^v]*v.\\([^&]*\\)" url)
;;     (let* ((vid (match-string 1 url))
;;            (out (format "%s/%s.mp4" youtube-videos-directory vid)))
;;       (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" out url)
;;       (emms-play-file "out")
;;       ;;(start-process "mplayer" nil "mplayer" "-quiet" out)
;;       )))

;; NOTE: w3m and save desktop mode
(defun w3m-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the current URL."
  (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

(defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `w3m' buffer on `save-desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
    (let ((url d-b-misc))
      (when url
        (require 'w3m)
        (if (string-match "^file" url)
            (w3m-find-file (substring url 7))
          (w3m-goto-url-new-session url))
        (current-buffer)))))

;; ERROR: I don't think this works ...
(defun switch-to-w3m-buffer ()
  "Switch to an existing w3m buffer."
  (interactive)
  (if (get-buffer "*w3m*")
      (switch-to-buffer
       (ido-completing-read "w3m session: "
			(save-excursion
			  (delq
			   nil
			   (mapcar (lambda (buf)
				     (when (buffer-live-p buf)
				       (with-current-buffer buf
					 (and (eq major-mode 'w3m-mode)
					      (buffer-name buf)))))
				   (buffer-list))))))
    (w3m w3m-home-page)))

;;; IMPORTANT: highlight custom comment tags
;; NOTE: i suppose technically this should be in the `appearance-config.el' file
(require 'custom-comments)

(custom-comment-create-new-tag "heading" '((t (:foreground "Blue" :weight bold))))
(custom-comment-create-new-tag "comment" '((t (:foreground "Orange" :weight bold))))
(custom-comment-create-new-tag "warning" '((t (:foreground "Red" :weight bold))))
(custom-comment-create-new-tag "testing" '((t (:foreground "Dark Grey" :weight bold))))
(custom-comment-create-new-tag "misc"    '((t (:foreground "Magenta" :weight bold))))
;; (custom-comment-create-new-tag "misc" '((t (:foreground "Cyan" :weight bold))))

(add-tag-to-category "heading" "HEADING")
(add-tag-to-category "heading" "IMPORTANT")
(add-tag-to-category "heading" "SOURCE")

(add-tag-to-category "comment" "COMMENT")
(add-tag-to-category "comment" "NOTE")
(add-tag-to-category "comment" "TODO")

(add-tag-to-category "warning" "WARNING")
(add-tag-to-category "warning" "ERROR")
(add-tag-to-category "warning" "FIX")

(add-tag-to-category "testing" "TESTING")
(add-tag-to-category "testing" "DEBUG")
(add-tag-to-category "testing" "BUG")

(add-tag-to-category "misc" "MISC")
(add-tag-to-category "misc" "EDIT")
(add-tag-to-category "misc" "TEMP")
(add-tag-to-category "misc" "TEST")

;;(custom-comment-mode t)
(highlight-custom-comment-tags) ;; TEMP: call this until the mode works ...

;;; IMPORTANT: window configuration
;; SOURCE: `http://www.emacswiki.org/emacs/TransposeWindows'
(defun swap-window-positions ()
   "*Swap the positions of this window and the next one."
   (interactive)
   (let ((other-window (next-window (selected-window) 'no-minibuf)))
     (let ((other-window-buffer (window-buffer other-window))
           (other-window-hscroll (window-hscroll other-window))
           (other-window-point (window-point other-window))
           (other-window-start (window-start other-window)))
       (set-window-buffer other-window (current-buffer))
       (set-window-hscroll other-window (window-hscroll (selected-window)))
       (set-window-point other-window (point))
       (set-window-start other-window (window-start (selected-window)))
       (set-window-buffer (selected-window) other-window-buffer)
       (set-window-hscroll (selected-window) other-window-hscroll)
       (set-window-point (selected-window) other-window-point)
       (set-window-start (selected-window) other-window-start))
     (select-window other-window)))

;; IMPORTANT: google translate
;; SOURCE: `https://github.com/manzyuk/google-translate'
(require 'google-translate)

(after "google-translate"
  (setq google-translate-enable-ido-completion t
	google-translate-show-phonetic t
	;; google-translate-default-source-language "auto"
	;; google-translate-default-target-language "en"
	)

  (global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
  (global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse))

;;; IMPORTANT: rainbow delimiters
;; SOURCE: `http://www.emacswiki.org/RainbowDelimiters'
(require 'rainbow-delimiters)

(after "rainbow-delimiters"
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode))

;;; IMPORTANT: ibuffer version control
;; SOURCE: `https://github.com/purcell/ibuffer-vc'
;; NOTE: since this package relies on `ibuffer'
(after "ibuffer"
  (require 'ibuffer-vc))

(after "ibuffer-vc"
  (setq ibuffer-formats	'((mark modified read-only vc-status-mini " "
				(name 18 18 :left :elide)
				" "
				;;(size 9 -1 :right)
				;;" "
				;; (mode 14 14 :left :elide)
				;; " "
				;; (vc-status 12 12 :left)
				;; " "
				filename-and-process))))

;;; IMPORTANT: iedit
;; SOURCE: `http://www.emacswiki.org/emacs/Iedit'
(autoload 'iedit-mode "iedit" "Interactive editing." t)

;;; IMPORTANT: adaptive text wrap
(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap" "Adaptive wrap for text mode buffers." t)

(defun turn-on-adaptive-wrap-prefix-mode ()
  "Enable `adaptive-wrap-prefix-mode'."
  (adaptive-wrap-prefix-mode t))

(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)

(provide 'user-config)
;;; user-config.el ends here

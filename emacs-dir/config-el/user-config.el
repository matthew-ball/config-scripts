;;; user-config.el --- Configuration for user settings/options

;; Copyright (C) 2013  Matthew Ball

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

;;; IMPORTANT: undo tree
;; SOURCE: `http://www.emacswiki.org/emacs/UndoTree'
(autoload 'global-undo-tree-mode "undo-tree" "Visualize the current buffer's undo tree." t)

(global-undo-tree-mode) ;; NOTE: enable undo-tree mode

;;; IMPORTANT: extension to info
;; SOURCE: `http://emacswiki.org/emacs/info+.el'
(require 'info+)

;;; IMPORTANT: extension to `ido'
(require 'ido-ubiquitous)

(ido-ubiquitous-mode t)

;;; IMPORTANT: emacs relay chat
;; SOURCE: `http://emacswiki.org/emacs/ERC'
;; SOURCE: `http://www.emacswiki.org/emacs/ErcSSL'
(autoload 'erc-tls "erc" "" t) ;; NOTE: this is to use SSL

(eval-after-load "erc"
  '(progn
     (require 'tls)
     (require 'erc-hl-nicks)
     (require 'erc-stamp)
     (require 'erc-join)
     (require 'erc-spelling)
     (require 'erc-netsplit)
     (require 'erc-ring)
     (require 'erc-goodies)
     (require 'erc-track)
     (require 'erc-match)
     (require 'erc-fill)
     (require 'erc-log)
     (require 'erc-pcomplete)
     (require 'erc-button)
     ;;(require 'erc-track)
     ))

(defvar erc-insert-post-hook)

;;; IMPORTANT: erc modules
;; SOURCE: 
(eval-after-load "erc-button" '(erc-button-enable))
;;(eval-after-load "erc-match" '(erc-match-enable))
(eval-after-load "erc-ring" '(erc-ring-enable))
(eval-after-load "erc-log" '(erc-log-enable))
(eval-after-load "erc-netsplit" '(erc-netsplit-enable))
(eval-after-load "erc-fill" '(erc-fill-disable)) ;; NOTE: disable ERC fil
(eval-after-load "erc-join" '(erc-autojoin-enable)) ;; NOTE: enable auto-joining mode
(eval-after-load "erc-spelling" '(erc-spelling-enable)) ;; NOTE: enable flyspell in ERC
(eval-after-load "erc-goodies" '(erc-scrolltobottom-enable)) ;; NOTE: enable scroll-to-bottom mode
(eval-after-load "erc-hl-nicks" '(erc-hl-nicks-enable))
(eval-after-load "erc-match" '(erc-match-mode t)) ;; NOTE: enable ERC match mode
(eval-after-load "erc-stamp" '(erc-timestamp-mode t)) ;; NOTE: enable ERC timestamp mode

;; (eval-after-load "erc-track"
;;   '(progn
;;      (defun erc-bar-move-back (n)
;;        "Moves back n message lines. Ignores wrapping, and server messages."
;;        (interactive "nHow many lines ? ")
;;        (re-search-backward "^.*<.*>" nil t n))

;;      (defun erc-bar-update-overlay ()
;;        "Update the overlay for current buffer, based on the content of `erc-modified-channels-alist'. Should be executed on window change."
;;        (interactive)
;;        (let* ((info (assq (current-buffer) erc-modified-channels-alist))
;;               (count (cadr info)))
;;          (if (and info (> count erc-bar-threshold))
;;              (save-excursion
;;                (end-of-buffer)
;;                (when (erc-bar-move-back count)
;;                  (let ((inhibit-field-text-motion t))
;;                    (move-overlay erc-bar-overlay
;;                                  (line-beginning-position)
;;                                  (line-end-position)
;;                                  (current-buffer)))))
;;            (delete-overlay erc-bar-overlay))))

;;      (defvar erc-bar-threshold 1 "Display bar when there are more than erc-bar-threshold unread messages.")
;;      (defvar erc-bar-overlay nil "Overlay used to set bar")

;;      (setq erc-bar-overlay (make-overlay 0 0))
;;      (overlay-put erc-bar-overlay 'face '(:underline "black"))

;;      ;; NOTE: put the hook before erc-modified-channels-update
;;      (defadvice erc-track-mode (after erc-bar-setup-hook
;;                                       (&rest args) activate)
;;        ;; NOTE: remove and add, so we know it's in the first place
;;        (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
;;        (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))

;;      (add-hook 'erc-send-completed-hook (lambda (str) (erc-bar-update-overlay)))))

;; TODO: use variables in here ...
(setq ;; erc-server "irc.freenode.net" ;; NOTE: freenode IRC server
      ;; erc-user-full-name user-full-name
      ;; erc-email-userid user-mail-address
      ;; erc-fill-column 90
      ;; erc-echo-notices-in-minibuffer-flag t ;; NOTE: notices in minibuffer
      erc-port 7000 ;; NOTE: `erc-tls' port (for ssl)
      erc-nick "chu"
      erc-nick-uniquifier "_"
      erc-current-nick-highlight-type 'all ;; NOTE: highlight the entire message where current nickname occurs
      erc-button-google-url "http://www.google.com/search?q=%s"
      erc-fill-prefix nil ;; NOTE: ... prefix column on the left (same size as the `timestamp-format' variable above)
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
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE") ;; NOTE: do not track these messages
      ;; erc-hide-list '("JOIN" "NICK" "PART" "QUIT") ;; NOTE: ignore JOIN, NICK, PART and QUIT messages
      erc-mode-line-format "%t %a" ;; NOTE: display only the channel name on the mode-line
      erc-header-line-format nil ;; NOTE: turn off the topic (header) bar
      header-line-format nil ;; NOTE: turn off the topic (header) bar
      erc-input-line-position -1 ;; NOTE: keep input at the last line
      erc-max-buffer-size 20000 ;; NOTE: truncate buffers (so they don't hog core)
      erc-truncate-buffer-on-save t
      erc-prompt ;; NOTE: channel specific prompt ...
      (lambda () (if (and (boundp 'erc-default-recipients) (erc-default-target))
		(erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
	      (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
      erc-autojoin-channels-alist '((".*\\.freenode.net"
				     "#emacs"
                                     "#gnus"
				     "#org-mode"
				     "#stumpwm"
				     "#lisp"
				     ;; "#ubuntu"
				     ;; "#ubuntu-discuss"
				     ;; "#ubuntuforums"
				     "#ubuntu-offtopic"
				     "#ubuntu-ops"
				     "#ubuntu-ops-team"
				     ;; "##club-ubuntu"
				     ;; "#anucssa"
				     ;; "#defocus"
				     ))
      erc-join-buffer 'bury)

;; TODO: eval-after-load
(setq erc-keywords '() ;; NOTE: highlight specific keywords
      erc-current-nick-highlight-type 'nick ;; NOTE: ...
      erc-pal-highlight-type 'all ;; NOTE: nicknames in a message
      erc-fool-highlight-type 'all ;; NOTE: highlight entire message
      erc-pals '("twb" "kcj" "dax" "LjL" "moocow" "AtomicSpark" "IdleOne" "jussi" "topyli") ;; NOTE: highlight pals
      erc-fools '("ubottu" "floodBot1" "floodBot2" "floodBot3" "fsbot" "rudybot" "birny" "lisppaste" "ubnotu") ;; NOTE: highlight fools
      erc-dangerous-hosts '()) ;; NOTE: mark any dangerous hosts

;; IMPORTANT: erc logging
;; SOURCE: `http://www.emacswiki.org/emacs/ErcLogging'
(setq erc-log-channels-directory "~/.emacs.d/erc/logs/"
      erc-save-buffer-on-part t ;; NOTE: save log file automatically when parting or quitting a channel
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-log-insert-log-on-open t
      erc-log-file-coding-system 'utf-8)

(eval-after-load "erc" '(setq erc-modules (delq 'fill erc-modules))) ;; NOTE: disable `erc-fill-mode'

(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(add-hook 'erc-mode-hook '(lambda () (pcomplete-erc-setup) (erc-completion-mode 1)))
;;(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0))) ;; NOTE: I think this needs to be quoted

(remove-hook 'erc-text-matched-hook 'erc-hide-fools)

(setq erc-remove-parsed-property nil)

;;; IMPORTANT: erc commands
;; SOURCE: 
(defun erc-cmd-UNAME (&rest ignore)
  "Display the result of running `uname -a' to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          "[ \n]+$" "" (shell-command-to-string "uname -a"))))
    (erc-send-message
     (concat "{uname -a} [" uname-output "]"))))

(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related stuff, to the current ERC buffer."
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

;; (defun erc-cmd-INFO (&rest ignore)
;;   "Send current info node."
;;   (unless (get-buffer "*info*")
;;     (info))
;;   (let (output)
;;     (with-current-buffer "*info*"
;;       (let* ((file (file-name-nondirectory Info-current-file))
;; 	     (node Info-current-node))
;; 	(setq output (format "(info \"(%s)%s\")"
;; 			     file node))))
;;     (erc-send-message output)))

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

(defun erc-cmd-OPME ()
  "Request ChanServ to put me into operator status."
  (erc-message "PRIVMSG"
	       (format "ChanServ OP %s %s"
		       (erc-default-target)
		       (erc-current-nick)) nil))

(defun erc-cmd-DEOPME ()
  "Deop myself from current channel."
  (erc-cmd-DEOP
   (format "%s"
	   (erc-current-nick))))

(defun erc-cmd-ACCESSLIST ()
  "Display the `access-list' for the current channel."
  (erc-message "PRIVMSG"
	       (format "ChanServ ACCESS %s LIST"
		       (erc-default-target))))

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

(erc-user-action "GNU" "takes" "aside and explains why GNU/Linux is the proper term for the operating system commonly referred to as Linux. See: http://www.gnu.org/gnu/linux-and-gnu.html")

;; IMPORTANT: freenode <*>Serv interaction commands
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

(defun erc-cmd-NS ()
  ""
  (let ((choice (ido-completing-read "Select command: " nickserv-commands-list)))
    (erc-message "PRIVMSG"
                 (format "NickServ help %s"
                         choice))))

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

(defun erc-cmd-MS (&rest junk)
  "Send `MemoServ' command to server process in an `erc-mode' buffer."
  (let ((choice (ido-completing-read "Select command: " memoserv-commands-list)))
    (erc-message "PRIVMSG" (concat "MemoServ " choice " help") nil)))

;; TODO: add a bunch of commands to automagically handle ChanServ stuff
;; ...
(defun erc-cmd-CS (&rest junk)
  )

;;; IMPORTANT: "Custom" `erc-mode' interactions with outside environment
(defun erc-cmd-MAN (program &rest args)
  "Open the `man' page for PROGRAM."
  (man program))

(defun erc-cmd-WOMAN (program &rest args)
  "Open the `woman' page for PROGRAM."
  (woman program))

(eval-after-load "erc-goodies" '(progn
                                  (add-to-list 'erc-noncommands-list 'erc-cmd-SHOW)
                                  (add-to-list 'erc-noncommands-list 'erc-cmd-MAN)
                                  (add-to-list 'erc-noncommands-list 'erc-cmd-WOMAN)))

;; IMPORTANT: when connecting ask me for a password
(defun erc-tls-connect-server (server &rest junk)
  "Ask for a password before connecting to SERVER."
  (let ((password (read-passwd "Enter IRC Password: ")))
    (erc-tls :server server :port erc-port :nick erc-nick :password password)))

;; IMPORTANT: this is where the ERC settings are activated
(defun erc-start-or-switch (&rest junk)
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:7000") ;; NOTE: if ERC is already active ...
      (erc-track-switch-buffer 1) ;; NOTE: ... switch to last active buffer ...
    (erc-tls-connect-server "irc.freenode.net"))) ;; NOTE: ... else, start an `erc-tls' session on `irc.freenode.net'

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
	    "#freenode"
	    "#hurd"
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
	    "#haskell"
	    "#latex"
	    "#reddit"
	    "#defocus"
	    "##club-ubuntu"
	    "##math"
	    "##programming"
	    "##philosophy"))

(defun custom-erc-join-channel (&rest junk)
  "Select a channel from a list of channels to join.

NOTE: This is currently hard-coded to strictly use channels on \"irc.freenode.net\"."
  (interactive)
  (let ((channel (ido-completing-read "Enter channel: " custom-erc-channel-list)))
    (if (get-buffer channel) ;; TODO: check to see if channel is already a open channel ...
	(switch-to-buffer channel)) ;; NOTE: ... and if so, just switch to buffer
    (erc-cmd-JOIN channel))) ;; NOTE: need to be in an existing ERC session for this command to work


;; NOTE: `erc' key-bindings
(eval-after-load "erc" '(define-key erc-mode-map (kbd "C-c C-b") 'custom-erc-join-channel))

;;; IMPORTANT: gnus
;; SOURCE: `http://emacswiki.org/emacs/CategoryGnus'
;; SOURCE: `http://emacswiki.org/emacs/Gnus'
;; (require 'gnus nil 'noerror)
;; (require 'nnimap)
;; (require 'starttls)
;; (require 'smtpmail)
;; (autoload 'gnus-parameters "gnus" "Parameters for Gnus mail." t)
(autoload 'gnus "gnus" "Read mail and news with GNU Emacs." t)
(autoload 'smtpmail-send-it "smtpmail" "Send mail with `smtpmail'." t)

;; TODO:
;; if: there is no file at ~/.authinfo
;; then: create new file ~/.authinfo generating the file:
;;  machine imap.gmail.com login user-primary-email-address port 993
;;  machine smtp.gmail.com login user-primary-email-address port 587
;; else: 
;; 1. use that information (i.e. start gnus)
;; 2. re-write the file to disk (i.e. something has changed)

;;; IMPORTANT: encryption
;; SOURCE: `http://emacswiki.org/emacs/EasyPG'
;; TODO: configuration encryption

;;; IMPORTANT: personal settings
(setq user-mail-address user-primary-email-address ;; NOTE: user primary email address
      ;; user-mail-address "mathew.ball@gmail.com" ;; NOTE: user mail address
      ;; user-full-name "Matthew Ball" ;; NOTE: user full-name
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

;;; IMPORTANT: gnus settings
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

;;; IMPORTANT: visible headers
(setq gnus-visible-headers
      (concat "^From:\\|^Subject:\\|^Newsgroups:"
	      "\\|^Organization:"
	      "\\|^To:\\|^Cc:\\|^Date:"))

;;; IMPORTANT: imap setup
(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p" ;; NOTE: set ssl
      imap-log t ;; NOTE: log the imap session
      imap-store-password t ;; NOTE: store the session password
      gnus-secondary-select-methods
      '((nnimap "gmail" ;; NOTE: gmail login
		(nnimap-address "imap.gmail.com") ;; NOTE: being the "gmail" account, this hard-coding is ok?
		(nnimap-server-port 993)
		;; (nnimap-authinfo-file "~/.authinfo")
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


;;; IMPORTANT: smtp setup(single account)
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "mathew.ball@gmail.com" nil))) ;; TODO: replace email address

;;; IMPORTANT: smtp setup (multiple accounts) (ERROR: this does not work)

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

;;; IMPORTANT: email config
;; (add-hook 'message-send-hook 'change-smtp) ;; change smtp server appropriately
;; (add-hook 'message-mode-hook (function (lambda () (local-set-key (kbd "<tab>") 'bbdb-complete-name)))) ;; NOTE: add tab completion to name in the "To:" field

;; (remove-hook 'gnus-summary-prepare-exit-hook
;; 	     'gnus-summary-expire-articles)

;;; IMPORTANT: html display
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-with-w3m-keymap nil)

;;; IMPORTANT: mode-line
;; (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n") ;; NOTE: set mode-line

;;; IMPORTANT: rss config
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ;; NOTE: topic mode - tree view - is always active

;; (eval-after-load "gnus-sum" ;; NOTE: set the default value of mm-discouraged-alternatives
;;   '(add-to-list 'gnus-newsgroup-variables '(mm-discouraged-alternatives . '("text/html" "image/.*"))))

;; NOTE: display 'text/html' parts in nnrss groups
;; (add-to-list 'gnus-parameters '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;;; IMPORTANT: gnus parameters
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

;;; IMPORTANT: dired extensions
;; SOURCE: `http://emacswiki.org/emacs/dired+.el'
(require 'dired+)

;;; IMPORTANT: directory details
;; SOURCE: `http://www.emacswiki.org/emacs/DiredDetails'
(require 'dired-details+)

(setq dired-details-hidden-string "")
(dired-details-install)

;;; IMPORTANT: smex mode
;; SOURCE: `http://emacswiki.org/emacs/Smex'
(require 'smex)
;;(autoload 'smex "smex" "Super-charge ido-mode." t)

(setq smex-save-file (concat user-emacs-directory "smex-items")
      smex-key-advice-ignore-menu-bar t)

(eval-after-load "smex" '(smex-initialize)) ;; NOTE: super-charge `ido-mode'

;;; IMPORTANT: auto-complete mode
;; SOURCE: `http://emacswiki.org/emacs/AutoComplete'
(require 'auto-complete)

(global-auto-complete-mode t)

;; (setq ac-auto-start nil ;; NOTE: start auto-complete after five characters (modified)
;;       ac-ignore-case t ;; NOTE: always ignore case
;;       ac-auto-show-menu t ;; NOTE: automatically show menu
;;       )

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

;; (ac-flyspell-workaround) ;; NOTE: apparently the flyspell-mode process disables auto-completion

;; (global-auto-complete-mode t) ;; NOTE: enable `auto-complete' where it makes sense

;; (define-globalized-minor-mode real-global-auto-complete-mode ;; NOTE: dirty fix for having AC everywhere
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;; 			   (auto-complete-mode 1))))

;; (real-global-auto-complete-mode t)

;;; IMPORTANT: smart completion
;; TODO: this guy probably needs to be generalised a bit more (though, he works for now)
(defalias 'smart-completion '(lambda () (if (fboundp 'auto-complete)
                                            (auto-complete nil)
                                          (dabbrev-expand nil))))

;;; IMPORTANT: smart tab
(defun smart-tab () ;; NOTE: implement a smarter TAB
  "This smart tab is minibuffer compliant: it acts as usual in the minibuffer.

If mark is active, indents region. Else if point is at the end of a symbol, expands it. Else indents the current line."
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

;;; IMPORTANT: stumpwm mode
;; SOURCE: `http://www.emacswiki.org/emacs/StumpWM'
(autoload 'stumpwm-mode "stumpwm-mode" "Major mode for editing StumpWM." t) ;; NOTE: not ideal

;;; IMPORTANT: emacs multimedia system
;; SOURCE: `http://emacswiki.org/cgi-bin/wiki/EMMS'
;; NOTE: this is really messy, could do with some clean-up
;; (require 'emms-autoloads) ;; NOTE: this could work best
;; (require 'emms-player-simple) ;; NOTE: could be needed
;; (autoload 'emms-all "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-default-players "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-player-mplayer "emms-player-mplayer" "MPlayer interface with GNU Emacs multimedia." t)  ;; ERROR: does not work
;; (autoload 'emms-player-mpd-connect  "emms-player-mode" "Interface between `EMMS' and `MPD'." t)

;; (emms-standard) ;; NOTE: runs just the `emms-standard' configuration
;; (emms-devel) ;; DEBUG: apparently not what I want
;; (emms-all) ;; NOTE: runs `emms-standard' and adds stable `emms' features
;; (emms-default-players)

;; TODO: set with variable
;; (setq emms-source-file-default-directory "~/Music/") ;; NOTE: when asked for `emms-play-directory' always start from this
;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "7700")
;; (add-to-list 'emms-info-functions 'emms-info-mpd) ;; NOTE: get track information from `mpd'
;; (add-to-list 'emms-player-list 'emms-player-mpd) ;; NOTE: add `mpd' to the `emms' player list
;; (emms-player-mpd-connect) ;; NOTE: connect `emms' to `mpd'
;; (setq emms-show-format "NP: %s") ;; NOTE: starts to play a track with "NP: "
;; (add-hook 'emms-player-started-hook 'emms-show) ;; NOTE: show the current track with `emms'
;; (setq emms-player-mpg321-parameters '("-o" "alsa")) ;;NOTE: use alsa with mpg321
;; (define-emms-simple-player flash '(file) "\\.flv$" "mplayer" "-fs") ;; NOTE: play `*.flv' files with `mplayer' (opening full-screen)
;; (add-to-list 'emms-player-list 'emms-player-flash)

;; NOTE: `emms' with `mplayer'
;; NOTE: I don't think I need this
;; (setq emms-player-mplayer-command-name "mplayer"
;;       emms-player-mplayer-parameters '("-slave")
;;       emms-player-mpg321-command-name "mpg123"
;;       ;; emms-player-list '(emms-player-mplayer
;;       ;; 			 emms-player-mplayer-playlist
;;       ;; 			 emms-player-mpg321
;;       ;; 			 emms-player-ogg123)
;;       )

;; (push emms-player-mplayer emms-player-list)
;; (push emms-player-mplayer-playlist emms-player-list)

;; (defun ddliu-emms-player-mplayer-volume-up ()
;;   "Depends on mplayer’s -slave mode."
;;   (interactive)
;;   (process-send-string
;;    emms-player-simple-process-name "volume 1\n"))

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

;;; IMPORTANT: w3m
;; SOURCE: `http://www.emacswiki.org/emacs/emacs-w3m'
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMTabs'
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMHintsAndTips'
(setq w3m-key-binding 'info) ;; NOTE: this needs to be set before loading

(autoload 'w3m "w3m" "Browse the internet with w3m." t)
;; (require 'w3m)
;; (require 'w3m-cookie) ;; NOTE: enable cookies support in w3m
;; (require 'w3m-lnum)

(eval-after-load "w3m" '(progn
                          (require 'w3m-cookie)
                          (require 'w3m-lnum)))

;; NOTE: w3m interface and cookies
(w3m-lnum-mode 1) ;; NOTE: enable Conkeror-like numbered links

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
;; (defun w3m-register-desktop-save ()
;;   "Set `desktop-save-buffer' to a function returning the current URL."
;;   (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

;; (defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
;;   "Restore a `w3m' buffer on `save-desktop' load."
;;   (when (eq 'w3m-mode desktop-buffer-major-mode)
;;     (let ((url d-b-misc))
;;       (when url
;;         (require 'w3m)
;;         (if (string-match "^file" url)
;;             (w3m-find-file (substring url 7))
;;           (w3m-goto-url-new-session url))
;;         (current-buffer)))))

;; (add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))

;; NOTE: w3m mode hooks
(add-hook 'w3m-display-hook ;; NOTE: remove trailing whitespace in w3m buffer
	  (lambda (url)
	    (let ((buffer-read-only nil))
	      (delete-trailing-whitespace))))

;;(add-hook 'w3m-mode-hook 'w3m-register-desktop-save) ;; NOTE: add w3m-buffers to desktop-save

;; ;; COMMENT: adding a new search engine
;; ;; NOTE: Find the entry point of the search engine you want to add, for example: (where foobar is the term you want to search for)
;; ;;  http://my.searchengine.com/?query=foobar
;; ;; NOTE: Then add info to your ~/.emacs-w3m file:
;; ;;  (eval-after-load "w3m-search" '(add-to-list 'w3m-search-engine-alist '("My engine" "http://my.searchengine.com/?query=%s" nil)))

;; COMMENT: w3m search
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSearch'
;; (eval-after-load "w3m-search"
;;   '(setq w3m-search-engine-alist
;; 	 '(("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
;; 	   ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" utf-8)
;; 	   ("wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
;; 	   ("stanford" "http://plato.stanford.edu/search/searcher.py?query=%s" utf-8))))

;;; IMPORTANT: gist
;; SOURCE: `https://github.com/defunkt/gist.el'
(autoload 'gist-region-or-buffer "gist" "Integrate with Github." t)

;;; IMPORTANT: highlight custom comment tags
;; NOTE: i suppose technically this should be in the `appearance-config.el' file
(require 'custom-comments)

(custom-comment-create-new-tag "heading" '((t (:foreground "Blue" :weight bold))))
(custom-comment-create-new-tag "comment" '((t (:foreground "Green" :weight bold))))
(custom-comment-create-new-tag "warning" '((t (:foreground "Red" :weight bold))))
(custom-comment-create-new-tag "testing" '((t (:foreground "Yed" :weight bold))))

;; (add-tag-to-category "heading" "HEADING")
(add-tag-to-category "heading" "IMPORTANT")
(add-tag-to-category "heading" "SOURCE")

;; (add-tag-to-category "comment" "COMMENT")
(add-tag-to-category "comment" "NOTE")
(add-tag-to-category "comment" "TODO")

;; (add-tag-to-category "warning" "WARNING")
(add-tag-to-category "warning" "ERROR")
(add-tag-to-category "warning" "FIX")

;; (add-tag-to-category "testing" "TESTING")
(add-tag-to-category "testing" "DEBUG")
(add-tag-to-category "testing" "BUG")

;;(custom-comment-mode t)
(highlight-custom-comment-tags) ;; TEMP: call this until the mode works ...

;;; IMPORTANT: the insidious big brother database
;; SOURCE: `http://www.emacswiki.org/emacs/BbdbMode'
;; (autoload 'bbdb "bbdb" "" t)

;; (eval-after-load "bbdb" '(bbdb-initialize 'gnus 'message))

;;(setq bbdb-file "~/.emacs.d/contacts-file.el")

;;; IMPORTANT: emacs speaks statistics
;; SOURCE: `http://ess.r-project.org/'
;;(autoload 'ess-mode "ess-mode" "Emacs Speaks Statistics." t)

(provide 'user-config)
;;; user-config.el ends here

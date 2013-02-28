;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/erc-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: erc
;; SOURCE: `http://emacswiki.org/emacs/ERC'
;; SOURCE: `http://www.emacswiki.org/emacs/ErcSSL'
(autoload 'erc-tls "erc" "" t) ;; NOTE: this is to use SSL
(eval-after-load "erc" '(progn
                         (require 'tls)
                         (require 'erc-highlight-nicknames)
                         (require 'erc-stamp)
                         (require 'erc-join)
                         (require 'erc-spelling)
                         (require 'erc-netsplit)
                         (require 'erc-ring)
                         (require 'erc-track)
                         (require 'erc-match)
                         (require 'erc-fill)
                         (require 'erc-pcomplete)
                         (require 'erc-button)
                         ;; (require 'erc-select)
                         (require 'erc-track)))

;; (autoload 'doctor-doc "doctor") ;; NOTE: for use with ERC doctor
;; (autoload 'make-doctor-variables "doctor") ;; NOTE: for use with ERC doctor
;; (autoload 'erc-scrolltobottom-mode "erc-goodies" "Scroll ERC buffer to the end.") ;; NOTE: this is essential
(autoload 'wtf-is "wtf" "ERC command for describing acronyms." t)

;; COMMENT: erc-logging
;; SOURCE: `http://www.emacswiki.org/emacs/ErcLogging'
;;(setq erc-log-channels-directory "~/.erc/logs/")
;;(setq erc-save-buffer-on-part t) ;; NOTE: save log file automatically when parting or quitting a channel
;;(setq erc-log-insert-log-on-open t) ;; NOTE: I *think* this is the goods

;;; COMMENT: erc faces
(defface erc-prompt-face '((t (:foreground "black" :bold t))) "ERC prompt.")
;;(defface erc-timestamp-face '((t (:foreground "black" :weight bold))) "ERC timestamp.")

(defvar erc-insert-post-hook)

;; (eval-after-load "erc-bbdb" '(erc-bbdb-mode t)) ;; NOTE: enable bbdb
(eval-after-load "erc-button" '(erc-button-enable))
(eval-after-load "erc-match" '(erc-match-enable)) ;; ??
(eval-after-load "erc-match" '(erc-match-mode t)) ;; ??
(eval-after-load "erc-ring" '(erc-ring-enable))
(eval-after-load "erc-netsplit" '(erc-netsplit-enable))
(eval-after-load "erc-fill" '(erc-fill-disable)) ;; NOTE: disable ERC fil
(eval-after-load "erc-stamp" '(erc-timestamp-mode t)) ;; NOTE: enable ERC timestamp mode
(eval-after-load "erc-join" '(erc-autojoin-enable)) ;; NOTE: enable auto-joining mode
(eval-after-load "erc-spelling" '(erc-spelling-enable)) ;; NOTE: enable flyspell in ERC
(eval-after-load "erc-goodies" '(erc-scrolltobottom-enable)) ;; NOTE: enable scroll-to-bottom mode
(eval-after-load "erc-highlight-nicknames" '(erc-highlight-nicknames-enable))
(eval-after-load "erc-modules"
  '(progn 
     (add-to-list 'erc-modules 'highlight-nicknames)
     (erc-update-modules)))
(eval-after-load "erc-track"
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of `erc-modified-channels-alist'. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
              (count (cadr info)))
         (if (and info (> count erc-bar-threshold))
             (save-excursion
               (end-of-buffer)
               (when (erc-bar-move-back count)
                 (let ((inhibit-field-text-motion t))
                   (move-overlay erc-bar-overlay
                                 (line-beginning-position)
                                 (line-end-position)
                                 (current-buffer)))))
           (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1 "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil "Overlay used to set bar")

     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "black"))

     ;; NOTE: put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
                                      (&rest args) activate)
       ;; NOTE: remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str) (erc-bar-update-overlay)))))

;; TODO: use variables in here ...
(setq ;;erc-server "irc.freenode.net" ;; NOTE: freenode IRC server
      erc-port 7000 ;; NOTE: `erc-tls' port (for ssl)
      erc-nick "chu"
      erc-nick-uniquifier "_"
      erc-button-google-url "http://www.google.com/search?q=%s"
      ;; erc-user-full-name user-full-name
      ;; erc-email-userid user-mail-address
      erc-current-nick-highlight-type 'all ;; NOTE: highlight the entire message where current nickname occurs
      erc-timestamp-format "[%H:%M] " ;; NOTE: put timestamps on the left
      erc-fill-prefix nil ;; NOTE: ... prefix column on the left (same size as the `timestamp-format' variable above)
      ;; erc-fill-column 90
      erc-fill-mode nil ;; NOTE: again, disable ERC fill (not sure why I have done it in multiple places)
      erc-timestamp-right-column 61
      erc-timestamp-only-if-changed-flag nil ;; NOTE: always show timestamp
      erc-track-showcount t ;; NOTE: show count of unseen messages
      erc-insert-timestamp-function 'erc-insert-timestamp-left ;; NOTE: insert timestamp in the left column
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
      ;; erc-echo-notices-in-minibuffer-flag t ;; NOTE: notices in minibuffer
      erc-max-buffer-size 20000 ;; NOTE: truncate buffers (so they don't hog core)
      erc-truncate-buffer-on-save t
      erc-prompt ;; NOTE: channel specific prompt
      (lambda () (if (and (boundp 'erc-default-recipients) (erc-default-target))
		(erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
	      (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
      erc-autojoin-channels-alist '((".*\\.freenode.net"
				     "#emacs"
				     ;;"#org-mode"
				     "#stumpwm"
				     ;;"#lisp"
				     ;; "#ubuntu"
				     "#ubuntu-offtopic"
				     "#ubuntu-discuss"
				     ;; "#ubuntuforums"
				     "#ubuntu-ops"
				     "#ubuntu-ops-team"
				     ;; "##club-ubuntu"
				     ;; "#felines-lair"
				     ;; "#anucssa"
				     ;; "#defocus"
				     ))
      erc-join-buffer 'bury)

(eval-after-load "erc" '(setq erc-modules (delq 'fill erc-modules))) ;; NOTE: disable `erc-fill-mode'

(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(add-hook 'erc-mode-hook '(lambda () (pcomplete-erc-setup) (erc-completion-mode 1)))
;;(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0))) ;; NOTE: I think this needs to be quoted

(remove-hook 'erc-text-matched-hook 'erc-hide-fools)

;; TODO: eval-after-load
(setq erc-keywords '() ;; NOTE: highlight specific keywords
      erc-current-nick-highlight-type 'nick ;; NOTE: ...
      erc-pal-highlight-type 'all ;; NOTE: nicknames in a message
      erc-fool-highlight-type 'all ;; NOTE: highlight entire message
      erc-pals '("twb" "kcj" "dax" "LjL" "moocow" "AtomicSpark" "IdleOne" "jussi" "topyli") ;; NOTE: highlight pals
      erc-fools '("ubottu" "floodBot1" "floodBot2" "floodBot3" "fsbot" "rudybot" "birny" "lisppaste" "ubnotu") ;; NOTE: highlight fools
      erc-dangerous-hosts '()) ;; NOTE: mark any dangerous hosts

(setq erc-remove-parsed-property nil)

;; (defvar erc-doctor-id "{Emacs doctor} ") ;; NOTE: erc doctor (this is a bit of a joke)

;; (defun erc-cmd-DOCTOR (&optional last-sender &rest ignore)
;;   "Get the last message in the channel and doctor it."
;;   (let ((limit (- (point) 1000))
;;         (pos (point))
;;         doctor-buffer
;;         last-message
;;         text)
;;     (when (< limit 0) (setq limit 0)) ;; NOTE: make sure limit is not negative
;;     (while (and pos (not (let ((data (get-text-property pos 'erc-parsed))) ;; NOTE: search backwards for text from someone
;;                            (and data
;;                                 (string= (aref data 3) "PRIVMSG")
;;                                 (or (not last-sender)
;;                                     (string= (car (split-string (aref data 2) "!"))
;;                                              last-sender))))))
;;       (setq pos (previous-single-property-change
;;                  pos 'erc-parsed nil limit))
;;       (when (= pos limit)
;;         (error "No appropriate previous message to doctor.")))
;;     (when pos
;;       (setq last-sender (car (split-string
;;                               (aref (get-text-property
;;                                      pos 'erc-parsed) 2) "!"))
;;             doctor-buffer (concat "*ERC Doctor: " last-sender "*")
;;             last-message (split-string
;;                           (replace-regexp-in-string ;; NOTE: remove punctuation from end of sentence
;;                            "[ .?!;,/]+$" ""
;;                            (aref (get-text-property pos
;;                                                     'erc-parsed) 5)))
;;             text (mapcar (lambda (s)
;;                            (intern (downcase s)))
;;                          (if (string-match ;; NOTE: remove salutation if it exists
;;                               (concat "^" erc-valid-nick-regexp
;;                                       "[:,]*$\\|[:,]+$")
;;                               (car last-message))
;;                              (cdr last-message)
;;                            last-message))))
;;     (erc-send-message
;;      (concat erc-doctor-id
;;              (if (not (erc-query-buffer-p)) ;; NOTE: only display sender if not in a query buffer
;;                  (concat last-sender ": "))
;;              (save-excursion
;;                (if (get-buffer doctor-buffer)
;;                    (set-buffer doctor-buffer)
;;                  (set-buffer (get-buffer-create doctor-buffer))
;;                  (make-doctor-variables))
;;                (erase-buffer)
;;                (doctor-doc text)
;;                (buffer-string))))))

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

(defun erc-cmd-WTF (term &rest ignore)
  "Look up definition for TERM."
  (let ((def (wtf-is term)))
    (if def
        (erc-send-message
         (concat "{Term} " (upcase term) " is " def))
      (message (concat "No definition found for " (upcase term))))))

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

;; TODO: "Custom" `erc-mode' commands
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

(defun erc-cmd-ACCESS-LIST ()
  "Display the `access-list' for the current channel."
  (erc-message "PRIVMSG"
	       (format "ChanServ ACCESS %s LIST"
		       (erc-default-target))))

;;; COMMENT: macro for "custom" ERC messages
(defmacro erc-user-message (command message)
  "Macro to create \"custom\" messages to an IRC user in an `erc-mode' session."
  (let ((func (intern (concat "erc-cmd-" command)))
	(doc (format "Send the command \"%s\" in an `erc-mode' buffer." command))
	(string message))
    `(defun ,func (name &rest junk)
       ,doc
       (erc-send-message (concat name ": " ,string)))))

;; TODO: need to rename these
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
(erc-user-message "OHMY" "Please watch your language in this channel, thank you.")
(erc-user-message "EMACS" "GNU Emacs is a powerful lisp environment and text editor. See: http://www.gnu.org/software/emacs/")
(erc-user-message "STUMPWM" "StumpWM is a tiling window manager for X11 written in common lisp. See: http://www.nongnu.org/stumpwm/")
(erc-user-message "CONKEROR" "Conkeror is a highly extensible web browser based on Firefox. See: http://conkeror.org/")
(erc-user-message "ORGMODE" "Org-mode is for keeping notes, maintaining TODO lists, project planning, and writing. See: http://orgmode.org/")

(erc-user-action "GNU" "takes" "aside and explains why GNU/Linux is the proper term for the operating system commonly referred to as Linux. See: http://www.gnu.org/gnu/linux-and-gnu.html")

;;; COMMENT: freenode <*>Serv interaction commands
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
;;   (let ((func (insert (concat "erc-cmd-SERV-" command)))
;; 	(doc (format "Send the command \"%s\" to a server process in an `erc-mode' buffer." command)))
;;     `(defun ,func (&rest junk)
;;        ,doc
;;        (erc-message "PRIVMSG" (format "%s help" ,command)))))

;;(erc-user-cmd "NICKSERV")

(defun erc-cmd-NS () (let ((choice (ido-completing-read "Select command: " nickserv-commands-list))) (erc-message "PRIVMSG" (format "NickServ help %s" choice))))

;;; COMMENT: freenode user and channel modes
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

;; (defmacro erc-user-message (command message)
;;   "Macro to create \"custom\" messages to an IRC user in an `erc-mode' session."
;;   (let ((func (intern (concat "erc-cmd-" command)))
;; 	(doc (format "Send the command \"%s\" in an `erc-mode' buffer." command))
;; 	(string message))
;;     `(defun ,func (name &rest junk)
;;        ,doc
;;        (erc-send-message (concat name ": " ,string)))))

;; (defun erc-cmd-ACCESS-LIST ()
;;   "Display the `access-list' for the current channel."
;;   (erc-message "PRIVMSG"
;; 	       (format "ChanServ ACCESS %s LIST"
;; 		       (erc-default-target))))

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

;;; COMMENT: "Custom" `erc-mode' messages
;; (defun erc-cmd-GUIDELINES (name &rest junk)
;;   "Send the user a link to the Ubuntu IRC guideliness page."
;;   (erc-send-message (concat name ": The guidelines for using the Ubuntu channels can be found here: http://wiki.ubuntu.com/IRC/Guidelines")))

;; (defun erc-cmd-OHMY (name &rest junk)
;;  "Send the user a message asking them to obey a language policy."
;;  (erc-send-message (concat name ": Please watch your language in this channel, thank you.")))

;; (defun erc-cmd-GNU (name &rest junk)
;;   "Send the user a link to the GNU/Linux naming issue."
;;   (erc-send-action (erc-default-target)
;; 		   (concat "takes " name " aside and explains why GNU/Linux is the proper term for the operating system commonly referred to as Linux. See: http://www.gnu.org/gnu/linux-and-gnu.html")))

;;; COMMENT: "Custom" `erc-mode' interactions with outside environment
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

;; COMMENT: when connecting ask me for a password
(defun erc-tls-connect-server (server &rest junk)
  "Ask for a password before connecting to SERVER."
  (let ((password (read-passwd "Enter IRC Password: ")))
    (erc-tls :server server :port erc-port :nick erc-nick :password password)))

;; COMMENT: this is where the ERC settings are activated
(defun erc-start-or-switch (&rest junk)
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:7000") ;; NOTE: if ERC is already active ...
      (erc-track-switch-buffer 1) ;; NOTE: ... switch to last active buffer ...
    (erc-tls-connect-server "irc.freenode.net"))) ;; NOTE: ... else, start an `erc-tls' session on `irc.freenode.net'

;;; COMMENT: custom this is an external function I wrote, it is just a (hard-coded) list of channels to join
(defvar custom-erc-channel-list ;; NOTE: currently this is hard-coded for "freenode" convert this to an alist
  nil
  "List of channels to connect to.

NOTE: This is currently hard-coded to strictly use channels on \"irc.freenode.net\".")

(setq custom-erc-channel-list
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

;;; COMMENT: `erc' key-bindings
(eval-after-load "erc" '(define-key erc-mode-map (kbd "C-c C-b") 'custom-erc-join-channel))

(provide 'erc-config)

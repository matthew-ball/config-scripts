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

(defgroup user-custom nil "Custom variables for user-specific settings (and packages)." :group 'user-variables)

;;; IMPORTANT: ace-jump-mode
;; SOURCE: `http://www.emacswiki.org/emacs/AceJump'
;; (autoload 'ace-jump-mode "ace-jump-mode" "..." t)

;; (global-set-key (kbd "C-c SPC") #'ace-jump-mode)
;; (global-set-key (kbd "C-c C-SPC") #'ace-jump-mode-pop-mark)

;;; IMPORTANT: the insidious big brother database
;; SOURCE: `http://www.emacswiki.org/emacs/BbdbMode'
(after "gnus"
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)

  (setq bbdb-mua-update-interactive-p '(query . create)
	;; bbdb-file (expand-file-name (concat user-emacs-directory "bbdb-file.el"))
	bbdb-file (expand-file-name (concat user-emacs-directory "contacts-file.el"))
	bbdb-mua-pop-up nil
	bbdb-default-country "Australia"))

;;; IMPORTANT: make `ido' available everywhere
;; SOURCE: `https://github.com/technomancy/ido-ubiquitous'
(after "ido"
  (require 'ido-ubiquitous)
  (ido-ubiquitous-mode t))

;;; IMPORTANT: improve `ido's flex matching
;; SOURCE: `https://github.com/lewang/flx'
(after "ido"
  (require 'flx)
  (setq ido-use-faces t)
  (flx-ido-mode t))

;;; IMPORTANT: smex mode
;; SOURCE: `http://emacswiki.org/emacs/Smex'
(autoload 'smex "smex" "Super-charge ido-mode." t)

(after "smex"
  (setq smex-save-file (concat user-emacs-directory "smex-items")
        smex-key-advice-ignore-menu-bar t)

  (smex-initialize)) ;; NOTE: super-charge `ido-mode'

(global-set-key (kbd "M-x") #'smex) ;; NOTE: smex improves default ido at the mini buffer
(global-set-key	(kbd "M-X") #'smex-major-mode-commands) ;; NOTE: available major mode commands

;;; IMPORTANT: browse kill ring
;; SOURCE: `http://www.emacswiki.org/BrowseKillRing'
(autoload 'browse-kill-ring "browse-kill-ring" "Browse the `kill-ring'." t)

(after "browse-kill-ring"
  (browse-kill-ring-default-keybindings))

(global-set-key (kbd "C-x R") #'browse-kill-ring)

;;; IMPORTANT: gist
;; SOURCE: `https://github.com/defunkt/gist.el'
(autoload 'gist-buffer "gist" "Integrate with Github." t)

;;(define-key programming-map (kbd "g") #'gist-buffer)

;;; IMPORTANT: git integration
;; SOURCE: `http://www.emacswiki.org/emacs/Magit'
(autoload 'magit-status "magit" "Version control with Git." t) ;; NOTE: magit for use with github

;; (after "magit"
;;   (setq magit-save-some-buffers t))

;;(define-key programming-map (kbd "m") #'magit-status)

;;; IMPORTANT: undo tree
;; SOURCE: `http://www.emacswiki.org/emacs/UndoTree'
(autoload 'global-undo-tree-mode "undo-tree" "Visualize the current buffer's undo tree." t)

;; NOTE: persistent undo history
;; (after "undo-tree"
;;   (setq undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist `((".*" . ,(concat user-emacs-directory "undo")))))

(global-undo-tree-mode) ;; NOTE: enable undo-tree mode

(global-set-key (kbd "C-z") #'undo-tree-visualize)
(global-set-key (kbd "M-Z") #'undo-tree-redo)

;;; IMPORTANT: emacs relay chat
;; SOURCE: `http://emacswiki.org/emacs/ERC'
;; SOURCE: `http://www.emacswiki.org/emacs/ErcSSL'
(autoload 'erc-tls "erc" "Internet Relay Chat in GNU Emacs with SSL." t)

(after "erc"
  (require 'tls)
  (require 'erc-spelling) ;; spelling
  (require 'erc-goodies)
  ;; (require 'erc-ibuffer) ;; ibuffer
  (require 'erc-log) ;; logging

  ;; IMPORTANT: erc spelling
  (erc-spelling-enable)

  ;; IMPORTANT: keep cursor at prompt
  (erc-scrolltobottom-enable)

  ;; IMPORTANT: netsplit
  (setq erc-netsplit-show-server-mode-changes-flag t)

  ;; IMPORTANT: erc match
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcMatch'
  (setq erc-keywords '() ;; NOTE: highlight specific keywords
        erc-current-nick-highlight-type 'nick ;; NOTE: ...
	erc-pal-highlight-type 'nick ;; NOTE: nicknames in a message
        erc-pals '()
        erc-fool-highlight-type 'all ;; NOTE: highlight entire message
        erc-fools '("ubottu" "floodBot1" "floodBot2" "floodBot3" "fsbot" "rudybot" "birny" "lisppaste" "ubnotu")
        erc-dangerous-hosts '()) ;; NOTE: mark any dangerous hosts

  (erc-lurker-initialize)

  (setq-default erc-ignore-list '()) ;; SOURCE: `http://www.emacswiki.org/emacs/ErcIgnoring'

  (remove-hook 'erc-text-matched-hook 'erc-hide-fools) ;; NOTE: keep messages from `erc-fools'

  ;; IMPORTANT: erc logging
  ;; SOURCE: `http://www.emacswiki.org/emacs/ErcLogging'
  (setq erc-save-buffer-on-part t ;; NOTE: save log file automatically when parting or quitting a channel
        erc-save-queries-on-quit t
	erc-log-channels-directory (expand-file-name (format "%s/erc/logs/" user-emacs-directory))
        erc-log-write-after-send t
        erc-log-write-after-insert t
        ;;erc-log-insert-log-on-open t
        erc-log-file-coding-system 'utf-8)

  (erc-log-enable)

  ;; TODO: `erc-button-alist'

  ;; TODO: clean up multiple server handling
  ;; (defcustom custom-erc-server-list nil "List of servers to connect to." :group 'user-custom :type 'list)
  ;; (defcustom custom-erc-channel-alist nil "List of (server . channel) for available channels on server." :group 'user-custom :type 'list)
  ;; (defcustom custom-erc-autojoin-alist nil "List of (server . channel) pairs for auto-joining." :group 'user-custom :type 'list)

  ;; (defun custom-erc-server (server)
  ;;   (pushnew server custom-erc-server-list))

  ;; (custom-erc-server "irc.freenode.net")
  ;; (custom-erc-server "eu.undernet.org")

  ;; (defun custom-erc-server-channel (server &rest channels)
  ;;   (if (listp channels)
  ;; 	(dolist (channel channels)
  ;; 	  (pushnew (cons server channel) custom-erc-channel-alist :test #'equal))
  ;;     (pushnew (cons server channels) custom-erc-channel-alist :test #'equal)))

  ;; (defun custom-erc-server-autojoin (server &rest channels)
  ;;   (if (listp channels)
  ;; 	(dolist (channel channels)
  ;; 	  (pushnew (cons server channel) custom-erc-autojoin-alist :test #'equal))
  ;;     (pushnew (cons server channels) custom-erc-autojoin-alist :test #'equal)))

  ;; (custom-erc-server-channel "irc.freenode.net" '("#emacs" "#screen" "#org-mode" "#stumpwm" "#irssi" "#gnus" "#sbcl" "#hurd" "#lisp" "#bash" "#scheme" "#clojure" "#guile" "#haskell" "#latex" "#ruby" "#ubuntu" "#ubuntu-offtopic" "#ubuntu-mars""#ubuntu+1" "#ubuntu-server" "#ubuntu-au" "#ubuntu-au-chat" "#ubuntu-discuss" "#ubuntu-irc" "#ubuntu-programming" "#ubuntu-bots-devel" "#ubuntu-bots" "#ubuntu-app-devel" "#ubuntu-devel" "#ubuntu-bugs" "#ubuntuforums" "#ubuntu-ops" "#ubuntu-ops-team" "#ubuntu-release-party" "#ubuntu-classroom" "#ubuntu-classroom-chat" "#ubuntu-fr" "#ubuntu-fr-offtopic" "#debian" "#debian-offtopic" "#reddit" "#anucssa" "#defocus" "##club-ubuntu" "##math" "##programming" "##economics" "##linguistics" "##philosophy"))
  ;; (custom-erc-server-autojoin "irc.freenode.net" '("#emacs" "#lisp" "#ubuntu-offtopic" "#ubuntu-mars" "#stumpwm" "#ubuntu-ops" "#ubuntu-ops-team" "#ruby"))

  ;; NOTE: currently this is hard-coded for "freenode" (TODO: convert this to an alist)
  (defcustom custom-erc-channel-list '("#ubuntu" "#ubuntu+1" "#ubuntu-server" "#ubuntu-au" "#ubuntu-au-chat" "#ubuntu-offtopic" "#ubuntu-discuss"
				       "#ubuntu-irc" "#ubuntu-programming" "#ubuntu-bots-devel" "#ubuntu-bots" "#ubuntu-app-devel" "#ubuntu-devel"
				       "#ubuntu-bugs" "#ubuntuforums" "#ubuntu-ops" "#ubuntu-ops-team" "#ubuntu-release-party" "#ubuntu-classroom"
				       "#ubuntu-classroom-chat" "#ubuntu-fr" "#ubuntu-fr-offtopic" "#freenode" "#bash" "#gnus" "#hurd" "#sbcl"
				       "#debian" "#debian-offtopic" "#emacs" "#org-mode" "#stumpwm" "#conkeror" "#screen" "#irssi" "#lisp" "#ruby"
				       "#scheme" "#guile" "#clojure" "#haskell" "#latex" "#reddit" "#anucssa" "#defocus" "##club-ubuntu" "##math"
				       "##programming" "##economics" "##linguistics" "##philosophy")
    "List of channels ERC can connect to (this should be replaced with the alist matching to server)." :group 'user-custom :type 'list)

  (defmacro custom-erc-propertize (prompt)
    `(erc-propertize (format "%s>" ,prompt) 'read-only t 'rear-nonsticky t 'front-nonsticky t))

  (defun custom-erc-prompt ()
    (if (and (boundp 'erc-default-recipients) (erc-default-target))
  	(custom-erc-propertize (erc-default-target))
      (custom-erc-propertize "ERC")))

  ;; IMPORTANT: erc user variables
  (setq erc-nick user-irc-nickname
	;;erc-nick (getenv "USER")
        erc-nick-uniquifier "_"
        ;; erc-server "irc.freenode.net"
        erc-port 7000 ;; NOTE: `erc-tls' port (for ssl)
        erc-user-full-name user-full-name
        erc-email-userid user-mail-address
        erc-format-nick-function 'erc-format-@nick
        erc-current-nick-highlight-type 'all ;; NOTE: highlight the entire message where current nickname occurs
        erc-button-google-url "http://www.google.com/search?q=%s"
        erc-timestamp-format "[%H:%M] " ;; NOTE: put timestamps on the left
        erc-timestamp-right-column 61
        erc-timestamp-only-if-changed-flag nil ;; NOTE: always show timestamp
	erc-insert-timestamp-function 'erc-insert-timestamp-left ;; NOTE: insert timestamp in the left column
        erc-track-showcount t ;; NOTE: show count of unseen messages
	erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477") ;; NOTE: do not track these messages
        erc-kill-buffer-on-part t ;; NOTE: kill buffers for channels after /part
        erc-kill-queries-on-quit t ;; NOTE: kill buffers for queries after quitting the server
        erc-kill-server-buffer-on-quit t ;; NOTE: kill buffers for server messages after quitting the server
        erc-interpret-mirc-color t ;; NOTE: interpret mIRC-style colour commands in IRC chats
        ;;erc-hide-list '("JOIN" "NICK" "PART" "QUIT") ;; NOTE: ignore JOIN, NICK, PART and QUIT messages
        erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT")
        erc-mode-line-format "%t %a" ;; NOTE: display only the channel name in the mode-line
        erc-header-line-format nil ;; NOTE: turn off the topic (header) bar
        erc-input-line-position -1 ;; NOTE: keep input at the last line
        erc-max-buffer-size 20000 ;; NOTE: truncate buffers (so they don't hog core)
        erc-truncate-buffer-on-save t
	erc-remove-parsed-property nil
	erc-prompt #'custom-erc-prompt
        erc-join-buffer 'bury
        erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs" "#stumpwm" "#lisp" "#ruby" "#ubuntu-offtopic" "#ubuntu-mars" "#ubuntu-ops" "#ubuntu-ops-team")))

  (setq erc-modules (delq 'fill erc-modules)) ;; NOTE: disable `erc-fill-mode'

  ;;; IMPORTANT: erc custom inserts
  (propertize-word erc-bold ?)
  (propertize-word erc-underline ?)

  ;; NOTE: `erc' key-bindings
  (defun custom-erc-key-bindings ()
    (define-key erc-mode-map (kbd "C-c C-b") #'custom-erc-join-channel)
    (define-key erc-mode-map (kbd "C-c b") #'erc-bold-word)
    (define-key erc-mode-map (kbd "C-c u") #'erc-underline-word))

  (add-hook 'erc-insert-post-hook #'erc-truncate-buffer)
  (add-hook 'erc-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'erc-mode-hook #'custom-erc-key-bindings)

  ;; IMPORTANT: erc user commands
  ;; (require 'erc-nicklist)
  ;; (require 'erc-star-serv) ;; NOTE: freenode stuff (TODO: not finished)
  ;; (require 'erc-bbdb)
  (require 'erc-extensions))

;; IMPORTANT: erc goodies
(after "erc-goodies"
  ;; the following "noncommands" are all defined in `erc-extensions'
  (add-to-list 'erc-noncommands-list 'erc-cmd-SHOW)
  (add-to-list 'erc-noncommands-list 'erc-cmd-MAN)
  (add-to-list 'erc-noncommands-list 'erc-cmd-WOMAN))

;; NOTE: ...
(defun erc-tls-connect-server (server &rest junk)
  "Ask for a password before connecting to SERVER."
  (let ((password (read-passwd "Enter IRC Password: ")))
    (erc-tls :server server :port erc-port :nick erc-nick :password password)))

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

(defun erc-start-or-switch (&rest junk)
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (save-excursion
    (if (get-buffer "irc.freenode.net:7000") ;; NOTE: if ERC is already active ...
	;; TODO: if connected to IRC and there has been no activity, execute `custom-erc-switch-buffer'
	;; (when (equal (erc-track-switch-buffer 1) erc-track-last-non-erc-buffer)
	;;   (custom-erc-switch-buffer))
	(erc-track-switch-buffer 1)
      (erc-tls-connect-server "irc.freenode.net")))) ;; NOTE: ... else, start an `erc-tls' session on `irc.freenode.net'

(defun custom-erc-join-channel (&rest junk)
  "Select a channel from a list of channels to join.

NOTE: This is currently hard-coded to strictly use channels on \"irc.freenode.net\"."
  (interactive)
  (let ((channel (ido-completing-read "Enter channel: " custom-erc-channel-list)))
    (when (get-buffer channel) ;; TODO: check to see if channel is already a open channel ...
      (switch-to-buffer channel)) ;; NOTE: ... and if so, just switch to buffer
    (erc-cmd-JOIN channel))) ;; NOTE: need to be in an existing ERC session for this command to work

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

;; (defun create-authinfo ()
;;   "Create an authinfo file, if none exists.

;; This requires collecting user input - including user password.")

(setq gnus-inhibit-startup-message t)

(after "gnus"
  (require 'smtpmail)
  ;; (require 'nnimap)
  ;; (require 'starttls)

  ;; IMPORTANT: encryption
  ;; SOURCE: `http://emacswiki.org/emacs/EasyPG'
  ;; TODO: configure encryption

  ;; IMPORTANT: personal settings
  (setq user-mail-address user-primary-email-address
	;; gnus-startup-file (expand-file-name (concat user-emacs-directory "newsrc"))
	;; gnus-init-file (expand-file-name (concat user-emacs-directory "gnus.el"))
        mail-personal-alias-file "~/.mailrc"
	mail-aliases t ;; NOTE: enable mail aliases (NOTE: uses `mail-personal-alias-file')
	auth-source-save-behavior nil
        read-mail-command #'gnus
	send-mail-function #'smtpmail-send-it ;; NOTE: not for gnus (mail-mode)
	message-kill-buffer-on-exit t ;; NOTE: kill the mail buffer after sending message
	message-from-style 'angles ;; NOTE: specifies how the "From" header appears
        message-send-mail-function #'smtpmail-send-it) ;; NOTE: for gnus (message-mode)

  ;; TODO: should these be set in `general-config.el' or even `init.el' ???
  (setq custom-mail-dir (expand-file-name user-mail-directory) ;; NOTE: set directory for mail
	custom-news-dir (expand-file-name user-news-directory)) ;; NOTE: set directory for news

  ;; IMPORTANT: gnus settings
  (setq gnus-use-full-window t
	;;gnus-use-full-window nil ;; NOTE: don't ruin my frame!
	gnus-adaptive-pretty-print t
	gnus-agent-plugged nil
        gnus-agent-expire-all t  ;; NOTE: allow uncaching of unread articles
        gnus-agent-article-alist-save-format 2 ;; NOTE: compress cache
	gnus-select-method '(nnml "")
	gnus-check-new-newsgroups nil ;; NOTE: suppress checking for new groups
        gnus-save-newsrc-file nil ;; NOTE: turn off writing the `.newsrc' file
        gnus-read-newsrc-file nil ;; NOTE: ignore the `.newsrc' file
        gnus-interactive-exit nil
        gnus-save-killed-list nil ;; NOTE: do not save a list of killed groups to startup file
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\(\\|$\\)\\|^[\"]\"[#'()]"
        gnus-invalid-group-regexp "[:`'\"]\\|^$"
        gnus-permanently-visible-groups "mail"
        gnus-thread-hide-subtree t
        gnus-fetch-old-headers t
	gnus-show-all-headers nil
	gnus-group-line-format "%M%S%p%P%y:%B%(%G%)\n"
	gnus-group-mode-line-format " %%b {%S}"
	gnus-summary-mode-line-format " %p %Z"
	gnus-summary-line-format "%U%R%z%I%(%[ %-18,18f%]%) %s\n"
	gnus-article-mode-line-format " %S%m"
	gnus-summary-display-arrow t
	;; gnus-use-trees t
        gnus-thread-ignore-subject t
	;; gnus-use-dribble-file t
	;; gnus-dribble-directory (concat user-emacs-directory "gnus/")
        gnus-always-read-dribble-file t ;; NOTE: don't bugger me with dribbles
        gnus-summary-thread-gathering-function #'gnus-gather-threads-by-subject
	gnus-visible-headers (concat "^From:\\|^Subject:" "\\|^To:\\|^Cc:\\|^Date:")
        gnus-posting-styles '((".*" (name "Matthew Ball")) ("gmail" (address "mathew.ball@gmail.com"))))

  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode) ;; NOTE: topic mode - tree view - is always active

  ;; NOTE: html display
  (setq mm-text-html-renderer 'w3m
	mm-inline-large-images 'resize
	mm-inline-text-html-with-images t
	mm-inline-text-html-with-w3m-keymap nil)

  ;; NOTE: display 'text/html' parts in nnrss groups
  (add-to-list 'gnus-parameters '("\\`nnrss:" (mm-discouraged-alternatives nil)))

  ;; IMPORTANT: imap setup
  ;; TODO: ...
  ;; (defmacro custom-imap-server (name address)
  ;;   `(nnimap ,name
  ;; 	     (nnimap-address ,address)
  ;; 	     (nnimap-server-port 993)
  ;; 	     (nnimap-authinfo-file "~/.authinfo.gpg")
  ;; 	     (nnimap-authenticator login)
  ;; 	     (nnimao-expunge-on-close 'never)
  ;; 	     (nnimap-stream ssl)))

  (setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p" ;; NOTE: set ssl
        imap-log t ;; NOTE: log the imap session
        imap-store-password t ;; NOTE: store the session password
	gnus-secondary-select-methods `((nntp "news.gmane.org")
					;; ,(custom-imap-server "anu" "anumail.anu.edu.au")
					;; ,(custom-imap-server "gmail" "imap.gmail.com")
					(nnimap "gmail"
						(nnimap-address "imap.gmail.com")
						(nnimap-server-port 993)
						(nnimap-authinfo-file "~/.authinfo.gpg")
						(nnimap-authenticator login)
						(nnimap-expunge-on-close 'never)
						(nnimap-stream ssl))))

  ;; IMPORTANT: smtp setup (single account)
  (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-primary-email-address nil))))

;;; IMPORTANT: abbreviations
;; SOURCE: `http://www.emacswiki.org/emacs/AbbrevMode'
;; (require 'abbrev)

;; (after "abbrev"
;;   (setq abbrev-file-name "~/.emacs.d/abbreviations"
;; 	save-abbrevs t))

;;; IMPORTANT: auto-complete mode
;; SOURCE: `http://emacswiki.org/emacs/AutoComplete'
(require 'auto-complete)

(after "auto-complete"
  (require 'auto-complete-config)

  (setq ac-expand-on-auto-complete t ;; NOTE: expand common portions
	ac-dwim nil ;; NOTE: get pop-ups with docs even if unique
	ac-use-fuzzy t
	ac-fuzzy-enable t
	ac-auto-start nil ;; NOTE: never auto-start the auto-complete menu
	ac-use-menu-map t ;;
	ac-ignore-case 'smart ;; NOTE: always ignore case
	;;ac-auto-show-menu t ;; NOTE: automatically show menu
	;; ac-trigger-key "TAB" ;; NOTE: use TAB for trigger
	ac-source-yasnippet '(action . #'yas-expand))

  ;; NOTE: enables auto-complete globally
  (ac-config-default))

;;; IMPORTANT: emacs snippets
;; SOURCE: `http://www.emacswiki.org/emacs/Yasnippet'
;;(autoload 'yas-minor-mode-on "yasnippet" "Emacs snippets." t)
(require 'yasnippet)

(after "yasnippet"
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  (yas-load-directory "~/.emacs.d/snippets/" t) ;; NOTE: use just-in-time

  (add-hook 'prog-mode-hook #'yas-minor-mode-on))

;;; IMPORTANT: smart completion
(defun smart-completion ()
  "Implement a way of selecting the completion system."
  (cond
   ((boundp 'auto-complete-mode) (auto-complete ac-sources))
   (t (dabbrev-expand nil))))

;;; IMPORTANT: smart tab
(defun smart-tab ()
  "This smart tab is minibuffer compliant: that is, it acts as usual in the minibuffer.

If mark is active, indents region. Else if point is at the end of a symbol, try to expand the symbol. Else indents the current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (smart-completion))
    (if mark-active
	(indent-region (region-beginning) (region-end))
      (if (looking-at "\\_>")
          (smart-completion)
	(indent-for-tab-command)))))

;;; IMPORTANT: default browser
(setq browse-url-new-window-flag t
      browse-url-browser-function #'choose-browser ;; NOTE: ask which browser to use
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

(after "w3m"
  (require 'w3m-cookie)
  (require 'w3m-lnum)
  ;; (require 'w3m-filter)
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
	w3m-command-arguments '("-F")
        w3m-home-page "www.emacswiki.org"
        w3m-default-display-inline-images t ;; NOTE: display images by default
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
  ;; (defvar youtube-videos-directory nil "Directory location to save YouTube videos.")

  ;; (setq youtube-videos-directory "~/Videos/youtube/")

  ;; IMPORTANT: w3m session
  ;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSession'
  ;; (require 'w3m-session)

  ;; (setq w3m-session-file "~/.emacs.d/w3m/session")

  ;; (progn
  ;;   (unless (fboundp 'desktop)
  ;;     (require 'desktop))
  ;;   ;;(add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))

  ;;   ;;(add-hook 'w3m-mode-hook 'w3m-register-desktop-save) ;; NOTE: add w3m-buffers to desktop-save
  ;;   )

  ;; NOTE: w3m mode hooks
  (defun desktop-display (url)
    "Remove trailing whitespace is w3m buffers."
    (let ((buffer-read-only nil))
      (delete-trailing-whitespace)))

  (add-hook 'w3m-display-hook #'desktop-display)

  ;; IMPORTANT: w3m search
  ;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSearch'
  (setq w3m-search-engine-alist '(("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
				  ;; ("github" "http://www.github.com/search?q=%s&ie=utf-8")
				  ("cliki" "http://www.cliki.net/site/search?query=%s" utf-8)
				  ;; ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" utf-8)
				  ;; ("emacswiki" "http://www.google.com/cse?cx=004774160799092323420%%3A6-ff2s0o6yi&q=%s" utf-8)
				  ("wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
				  ("stanford" "http://plato.stanford.edu/search/searcher.py?query=%s" utf-8))))

;; (defun w3m-youtube-video ()
;;   "..."
;;   (interactive)
;;   (let* ((video (browse-url-url-at-point))
;;          (output (format "%s/%s.mp4" youtube-videos-directory video)))
;;     (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" output video)
;;     (emms-play-file output)))

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

(defun custom-w3m-switch-buffer (&optional url)
  "Switch to an existing w3m buffer."
  (interactive)
  (let ((buffers (save-excursion
		   (delq nil (mapcar
			      (lambda (buf)
				(when (buffer-live-p buf)
				  (with-current-buffer buf
				    (and (eq major-mode 'w3m-mode) (buffer-name buf)))))
			      (buffer-list))))))
    (if (get-buffer "*w3m*")
	(if (= (length buffers) 1)
	    (switch-to-buffer (first buffers))
	  (switch-to-buffer (ido-completing-read "w3m session: " buffers)))
      (if (null url)
	  (w3m w3m-home-page)
	(w3m url)))))

;;; IMPORTANT: highlight custom comment tags
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

;; IMPORTANT: google translate
;; SOURCE: `https://github.com/manzyuk/google-translate'
(require 'google-translate)

(after "google-translate"
  (setq google-translate-enable-ido-completion t
	;; google-translate-default-source-language "auto"
	;; google-translate-default-target-language "en"
	google-translate-show-phonetic t)

  (global-set-key (kbd "C-c r") #'google-translate-at-point-reverse)
  (global-set-key (kbd "C-c R") #'google-translate-query-translate-reverse))

;;; IMPORTANT: rainbow delimiters
;; SOURCE: `http://www.emacswiki.org/RainbowDelimiters'
(require 'rainbow-delimiters)

(after "rainbow-delimiters"
  ;; (add-hook 'text-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; IMPORTANT: ibuffer version control
;; SOURCE: `https://github.com/purcell/ibuffer-vc'
(after "ibuffer"
  (require 'ibuffer-vc))

(after "ibuffer-vc"
  (setq ibuffer-formats	'((mark modified read-only vc-status-mini " " (name 18 18 :left :elide) " " filename-and-process))))

;;; IMPORTANT: iedit
;; SOURCE: `http://www.emacswiki.org/emacs/Iedit'
(autoload 'iedit-mode "iedit" "Interactive editing." t)

;;; IMPORTANT: adaptive text wrap
(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap" "Adaptive wrap for text mode buffers." t)

;; (defun turn-on-adaptive-wrap-prefix-mode ()
;;   "Enable `adaptive-wrap-prefix-mode'."
;;   (adaptive-wrap-prefix-mode t))

(add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)

;;; IMPORTANT: projectile
;; SOURCE: `http://www.emacswiki.org/emacs/Projectile'
(require 'projectile)

(after "projectile"
  (projectile-global-mode))

;;; IMPORTANT: smart mode line
;; SOURCE: `https://github.com/Bruce-Connor/smart-mode-line'
;; (when (display-graphic-p)
;;   (require 'smart-mode-line))
(require 'smart-mode-line)

(after "smart-mode-line"
  (add-to-list 'sml/replacer-regexp-list '("^~/.config-scripts/"            ":config:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/.config-scripts/emacs-dir/"  ":emacs:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/.config-scripts/stumpwm-dir" ":stumpwm:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/.config-scripts/bash-dir/"   ":bash:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/.config-scripts/xinit-dir/"  ":xinit:"))
  ;; ---
  (add-to-list 'sml/replacer-regexp-list '("^~/Public/"                 ":public:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Public/scratch/"         ":scratch:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/"              ":docs:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/ANU/"          ":uni:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Reading/"      ":read:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Writing/"      ":write:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Mail/"         ":mail:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/News/"         ":news:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Organisation/" ":org:"))

  (setq sml/name-width 1
	sml/mode-width 0
	sml/shorten-directory t
	sml/theme 'light)

  (sml/setup))

;;; IMPORTANT: expand region
;; SOURCE: `https://github.com/magnars/expand-region.el'
(require 'expand-region)

(global-set-key (kbd "C-x U") #'er/expand-region)

;;; IMPORTANT: journal entries with `org-mode'
;; SOURCE: `http://www.emacswiki.org/emacs/OrgJournal'
;; (require 'org-journal)
(autoload 'org-journal-new-entry "org-journal" "Manage journals with `org-mode'." t)

(after "org-journal"
  (setq org-journal-dir (expand-file-name (concat user-organisation-directory "/journal/"))))

;;; IMPORTANT: switch window
;; SOURCE: ...
(require 'switch-window)

(global-set-key (kbd "C-x o") #'switch-window)

;;; IMPORTANT: deft
;; SOURCE: `http://jblevins.org/projects/deft/'
(autoload 'deft "deft" "Note taking with deft." t)

(after "deft"
  (setq deft-extension "org"
	deft-text-mode 'org-mode
	;; deft-directory (format "%s.deft/" user-organisation-directory)
	def-directory (expand-file-name (concat user-organisation-directory ".deft/"))))

;; ;;; IMPORTANT: ecb
;; (autoload 'ecb-minor-mode "ecb" "..." t)

;; (after "ecb"
;;   (setf ecb-tip-of-the-day-file (expand-file-name (concat user-emacs-directory "ecb-tip-of-day.el"))
;; 	;; ecb-layout-name 'left1
;; 	;; ecb-compile-window-height 12
;; 	ecb-show-sources-in-directories-buffer 'always))

;; ;; TODO: write `ecb-toggle'

;; IMPORTANT: `inf-ruby'
;; IMPORTANT: `rvm'
;; IMPORTANT: `rinari'
;; IMPORTANT: `ruby-tools'
(after "ruby-mode"
  (require 'inf-ruby) ;; inferior ruby
  (require 'rvm) ;; ruby virtual machine
  (require 'rinari) ;; ruby on rails environment
  (require 'ruby-tools)

  (defconst ruby-programming-prefix-key (kbd "C-c C-r") "Ruby programming prefix key.")
  (defvar ruby-programming-map (lookup-key global-map ruby-programming-prefix-key) "Keymap designed for ruby programming.")

  (unless (keymapp ruby-programming-map) (setq ruby-programming-map (make-sparse-keymap)))

  (define-key global-map ruby-programming-prefix-key ruby-programming-map)
  (define-key ruby-programming-map (kbd "r") #'inf-ruby)
  (define-key ruby-programming-map (kbd "a") #'rvm-activate-corresponding-ruby))

;;; IMPORTANT: dictionary
(autoload 'dictionary-search "dictionary" "Look-up definitions of words online." t)

(global-set-key (kbd "C-c d") #'dictionary-search)

(provide 'user-config)
;;; user-config.el ends here

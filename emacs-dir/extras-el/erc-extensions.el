;;; erc-extensions.el --- Custom user commands and functions for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Matthew Ball

;; Author: Matthew Ball <chu@lispux>
;; Keywords: 

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

;; This file provides custom user commands for ERC.
;; It includes operator commands for IRC channel maintenance.

;;; Code:

;; SOURCE: `http://www.emacswiki.org/emacs/ErcUname'
(defun erc-cmd-UNAME (&rest ignore)
  "Display the result of running `uname -a' to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          "[ \n]+$" "" (shell-command-to-string "uname -a"))))
    (erc-send-message
     (concat "{uname -a} [" uname-output "]"))))

(defun erc-cmd-HOWMANY (&rest ignore)
  "Display how many users (and ops) the current channel has."
  (erc-display-message nil 'notice (current-buffer)
		       (let ((hash-table (with-current-buffer (erc-server-buffer) erc-server-users))
			     (users 0)
			     (ops 0))
			 (maphash (lambda (k v)
				    (when (member (current-buffer) (erc-server-user-buffers v))
				      (incf users))
				    (when (erc-channel-user-op-p k)
				      (incf ops)))
				  hash-table)
			 (format "There are %s users (%s ops) in the current channel." users ops))))

(defun erc-cmd-GENTLEMEN ()
  "Send calm down message."
  (erc-send-message "Gentlemen, you can't fight here. This is the war room!"))

;;; IMPORTANT: "Custom" `erc-mode' interactions with outside environment
(defun erc-cmd-MAN (program &rest args)
  "Open the `man' page for PROGRAM."
  (man program))

(defun erc-cmd-WOMAN (program &rest args)
  "Open the `woman' page for PROGRAM."
  (woman program))

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

;; SOURCE: `http://www.emacswiki.org/emacs/ErcChanop'
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsChannelMaintenance'
;; TODO: can achieve this with the chanserv command ...
(defun erc-cmd-OPME ()
  "Request ChanServ to put me into operator status."
  (let ((chan (erc-default-target))
	(nick (erc-current-nick)))
    (erc-message "PRIVMSG" (format "chanserv op %s %s" chan nick) nil)))

(defun erc-cmd-DEOPME ()
  "Deop me from current channel."
  (erc-cmd-DEOP (format "%s" (erc-current-nick))))

;; TODO: can achieve this with the chanserv command ...
(defun erc-cmd-VOICE (nick &optional devoice)
  "Apply (de)voice to user."
  (let ((chan (erc-default-target)))
    (erc-message "PRIVMSG" (format "chanserv %s %s %s" (if devoice "devoice" "voice") chan nick))))

(defun erc-cmd-QUIET (nick &optional unquiet)
  "Apply (un)quiet to user."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick))
	 (host (erc-server-user-host who)))
    (erc-cmd-OPME)
    (erc-send-command (format "MODE %s %sq *!*@%s" chan (if unquiet "-" "+") host))
    (erc-cmd-DEOPME)))

(defun erc-cmd-BAN (nick &optional unban)
  "Ban user NICK from channel specified by `erc-default-target'."
  (let* ((chan (erc-default-target))
	 (who (erc-get-server-user nick))
	 (host (erc-server-user-host who)))
    (erc-send-command (format "MODE %s %sb *!*@%s" chan (if unban "-" "+") host))))

(defun erc-cmd-KICK (nick)
  "Kick NICK from channel."
  (let ((chan (erc-default-target)))
    (erc-send-command (format "KICK %s %s (Kicked)" chan nick))))

(defun erc-cmd-REMOVE (nick)
  "Remove user NICK from current ERC channel."
  (let ((chan (erc-default-target)))
    (erc-send-command (format "REMOVE %s %s Removed" chan nick))))

;; (defun erc-cmd-BANREMOVE (nick)
;;   "Remove and ban user NICK from current ERC channel."
;;   (erc-cmd-OPME)
;;   (erc-cmd-REMOVE nick)
;;   (erc-cmd-BAN nick)
;;   (erc-cmd-DEOPME))

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

;; (erc-user-message "NICKSERV" "Freenode's NickServ allows a user to register a nickname. See: /msg NickServ help")
;; (erc-user-message "MEMOSERV" "Freenode's MemoServ allows a user to send messages to registered users. See: /msg MemoServ help")
;; (erc-user-message "CHANSERV" "Freenode's ChanServ gives normal users the ability to maintain control of a channel. See: /msg ChanServ help")
;;
(erc-user-message "GUIDELINES" "The guidelines for using the Ubuntu channels can be found here: http://wiki.ubuntu.com/IRC/Guidelines")
(erc-user-message "LANGUAGE" "Please watch your language in this channel, thank you.")
(erc-user-message "EMACS" "GNU Emacs is a powerful lisp environment and text editor. See: http://www.gnu.org/software/emacs/")
(erc-user-message "STUMPWM" "StumpWM is a tiling window manager for X11 written in common lisp. See: http://www.nongnu.org/stumpwm/")
(erc-user-message "ORGMODE" "Org-mode is for keeping notes, maintaining TODO lists, project planning, and writing. See: http://orgmode.org/")

;; SOURCE: `fsbot' in #emacs
(erc-user-action "GNU" "takes" "aside and explains why GNU/Linux is the proper term for the operating system commonly referred to as Linux. See: http://www.gnu.org/gnu/linux-and-gnu.html")

(provide 'erc-extensions)
;;; erc-extensions.el ends here

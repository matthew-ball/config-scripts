;;; erc-star-serv.el --- IRC Services

;; Copyright (C) 2014  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
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

;; This essentially defines an interface to Freenode's IRC services.

;;; Code:

(defgroup erc-star-services nil "ERC Freenode services.")

;; IMPORTANT: freenode <*>Serv interaction commands
;; SOURCE: `http://keramida.wordpress.com/2008/11/04/extending-erc-with-emacs-lisp/'
;; SOURCE: `http://paste.ubuntu.com/7629077/'
(defmacro erc-server-services (service &rest args)
  (let ((func (intern (format "erc-cmd-%s" (upcase service))))
	 (doc (format "Provide %s services." service)))
    `(defun ,func (&rest args)
       ,doc
       (let* ((command-args (append (list ,service) args))
	      (command (mapconcat #'identity command-args " ")))
	 (erc-send-command command)))))

(defun service-commands (service)
  (mapcar #'(lambda (elem (car elem)) service)))

;; NOTE: Freenode `ChanServ' commands:
;; ...
(erc-server-services "chanserv") ;; => erc-cmd-CHANSERV

(defcustom user-modes-list '("D" "g" "i" "Q" "R" "w" "z") "User modes." :group 'erc-star-services)
(defcustom channel-modes-list '("b" "C" "c" "e" "f" "F" "g" "i" "I" "j" "k" "l" "L" "m" "n" "p" "P" "q" "Q" "r" "s" "t" "z") "Channel modes." :group 'erc-star-services)

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

(erc-server-services "memoserv") ;; => erc-cmd-MEMOSERV

(defcustom memoserv-commands-alist '((DEL . "Alias for DELETE.")
				     (DELETE . "Deletes memos.")
				     (FORWARD . "Forwards a memo.")
				     (HELP . "Displays contextual help information.")
				     (IGNORE . "Ignores memos.")
				     (LIST . "List of all user memos.")
				     (READ . "Reads a memo.")
				     (SEND . "Sends a memo to a user.")
				     (SENDOPS . "Sends a memo to all ops on a channel."))
  "List of Freenode's `MemoServ' commands." :group 'erc-star-services)


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
;; Other commands: ACC, ACCESS, CERT, DROP, HELP, LISTOWNMAIL, LOGOUT, REGAIN, SETPASS, STATUS, TAXONOMY, VACATION, VERIFY

(erc-server-services "nickserv") ;; => erc-cmd-NICKSERV

(defcustom nickserv-commands-alist '((GHOST . "Reclaims use of a nickname.")
				     (GROUP . "Adds a nickname to user account.")
				     (IDENTIFY . "Identifies to services for a nickname.")
				     (INFO . "Displays information on registrations.")
				     (LISTCHANS . "Lists channels that user has access to.")
				     (REGISTER . "Registers a nickname.")
				     (RELEASE . "Releases a service enforcer.")
				     (SET . "Sets various control flags.")
				     (UNGROUP . "Removes a nicknae from user account."))
  "List of Freenode's `NickServ' commands." :group 'erc-star-services)

;; NOTE: other (nickserv) commands
(add-to-list 'nickserv-commands-alist '(ACC . "Description"))
(add-to-list 'nickserv-commands-alist '(ACCESS . "Description"))
(add-to-list 'nickserv-commands-alist '(CERT . "Description"))
(add-to-list 'nickserv-commands-alist '(DROP . "Description"))
(add-to-list 'nickserv-commands-alist '(HELP . "Description"))
(add-to-list 'nickserv-commands-alist '(LISTOWNMAIL . "Description"))
(add-to-list 'nickserv-commands-alist '(LOGOUT . "Description"))
(add-to-list 'nickserv-commands-alist '(REGAIN . "Description"))
(add-to-list 'nickserv-commands-alist '(SETPASS . "Description"))
(add-to-list 'nickserv-commands-alist '(STATUS . "Description"))
(add-to-list 'nickserv-commands-alist '(TAXONOMY . "Description"))
(add-to-list 'nickserv-commands-alist '(VACATION . "Description"))
(add-to-list 'nickserv-commands-alist '(VERIFY . "Description"))

(provide 'erc-star-serv)
;;; erc-star-serv.el ends here

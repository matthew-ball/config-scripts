;;; init.el --- Configuration initiation

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

;; This is the "entrance" point of the configuration.

;;; Code:

;;; IMPORTANT: user variables
(defgroup user-variables nil "User variables.")

;; NOTE: user directories
(defgroup user-directories nil "User directories." :group 'user-variables)

(defcustom user-shell (getenv "SHELL") "The user's $SHELL environment variable." :group 'user-directories :type 'string)
(defcustom user-browser (getenv "BROWSER") "The user's $BROWSER environment variable." :group 'user-directories :type 'string)
(defcustom user-home-directory (getenv "HOME") "The user's $HOME environment variable." :group 'user-directories :type 'string)
(defcustom user-scripts-directory (format "%s/.conf-scripts/" user-home-directory) "Directory for user's run-time scripts." :group 'user-directories :type 'string)
(defcustom user-documents-directory (format "%s/Documents/" user-home-directory) "Directory for user's documents." :group 'user-directories :type 'string)
(defcustom user-news-directory (format "%s/News/" user-home-directory) "Directory for user's news." :group 'user-directories :type 'string)
(defcustom user-mail-directory (format "%s/Mail/" user-home-directory) "Directory for user's mail." :group 'user-directories :type 'string)
(defcustom user-audio-directory (format "%s/Music/" user-home-directory) "Directory for user's music." :group 'user-directories :type 'string)
(defcustom user-video-directory (format "%s/Videos/" user-home-directory) "Directory for user's videos." :group 'user-directories :type 'string)
(defcustom user-programming-directory (format "%s/Programming/" user-home-directory) "Directory for user's programming files." :group 'user-directories :type 'string)
(defcustom user-projects-directory (format "%s/Projects/" user-home-directory) "Directory for user's projects." :group 'user-directories :type 'string)
(defcustom user-reading-directory (format "%s/Reading/" user-documents-directory) "Directory for user's reading material." :group 'user-directories :type 'string)
(defcustom user-writing-directory (format "%s/Writing/" user-documents-directory) "Directory for user's writing material." :group 'user-directories :type 'string)
(defcustom user-organisation-directory (format "%s/Organisation/" user-documents-directory) "Directory for user's organisation files." :group 'user-directories :type 'string)
(defcustom user-university-directory (format "%s/ANU/" user-documents-directory) "Directory for user's university files." :group 'user-directories :type 'string)

;; NOTE: user files
(defgroup user-files nil "User files." :group 'user-variables)

(defcustom user-org-contacts-file (format "%s/contacts.org" user-organisation-directory) "File for user's contacts." :group 'user-files :type 'string)
(defcustom user-org-university-file (format "%s/school.org" user-organisation-directory) "File for user's university organisation." :group 'user-files :type 'string)
(defcustom user-org-notes-file (format "%s/journal.org" user-organisation-directory) "File for user's notes organisation." :group 'user-files :type 'string)
(defcustom user-org-projects-file (format "%s/projects.org" user-organisation-directory) "File for user's projects organisation." :group 'user-files :type 'string)
(defcustom user-org-archive-file (format "%s/archive.org" user-organisation-directory) "File for user's archive organisation." :group 'user-files :type 'string)

;; NOTE: user details
(setq user-full-name "Matthew Ball") ;; NOTE: set the user full name
(defcustom user-university-id "u4537508" "University ID for the user." :group 'user-variables :type 'string)
(defcustom user-primary-email-address "mathew.ball@gmail.com" "Primary email address for the user." :group 'user-variables :type 'string)
(defcustom user-secondary-email-address (format "%s@%s" user-university-id "anu.edu.au") "Secondary email address for the user." :group 'user-variables :type 'string)

;;; IMPORTANT: load path
;; SOURCE: `http://emacswiki.org/emacs/LoadPath'
(add-to-list 'load-path (expand-file-name user-emacs-directory)) ;; NOTE: add `~/.emacs.d/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "config-el")) ;; NOTE: add `config-el/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "extras-el")) ;; NOTE: add `extras-el/' to `load-path' variable
;;(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "apt-el")) ;; NOTE: add `apt-el/' to `load-path' variable
(add-to-list 'load-path (expand-file-name "~/Programming/lisp/common-lisp/stumpwm/contrib")) ;; TODO: this is for `stumpwm-mode'
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "dictem-1.0.4")) ;; TODO: move to `../extras-el/dictem-el/'
;;(add-to-list 'load-path "/home/chu/Programming/lisp/elisp/wireless/wireless") ;; TODO: move to `../extras-el/wireless-el/'

(let ((default-directory (concat (expand-file-name user-emacs-directory) "elpa/")))
  (if (file-exists-p default-directory) ;; NOTE: if the directory `~/.emacs.d/elpa/' exists ...
      (normal-top-level-add-subdirs-to-load-path) ;; NOTE: ... then recursively add sub-directories to `load-path' variable
    (make-directory (concat (expand-file-name user-emacs-directory) "elpa/")))) ;; NOTE: ... else create directory

;;; IMPORTANT: info path
;; SOURCE: `http://www.emacswiki.org/emacs/InfoPath'
(eval-after-load "info"
  '(add-to-list 'Info-additional-directory-list (expand-file-name "~/Programming/lisp/common-lisp/stumpwm/")))

;; (add-to-list 'Info-additional-directory-list (expand-file-name "~/Programming/lisp/common-lisp/stumpwm/"))

;;; IMPORTANT: common lisp
;; SOURCE: `http://emacswiki.org/emacs/CommonLispForEmacs'
(eval-when-compile (require 'cl-lib))

;;; IMPORTANT: after macro
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode '(progn ,@body)))

;;; IMPORTANT: use configuration files
(defun use-config-file (name)
  "Print a loading message and call `require' on configuration file referred to by \"NAME-config\"."
  (let ((config-file (concat name "-config")))
    (message "Loading %s configuration" name)
    (funcall 'require (intern config-file))))

(use-config-file "appearance")
(use-config-file "general")
(use-config-file "user")
(use-config-file "writing")
(use-config-file "programming")
(use-config-file "key-bindings")

;;; IMPORTANT: shutdown emacs server
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsAsDaemon'
(defun server-shutdown (&rest junk)
  "Shutdown (kill) the GNU Emacs daemon server."
  (interactive)
  (kill-emacs)) ;; NOTE: kill GNU Emacs instance

;;; IMPORTANT: customize configuration file
;; SOURCE: `http://www.emacswiki.org/emacs/CustomFile'
(setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))
(load custom-file 'noerror) ;; NOTE: load custom file

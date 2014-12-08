;;; init.el --- Configuration initiation

;; Copyright (C) 2008-2013  Matthew Ball

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

;; This is the "entrance" point of the (GNU) Emacs configuration.

;;; Code:

;;; IMPORTANT: customize configuration file
;; SOURCE: `http://www.emacswiki.org/emacs/CustomFile'
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(load custom-file 'noerror)

;;; IMPORTANT: user variables
(defgroup user-variables nil "User variables.")

;; NOTE: user directories
(defgroup user-directories nil "User directories." :group 'user-variables)

(defcustom user-shell (getenv "SHELL") "The user's $SHELL environment variable." :group 'user-directories :type 'string)
(defcustom user-browser (getenv "BROWSER") "The user's $BROWSER environment variable." :group 'user-directories :type 'string)
(defcustom user-home-directory (getenv "HOME") "The user's $HOME environment variable." :group 'user-directories :type 'string)
(defcustom user-scripts-directory (format "%s/.config-scripts/" user-home-directory) "Directory for user's run-time scripts." :group 'user-directories :type 'string)
(defcustom user-documents-directory (format "%s/Documents/" user-home-directory) "Directory for user's documents." :group 'user-directories :type 'string)
(defcustom user-news-directory (format "%sNews/" user-home-directory) "Directory for user's news." :group 'user-directories :type 'string)
(defcustom user-mail-directory (format "%sMail/" user-home-directory) "Directory for user's mail." :group 'user-directories :type 'string)
(defcustom user-audio-directory (format "%sMusic/" user-home-directory) "Directory for user's music." :group 'user-directories :type 'string)
(defcustom user-video-directory (format "%sVideos/" user-home-directory) "Directory for user's videos." :group 'user-directories :type 'string)
(defcustom user-programming-directory (format "%sPublic/" user-home-directory) "Directory for user's programming files." :group 'user-directories :type 'string)
(defcustom user-public-directory (format "%sPublic/" user-home-directory) "Directory for user's public files." :group 'user-directories :type 'string)
(defcustom user-projects-directory (format "%sProjects/" user-home-directory) "Directory for user's projects." :group 'user-directories :type 'string)
(defcustom user-reading-directory (format "%sReading/" user-documents-directory) "Directory for user's reading material." :group 'user-directories :type 'string)
(defcustom user-writing-directory (format "%sWriting/" user-documents-directory) "Directory for user's writing material." :group 'user-directories :type 'string)
(defcustom user-organisation-directory (format "%sOrganisation/" user-documents-directory) "Directory for user's organisation files." :group 'user-directories :type 'string)
(defcustom user-university-directory (format "%sANU/" user-documents-directory) "Directory for user's university files." :group 'user-directories :type 'string)
;; NOTE: requires quicklisp (from `http://www.quicklisp.org/')
(defcustom quicklisp-directory (expand-file-name "~/quicklisp/dists/quicklisp/software/") "The directory path to `quicklisp'." :group 'user-directories :type 'string)

;; NOTE: user files
(defgroup user-files nil "User files." :group 'user-variables)

;;(defcustom user-org-contacts-file (format "%scontacts.org" user-organisation-directory) "File for user's contacts." :group 'user-files :type 'string)
(defcustom user-org-university-file (format "%sschool.org" user-organisation-directory) "File for user's university organisation." :group 'user-files :type 'string)
(defcustom user-org-notes-file (format "%snotes.org" user-organisation-directory) "File for user's notes organisation." :group 'user-files :type 'string)
(defcustom user-org-journal-file (format "%sjournal.org" user-organisation-directory) "File for user's journal." :group 'user-riles :type 'string)
;; TODO: the projects file is obsolete and should be removed
;;(defcustom user-org-projects-file (format "%sprojects.org" user-organisation-directory) "File for user's projects organisation." :group 'user-files :type 'string)
(defcustom user-org-archive-file (format "%sarchive.org" user-organisation-directory) "File for user's archive organisation." :group 'user-files :type 'string)

;; NOTE: user details
(setq user-full-name (getenv "USER_FULL_NAME")) ;; NOTE: set the user full name
(defcustom user-university-id (getenv "USER_UNI_ID") "University ID for the user." :group 'user-variables :type 'string)
(defcustom user-irc-nickname (or (getenv "USER_IRC_NICKNAME") (getenv "USER")) "User IRC nickname." :group 'user-variables :type 'string)
(defcustom user-primary-email-address (getenv "USER_PRIMARY_EMAIL") "Primary email address for the user." :group 'user-variables :type 'string)
(defcustom user-secondary-email-address (format "%s@%s" user-university-id (getenv "USER_UNI")) "Secondary email address for the user." :group 'user-variables :type 'string)

;; TODO: define `programming-map' and `writing-map'

;;; IMPORTANT: common lisp
;; SOURCE: `http://emacswiki.org/emacs/CommonLispForEmacs'
(eval-when-compile (require 'cl-lib))

;;; IMPORTANT: after
(defmacro after (mode &rest body)
  "A conveniant macro for defining user-settings and functions for major-modes."
  (declare (indent defun)) ;; NOTE: this is for the lisp reader ...
  `(eval-after-load ,mode '(progn ,@body)))

;;; IMPORTANT: bind key
;; NOTE: this would be nice and simple, but wouldn't save much (unless it did an `fboundp' search)
;; ERROR: doesn't work
(defmacro bind-key (key &rest body)
  `(global-set-key (kbd ,key) ,@body))

;;; IMPORTANT: load path
;; SOURCE: `http://emacswiki.org/emacs/LoadPath'
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "config-el"))
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "extras-el"))
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "extras-el/erc-extras/"))
(add-to-list 'load-path (concat (expand-file-name user-public-directory) "stumpwm-contrib/util/swm-emacs/"))

(let ((default-directory (concat (expand-file-name user-emacs-directory) "elpa/")))
  (when (file-exists-p default-directory) ;; NOTE: if the directory `~/.emacs.d/elpa/' exists ...
    (normal-top-level-add-subdirs-to-load-path))) ;; NOTE: ... then recursively add sub-directories to `load-path' variable

;;; IMPORTANT: info path
;; SOURCE: `http://www.emacswiki.org/emacs/InfoPath'
(after "info"
  (add-to-list 'Info-default-directory-list (expand-file-name "~/.emacs.d/info"))
  ;; TODO: ...
  (add-to-list 'Info-default-directory-list (expand-file-name (concat user-public-directory "stumpwm/")))
  (add-to-list 'Info-default-directory-list (expand-file-name (concat user-public-directory "slime/doc/"))))

;;; IMPORTANT: package manager
;; SOURCE: `http://emacswiki.org/emacs/ELPA'
(require 'package)

(after "package"
  (package-initialize)
  (setq ;;package-enable-at-startup nil
	load-prefer-newer t)

  ;; NOTE: set download repositories
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
			   ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			   ;; ("melpa-unstable" . "http://melpa-unstable.milkbox.net/packages/")
			   ;; ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("gnu" . "http://elpa.gnu.org/packages/"))))

;; SOURCE: `http://hastebin.com/yidodunufo.lisp'
(defun ensure-package-installed-p (package)
  "Assure PACKAGE is installed."
  (if (package-installed-p package)
      nil
    (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	(package-install package)
      package)))

(defun ensure-packages-installed-p (&rest packages)
  "Assure every package is installed, ask for installation if it's not.

Return a list of installed packages or nil for every skipped package."
  (mapcar #'ensure-package-installed-p packages))

;; NOTE: either `~/.emacs.d/elpa/' exists or refresh the package contents
(or (file-exists-p package-user-dir) (package-refresh-contents))

(ensure-packages-installed-p 'adaptive-wrap 'auto-complete 'bbdb 'browse-kill-ring 'dash 'deft 'diminish 'dictionary 'ebib 'elisp-slime-nav
			     'epl 'erc-hl-nicks 'find-file-in-project 'flx 'flx-ido 'fuzzy 'geiser 'gh 'gist 'git-commit-mode 'git-rebase-mode
			     'google-translate 'haskell-mode 'highlight-indentation 'ibuffer-vc 'inf-ruby 'rinari 'rvm 'ruby-tools
			     'ido-ubiquitous 'idomenu 'iedit 'logito 'magit 'nose 'org-journal 'paredit 'pcache 'pkg-info 'popup 'projectile
			     'rainbow-delimiters 's 'smart-mode-line 'smex 'tabulated-list 'undo-tree 'w3m 'yasnippet)

;;; IMPORTANT: use configuration files
;; NOTE: requires that config files are in `load-path' already
(defcustom user-config-files '("appearance"
			       "general"
			       "programming"
			       "user"
			       "key-bindings") "User configuration files.")

(defun use-config-file (name)
  "Print a loading message and call `require' on configuration file referred to by \"NAME-config\"."
  (let ((config-file (concat name "-config")))
    (message "Loading %s configuration" name)
    (funcall #'require (intern config-file))))

(mapc #'use-config-file user-config-files)

;;; IMPORTANT: shutdown emacs server
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsAsDaemon'
(defun server-shutdown (&rest junk)
  "Shutdown (kill) the GNU Emacs daemon server."
  (interactive)
  (when (y-or-n-p "Kill emacs daemon? ")
    (kill-emacs)))

;; (setq initial-buffer-choice user-org-notes-file)


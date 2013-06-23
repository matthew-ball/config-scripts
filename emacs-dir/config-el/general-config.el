;;; general-config.el --- Configuration for general settings

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

;; Configuration for general settings.

;;; Code:

;; IMPORTANT: default variable values
(setq inhibit-startup-message t ;; NOTE: turn off startup message
      inhibit-startup-echo-area-message t ;; NOTE: turn off startup echo area message
      initial-scratch-message (concat ";; For information about "
      				      (substring (emacs-version) 0 16)
      				      " and the GNU system, type C-h C-a.\n\n") ;; NOTE: initial scratch message
      completion-ignore-case t ;; NOTE: ignore case in auto-completing text
      read-file-name-completion-ignore-case t ;; NOTE: ignore cases in filenames
      ;; enable-recursive-minibuffers t ;; NOTE: ...
      auto-compression-mode t ;; NOTE: automatically parse an archive
      message-log-max 1000 ;; NOTE: maximum number of lines to keep in the message log buffer (default is 100)
      show-trailing-whitespace t ;; NOTE: show trailing whitespace
      scroll-margin 0 ;; NOTE: use ...
      scroll-conservatively 10000 ;; NOTE: ... smooth scrolling
      scroll-preserve-screen-position t ;; NOTE: preserve screen position with C-v/M-v
      auto-save-interval 1000 ;; NOTE: change auto-save interval from 300 to 1000 keystrokes
      sentence-end-double-space 'nil ;; NOTE: sentences end with a single space
      echo-keystrokes 0.1 ;; NOTE: see what you are typing
      use-dialog-box nil ;; NOTE: do not use mouse
      suggest-key-bindings nil) ;; NOTE: do not show respective key-bindings

(setq-default scroll-up-aggressively 0 ;; NOTE: local variables for smooth scrolling
	      scroll-down-aggressively 0) ;; NOTE: local variables for smooth scrolling

;;; IMPORTANT: default auto-mode list
(add-to-list 'auto-mode-alist '("README$" . org-mode)) ;; NOTE: open `README' files in `org-mode'
(add-to-list 'auto-mode-alist '("NEWS$" . org-mode)) ;; NOTE: open `NEWS' files in `org-mode'
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; NOTE: open `*.org' files in `org-mode'
;; (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode)) ;; NOTE: open `*.hs' files in `haskell-mode'
;; (add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)) ;; NOTE: open `*.cabal' files in `haskell-cabal-mode'
;; (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode)) ;; NOTE: open `*.tex' files in `latex-mode'
;; (add-to-list 'auto-mode-alist '(".screenrc" . shell-script-mode)) ;; NOTE: open .screenrc in `shell-script-mode'
;; (add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode)) ;; NOTE: open .emacs in `emacs-lisp-mode'
;; (add-to-list 'auto-mode-alist '(".stumpwmrc$" . stumpwm-mode)) ;; NOTE: open .stumpwmrc in `stumpwm-mode'
;; (add-to-list 'auto-mode-alist '(".conkerorrc/" . javascript-mode)) ;; NOTE: open `.conkerorrc/' files in `javascript-mode'
;; (add-to-list 'auto-mode-alist '("bashrc" . shell-script-mode)) ;; NOTE: open `.bashrc' file in `shell-script-mode'
;; (add-to-list 'auto-mode-alist '("stumpwmrc" . common-lisp-mode)) ;; NOTE: open `.stumpwmrc' file in `common-lisp-mode'
;; (add-to-list 'auto-mode-alist '("stumpwmrc" . stumpwm-mode)) ;; NOTE: open `.stumpwmrc' file in `stumpwm-mode'

;;;IMPORTANT: uniquify (unique buffer names)
;; SOURCE: `http://emacswiki.org/emacs/uniquify'
(require 'uniquify)

(after "uniquify"
  (setq uniquify-buffer-name-style 'reverse
	uniquify-separator "/"
	uniquify-after-kill-buffer-p t ;; NOTE: rename after killing uniquified
	uniquify-ignore-buffers-re "^\\*")) ;; NOTE: don't muck with special buffers

;;; IMPORTANT: ido mode
;; SOURCE: `http://emacswiki.org/emacs/InteractivelyDoThings'
(autoload 'ido-switch-buffer "ido" "Interactively do thing." t)

(after "ido"
  (ido-mode 'both) ;; NOTE: turn on interactive mode (files and buffers)

  (setq ido-enable-flex-matching t ;; NOTE: enable fuzzy matching
	ido-use-virtual-buffers t ;; NOTE: keep buffers around
	ido-create-new-buffer 'always ;; NOTE: create new buffers (if name does not exist)
	ido-everywhere t ;; NOTE: enable ido everywhere
	;; ido-use-filename-at-point t
	;; ido-use-url-at-point t
	;; ido-save-directory-list-file (expand-file-name (concat user-emacs-directory "ido-cache"))
	;; ido-save-directory-list-file (expand-file-name (concat user-emacs-directory "ido-directory-list"))
	;; ido-ignore-directories '("." "..")
	;; ido-ignore-files '(".")
	ido-ignore-extensions t ;; NOTE: ignore extentions
	;; TODO: can clean up the following ...
	ido-ignore-buffers '("\\` " "^\#[#]?" "^\*Mess" "^\*Back" ".*Completion" "^\*Ido"
			     "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*") ;; NOTE: ignore buffers matching regexp
	ido-work-directory-list `(,(expand-file-name user-home-directory)
				  ,(expand-file-name user-documents-directory)
				  ,(expand-file-name user-university-directory)
				  ,(expand-file-name user-organisation-directory))
	ido-case-fold t ;; NOTE: enable case-insensitivity
	ido-enable-last-directory-history t ;; NOTE: enable directory history
	ido-max-work-directory-list 500 ;; NOTE: remember last used directories
	ido-max-work-file-list 500 ;; NOTE: ... and files
	ido-max-prospects 7 ;; NOTE: don't spam the mini buffer
	ido-show-dot-for-dired t ;; NOTE: enable `dired' with `ido-mode'
	confirm-nonexistent-file-or-buffer nil ;; NOTE: the confirmation is rather annoying
	))

(defun recentf-ido-find-file (&rest junk) ;; NOTE: replace recentf-open-files
  "Find a recent file using `ido-mode'."
  (interactive)
  (unless (featurep 'recentf)
    (require 'recentf))
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file (find-file file))))

;;; IMPORTANT: recent files
;; SOURCE: `http://emacswiki.org/emacs/RecentFiles'
;; (autoload 'recentf-mode "recentf" "..." t)
(require 'recentf) ;; TODO: change to an autoload

(after "recentf"
  (setq recentf-save-file (concat (expand-file-name user-emacs-directory) "recent-files") ;; NOTE: recently saved files
	recentf-max-saved-items 500 ;; NOTE: maximum saved items is 500
	recentf-max-menu-items 25) ;; NOTE: maximum 25 files in menu

  (recentf-mode t))

;;; IMPORTANT: ibuffer
;; SOURCE: `http://www.emacswiki.org/emacs/IbufferMode'
(autoload 'ibuffer "ibuffer" "..." t)

(after "ibuffer"
  (require 'ibuf-ext)

  ;; TODO: investigate `ibuffer-directory-abbrev-list'
  ;; (setq ibuffer-directory-abbrev-alist
  ;; 	'((expand-file-name "~/Documents/" . "Documents")
  ;; 	  (expand-file-name "~/Programming" . "Programming")))

  (add-to-list 'ibuffer-never-show-predicates "^\\*Minibuf-0\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Minibuf-1\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Ibuffer\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*AgendaCommands\\*$")
  ;;(add-to-list 'ibuffer-never-show-predicates (lambda () (when (eq major-mode minibuffer-inactive-mode)))) ;; WILL IT BLEND?!

  (setq ibuffer-saved-filter-groups
	`(("default"
	   ("Configuration" ;; NOTE: run-time configuration related buffers
	    (or (filename . ,(expand-file-name user-emacs-directory))
		(filename . ,(expand-file-name user-scripts-directory))))
	   ("University" ;; NOTE: university related buffers
	    (filename . ,(expand-file-name user-university-directory)))
	   ("Reading" ;; NOTE: reading (material and notes) related buffers
	    (or (filename . ,(expand-file-name user-reading-directory))
		(mode . doc-view-mode)))
	   ("Writing" ;; NOTE: writing related buffers
	    (or (filename . ,(expand-file-name user-writing-directory))
		(mode . reftex-toc-mode)
		(mode . bibtex-mode)
		(mode . ebib-log-mode)
		(mode . ebib-index-mode)
		(mode . ebib-entry-mode)
		(mode . deft-mode)
		(mode . dictem-mode)
		(name . "^\\*Ebib-edit\\*$")))
	   ("Projects" ;; NOTE: project related buffers
	    (filename . ,(expand-file-name user-projects-directory)))
	   ("Programming" ;; NOTE: programming related buffers
	    (or (filename . ,(expand-file-name user-programming-directory))
		(mode . c-mode)
		(mode . c++-mode)
		(mode . haskell-mode)
		(mode . inferior-haskell-mode)
		(mode . python-mode)
		(mode . inferior-python-mode)
		(mode . nrepl-mode)
		(name . "^\\*nrepl-error\\*$")
		(mode . lisp-mode)
		(mode . common-lisp-mode)
		(mode . emacs-lisp-mode)
		(mode . inferior-lisp-mode)
		(mode . inferior-emacs-lisp-mode)
		(mode . nxml-mode)
		(mode . ada-mode)
		(mode . makefile-gmake-mode)
		(mode . perl-mode)
		(mode . slime-fuzzy-completions-mode)
		(mode . slime-xref-mode)
		(mode . html-mode)
		(mode . css-mode)
		(mode . javascript-mode)
		(mode . scheme-mode)
		(mode . inferior-scheme-mode)
		(mode . compilation-mode)
		(mode . shell-script-mode)
		(mode . sh-mode)
		(mode . conf-unix-mode)
		(mode . conf-space-mode)
		(mode . gist-list-mode)
		(mode . gist-menu-mode)
		(mode . slime-mode)
		(mode . inferior-slime-mode)
		(mode . repl-mode)
		(mode . sldb-mode)
		(name . "^\\*slime-events\\*$")
		(name . "^\\*slime-threads\\*$")
		(name . "^\\*slime-connections\\*$")
		(name . "^\\*slime-repl sbcl\\*$")
		(name . "^\\*slime-compilation\\*$")
		(name . "^\\*slime-description\\*$")
		(name . "^\\*inferior-lisp\\*$")
		(name . "^\\*Compile-Log\\*$")))
	   ("Version Control" ;; NOTE: version control related buffers
	    (or (mode . diff-mode)
		(mode . magit-status-mode)
		(mode . magit-key-mode)
		(mode . magit-log-edit-mode)
		(mode . magit-diff-mode)
		(mode . vc-mode)
		(mode . vc-dir-mode)
		(mode . vc-log-entry-mode)
		(name . "^\\*magit-process\\*$")
		(name . "^\\*magit-log-edit\\*$")))
	   ("Organisation" ;; NOTE: org-mode related buffers
	    (or (mode . org-mode)
		(mode . org-agenda-mode)
		(mode . calendar-mode)
		(mode . diary-mode)
		(filename . ,(expand-file-name user-organisation-directory))))
	   ("IRC" ;; NOTE: irc related buffers
	    (or (mode . erc-mode)
		(mode . rcirc-mode)))
	   ("Web Browser" ;; NOTE: w3m related buffers
	    (mode . w3m-mode))
	   ("File Manager" ;; NOTE: dired related buffers
	    (or (mode . dired-mode)
		(mode . tar-mode)
		(name . "^\\*Dired log\\*$")))
	   ("Shell" ;; NOTE: shell related buffers
	    (or (mode . eshell-mode)
		(mode . shell-mode)
		(mode . term-mode)
		(mode . locate-mode)
		(mode . tex-shell-mode)
		(name . "^\\*Shell Command Output\\*$")))
	   ("Games" ;; NOTE: buffers related to games
	    (or (mode . sudoku-mode)
		(mode . solitaire-mode)
		(mode . snake-mode)
		(mode . doctor-mode)))
	   ("Mathematics and Science" ;; NOTE: buffers related to mathematics and science
	    (or (mode . calculator-mode)
		(mode . calc-mode)
		(mode . calc-trail-mode)
		(mode . maxima-mode)
		(mode . inferior-maxima-mode)
		(name . "^\\*ESS\\*$")))
	   ("Mail and News" ;; NOTE: mail (and news) related buffers
	    (or ;;(newsticker-treeview-mode)
	     ;;(newsticker-plainview-mode)
	     (mode . gnus-group-mode)
	     (mode . gnus-topic-mode)
	     (mode . gnus-browse-mode)
	     (mode . gnus-summary-mode)
	     (mode . gnus-server-mode)
	     (mode . gnus-article-mode)
	     (mode . gnus-edit-form-mode)
	     (mode . message-mode)
	     (mode . bbdb-mode)
	     (name . "^\\*gnus trace\\*$")
	     (filename . ".newsrc-dribble$")))
	   ("Information" ;; NOTE: help and information related buffers
	    (or (mode . info-mode)
		(mode . Info-mode)
		(mode . apropos-mode)
		(mode . Help-Mode)
		(mode . help-mode)
		(mode . Man-mode)
		(mode . woman-mode)
		(mode . occur-mode)
		(mode . grep-mode)
		(name . "^\\*WoMan-Log\\*$")
		(name . "^\\*Org Processes\\*$")))
	   ("Process Manager" ;; NOTE: process management related buffers
	    (or (mode . proced-mode)
		(mode . process-menu-mode)))
	   ("Package Management" ;; NOTE: package management related buffers
	    (or (mode . apt-mode)
		(mode . package-menu-mode)
		(name . "^\\*Package Info\\*$")))
	   ("Miscellaneous" ;; NOTE: miscellaneous special buffers
	    (or (mode . customize-mode)
		(mode . Custom-mode)
		(mode . completion-list-mode)
		(mode . finder-mode)
		(mode . color-theme-mode)
		(mode . browse-kill-ring-mode)
		(name . "\\*scratch\\*$")
		(name . "\\*Messages\\*$")
		(name . "\\*Backtrace\\*$")
		(name . "\\*Keys\\*$")
		(name . "\\*Disabled Command\\*$")
		(name . "\\*Apropos\\*$")
		(name . "\\*tramp/ssh ssh\\*$")
		(name . "\\*Help\\*$")
		(name . "\\*Org PDF LaTeX Output\\*$"))))))

  (setq ibuffer-show-empty-filter-groups nil ;; NOTE: do not display empty groups
	;; ibuffer-default-sorting-mode 'major-mode ;; NOTE: sort buffers by `major-mode'
	ibuffer-default-sorting-mode 'filename/process ;; NOTE: sort buffers by `buffer-file-name'
	ibuffer-sorting-mode 'recency
	ibuffer-expert t ;; NOTE: do not ask for confirmation
	;;ibuffer-shrink-to-minimum-size t
	;;ibuffer-default-shrink-to-minimum-size t ;; NOTE: minimize the size of the ibuffer window
	;;ibuffer-use-other-window t
	ibuffer-always-show-last-buffer t ;; NOTE: always display the previous buffer
	ibuffer-display-summary t ;; NOTE: summarize ibuffer columns
	ibuffer-case-fold-search t ;; NOTE: ignore case when searching
	ibuffer-old-time 72 ;; NOTE: number of hours before a buffer is considered "old"
	ibuffer-truncate-lines t ;; NOTE: do not display continuation lines
	ibuffer-use-header-line t) ;; NOTE: display a line containing current filters

  (add-hook 'ibuffer-mode-hook (lambda ()
				 (ibuffer-auto-mode 1) ;; NOTE: automatically update buffer list
				 (ibuffer-switch-to-saved-filter-groups "default"))))

;;; IMPORTANT: find file at point
;; SOURCE: `http://emacswiki.org/emacs/FindFileAtPoint'
(autoload 'find-file-at-point "ffap" "" t)

;;; IMPORTANT: tramp
;; SOURCE: `http://emacswiki.org/cgi-bin/wiki/TrampMode'
(autoload 'tramp "tramp" "Remote file manipulation with Tramp." t)

(after "tramp"
  (setq tramp-default-method "ssh")) ;; NOTE: use ssh for tramp

;;; IMPORTANT: enable commands
;; SOURCE: `http://www.emacswiki.org/emacs/DisabledCommands'
(setq disabled-command-function nil)

;;; IMPORTANT: mini-buffer
(file-name-shadow-mode t) ;; NOTE: be smart about filenames in the mini-buffer
(fset 'yes-or-no-p 'y-or-n-p) ;; NOTE: changes all "yes/no" questions to "y/n"
(setq read-buffer-completion-ignore-case t) ;; NOTE: ignore case when reading a buffer name

;; NOTE: don't let the cursor go into minibuffer prompt
;;(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;;; IMPORTANT: selection
(delete-selection-mode 1) ;; NOTE: replace (delete) selected region

;;; IMPORTANT: save mini-buffer history
;; SOURCE: `http://emacswiki.org/emacs/SaveHist'
(require 'savehist)

(savehist-mode t) ;; NOTE: keep mini buffer history between session

(setq savehist-file (concat (expand-file-name user-emacs-directory) "minibuffer-history"))

;;; IMPORTANT: save place
;; SOURCE: `http://www.emacswiki.org/emacs/SavePlace'
(require 'saveplace)

(setq-default save-place t)

(setq save-place-file (concat (expand-file-name user-emacs-directory) "save-place"))

;;; IMPORTANT: temporary buffers
(temp-buffer-resize-mode t) ;; NOTE: auto-fit the *Help* buffer to its contents

;;; IMPORTANT: backups
(setq-default delete-old-versions t) ;; NOTE: delete excess file backups silently

(setq ;; backup-by-copying t ;; NOTE: don't clobber symlinks
      ;; backup-inhibited t ;; NOTE: disable backup
      ;; auto-save-default nil ;; NOTE: disable auto save
      backup-directory-alist `(("." . ,(concat (expand-file-name user-emacs-directory) "save-files"))) ;; NOTE: don't litter the file-system
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ;; NOTE: use versioned backups

;;; IMPORTANT: desktop save mode
;; SOURCE: `http://emacswiki.org/emacs/DeskTop'
(require 'desktop)

(after "desktop"
  ;; (restore-desktop-session) ;; NOTE: this is not asked so that `emacs --daemon' works
  ;; (desktop-save-mode 1) ;; NOTE: enable desktop save mode
  (setq desktop-path (list (expand-file-name user-emacs-directory))
	desktop-dirname (expand-file-name user-emacs-directory)
	desktop-base-file-name "emacs-desktop"
	desktop-restore-eager 10
	desktop-buffers-not-to-save "\\(\\.newsrc-dribble\\|\\.bbdb\\)$"
	history-length 300)

  (setq desktop-globals-to-save ;; NOTE: save variables to the desktop file (for lists specify the length of the saved data also)
	(append '((extended-command-history . 30)
		  (file-name-history        . 100)
		  (grep-history             . 30)
		  (compile-history          . 30)
		  (minibuffer-history       . 50)
		  (query-replace-history    . 60)
		  (read-expression-history  . 60)
		  (regexp-history           . 60)
		  (regexp-search-ring       . 20)
		  (search-ring              . 20)
		  (shell-command-history    . 50)
		  tags-file-name
		  register-alist)))

  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

(defun restore-desktop-session (&rest junk)
  "Query the user to start the previous saved session or not."
  (interactive)
  ;;(desktop-save-mode 1) ;; NOTE: enable desktop-save-mode also
  (desktop-read);; NOTE: read the desktop file
  ;;(message (format "Restoring %s session." desktop-base-file-name))
  )

(defun save-desktop-session (&rest junk)
  "Saves the current GNU Emacs session."
  (interactive)
  ;;(desktop-save)
  (desktop-save-in-desktop-dir))

(defun emacs-process-p (pid) ;; NOTE: over-ride stale lock
  "If PID is the process ID of an emacs process, return t, else nil. Also returns nil if PID is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t) pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead GNU Emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;;; IMPORTANT: browse kill ring
;; SOURCE: `http://www.emacswiki.org/BrowseKillRing'
(autoload 'browse-kill-ring "browse-kill-ring" "Browse the `kill-ring'." t)

(after "browse-kill-ring"
  (browse-kill-ring-default-keybindings))

;;; IMPORTANT: auto refresh buffers
;; SOURCE: `http://www.emacswiki.org/emacs/AutoRevertMode'
(global-auto-revert-mode 1) ;; NOTE: auto refresh buffers

;; NOTE: auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;; IMPORTANT: eshell
;; SOURCE: `http://emacswiki.org/emacs/CategoryEshell'
(autoload 'eshell "eshell" "GNU Emacs Shell." t)

(after "eshell"
  (require 'esh-mode)
  (require 'esh-util)

  (setq eshell-prompt-function 'eshell-prompt
	eshell-ls-use-in-dired t  ;; NOTE: use eshell to read directories in `dired'
	eshell-highlight-prompt nil
	eshell-prompt-regexp "^[^#$\n]*[#$] " ;; NOTE: fix shell auto-complete
	eshell-cmpl-cycle-completions nil ;; NOTE: avoid cycle-completion
	eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\|\\.elc\\)/\\'" ;; NOTE: ignore file prefixes
	eshell-save-history-on-exit t ;; NOTE: save eshell history on exit
	eshell-where-to-jump 'begin ;; NOTE: jump to beginning of line
	eshell-review-quick-commands nil ;; NOTE: enable quick review
	eshell-smart-space-goes-to-end t) ;; NOTE: save buffer history

  (setq eshell-modules-list '(eshell-alias
			      eshell-banner
			      eshell-basic
			      eshell-cmpl
			      eshell-dirs
			      eshell-glob
			      eshell-hist
			      eshell-ls
			      eshell-pred
			      eshell-prompt
			      eshell-script
			      eshell-term
			      eshell-tramp
			      eshell-unix))

  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))

(defun eshell/git-branch (&rest junk)
  "Return the current git branch, if applicable."
  (let ((branch (shell-command-to-string "git branch")))
    (string-match "^\\* \\(.*\\)" branch)
    (match-string 1 branch)))

(defun eshell/clear (&rest junk)
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/deb (&rest args)
  "Interface with a debian apt system."
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
      (find
       (format "apt-cache search %s" find))
      (installed
       (format "dlocate -l %s | grep '^.i'" installed))
      (list-files
       (format "dlocate -L %s | sort" list-files))
      (show
       (format "apt-cache show %s" show))
      (version
       (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
      (where
       (format "dlocate %s" where))))
    t)))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun eshell-prompt ()
  "Fancy prompt for `eshell'."
  (concat
   (with-face user-login-name :foreground "red")
   "@"
   (with-face (car (split-string system-name "\\.")) :foreground "green")
   ":"
   (with-face (eshell/pwd) :foreground "blue" :weight 'bold)
   (if (string= (substring (shell-command-to-string "git branch") 0 1) "f")
       " "
     (with-face (concat " (" (eshell/git-branch) ")") :foreground "yellow" :weight 'bold))
   (if (= (user-uid) 0)
       (with-face " #" :foreground "red")
     "$")
   " "))

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;;; IMPORTANT: directory editor (extensions)
;; SOURCE: `http://emacswiki.org/emacs/DiredMode'
(autoload 'dired "dired" "File manager in Emacs." t)

(after "dired"
  (require 'dired-x)

  (add-hook 'dired-mode-hook (lambda ()
			       (turn-on-dired-find-alternate-file)
			       ;; NOTE: set `dired-x' buffer-local variables here
			       (dired-omit-mode)))

  ;; NOTE: make sizes human-readable by default, sort version numbers correctly, and put dotfiles and capital-letters first
  (setq dired-listing-switches "-DaGghlv --group-directories-first --time-style=long-iso"
	dired-dwim-target t ;; NOTE: try suggesting dired targets
	dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; NOTE: hide un-interesting files in dired
	dired-omit-extensions (append dired-latex-unclean-extensions
				      dired-tex-unclean-extensions
				      dired-patch-unclean-extensions
				      dired-bibtex-unclean-extensions
				      dired-texinfo-unclean-extensions)
	;; NOTE: dired application management
	dired-guess-shell-alist-user (list
				      (list "\\.pdf$" "evince")
				      (list "\\.PDF$" "evince")
				      (list "\\.doc$" "openoffice.org")
				      (list "\\.docx$" "openoffice.org")
				      (list "\\.DOC$" "openoffice.org"))))

(defun turn-on-dired-find-alternate-file (&rest junk)
  "Enable `dired-find-alternate-file' function and modifies `dired-up-directory'."
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; NOTE: was `dired-advertised-find-file'
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ;; NOTE: was `dired-up-directory'

;;; IMPORTANT: general functions
(defun eval-and-replace ()
  "Replace the preceding s-expression with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun switch-to-scratch (&rest junk)
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

;; SOURCE: `http://wenshanren.org/?p=298'
(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(provide 'general-config)
;;; general-config.el ends here

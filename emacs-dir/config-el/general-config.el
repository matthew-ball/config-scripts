;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/general-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: user variables
;; NOTE: user directories
(defcustom user-shell (getenv "SHELL") "The user's $SHELL environment variable.")
(defcustom user-browser (getenv "BROWSER") "The user's $BROWSER environment variable.")
(defcustom user-home-directory (getenv "HOME") "The user's $HOME environment variable.")
(defcustom user-scripts-directory (format "%s/.conf-scripts/" user-home-directory) "Directory for user's run-time scripts.")
(defcustom user-documents-directory (format "%s/Documents/" user-home-directory) "Directory for user's documents.")
(defcustom user-news-directory (format "%s/News/" user-home-directory) "Directory for user's news.")
(defcustom user-mail-directory (format "%s/Mail/" user-home-directory) "Directory for user's mail.")
(defcustom user-audio-directory (format "%s/Music/" user-home-directory) "Directory for user's music.")
(defcustom user-video-directory (format "%s/Videos/" user-home-directory) "Directory for user's videos.")
(defcustom user-programming-directory (format "%s/Programming/" user-home-directory) "Directory for user's programming files.")
(defcustom user-projects-directory (format "%s/Projects/" user-home-directory) "Directory for user's projects.")
(defcustom user-reading-directory (format "%s/Reading/" user-documents-directory) "Directory for user's reading material.")
(defcustom user-writing-directory (format "%s/Writing/" user-documents-directory) "Directory for user's writing material.")
(defcustom user-organisation-directory (format "%s/Organisation/" user-documents-directory) "Directory for user's organisation files.")
(defcustom user-university-directory (format "%s/ANU/" user-documents-directory) "Directory for user's university files.")

;; NOTE: user files
(defcustom user-org-contacts-file (format "%s/contacts.org" user-organisation-directory) "File for user's contacts.")
(defcustom user-org-university-file (format "%s/school.org" user-organisation-directory) "File for user's university organisation.")
(defcustom user-org-notes-file (format "%s/journal.org" user-organisation-directory) "File for user's notes organisation.")
(defcustom user-org-projects-file (format "%s/projects.org" user-organisation-directory) "File for user's projects organisation.")
(defcustom user-org-archive-file (format "%s/archive.org" user-organisation-directory) "File for user's archive organisation.")

;; NOTE: user details
(setq user-full-name "Matthew Ball") ;; NOTE: set the user full name
(defcustom user-university-id "u4537508" "University ID for the user.")
(defcustom user-primary-email-address "mathew.ball@gmail.com" "Primary email address for the user.")
(defcustom user-secondary-email-address (format "%s@%s" user-university-id "anu.edu.au") "Secondary email address for the user.")

;;; COMMENT: user functions
(defun eval-and-replace (&rest junk)
  "Replace the preceding s-expression with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; TODO: this works, but as I no longer have my whole dot emacs file in a single location, it serves no purpose
;; (defun switch-to-dot-emacs (&rest junk)
;;   "Switch to init.el file (or evaluate the buffer if the init.el file is present)."
;;   (interactive)
;;   ;; (config files)
;;   (if (equal (buffer-name) "init.el")
;;       (eval-buffer) ;; evaluate the current buffer
;;     (find-file (concat (expand-file-name user-emacs-directory) "init.el")))) ;; switch to the init.el file

;;; COMMENT: initial minibuffer message
;; (defun display-startup-echo-area-message (&rest junk)
;;   "Clear the message buffer initially."
;;   (message ""))

;;; COMMENT: default variable values
(setq inhibit-startup-message t ;; NOTE: turn off startup message
      inhibit-startup-echo-area-message t ;; NOTE: turn off startup echo area message
      initial-scratch-message (concat ";; For information about "
				      (substring (emacs-version) 0 16)
				      " and the GNU system, type C-h C-a.\n\n") ;; NOTE: initial scratch message
      completion-ignore-case t ;; NOTE: ignore case in auto-completing text
      read-file-name-completion-ignore-case t ;; NOTE: ignore cases in filenames
      auto-compression-mode 1 ;; NOTE: automatically parse an archive
      message-log-max 200 ;; NOTE: maximum number of lines to keep in the message log buffer (default is 100)
      show-trailing-whitespace 1 ;; NOTE: show trailing whitespace
      scroll-margin 0 ;; NOTE: use smooth scrolling
      scroll-conservatively 100000 ;; NOTE: ... the defaults
      scroll-up-aggressively 0 ;; NOTE: ... are very
      scroll-down-aggressively 0 ;; NOTE: ... annoying
      scroll-preserve-screen-position t ;; NOTE: preserve screen position with C-v/M-v
      auto-save-interval 1000 ;; NOTE: change auto-save interval from 300 to 1000 keystrokes
      sentence-end-double-space 'nil ;; NOTE: sentences end with a single space
      echo-keystrokes 0.1 ;; NOTE: see what you are typing
      suggest-key-bindings nil) ;; NOTE: do not show respective key-bindings

;;; COMMENT: default major mode
;; (setq default-major-mode
;;       (lambda ()
;; 	(let ((buffer-file-name (or buffer-file-name (buffer-name))))
;; 	  (set-auto-mode))))

;;; COMMENT: default auto-mode list
;; (add-to-list 'auto-mode-alist '(".screenrc" . shell-script-mode)) ;; NOTE: open .screenrc in `shell-script-mode'
;; (add-to-list 'auto-mode-alist '(".mpdconf/" . shell-script-mode)) ;; NOTE: open `.mpdconf/' files in `shell-script-mode'
;; (add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode)) ;; NOTE: open .emacs in `emacs-lisp-mode'
;; (add-to-list 'auto-mode-alist '(".stumpwmrc$" . stumpwm-mode)) ;; NOTE: open .stumpwmrc in `stumpwm-mode'
;; (add-to-list 'auto-mode-alist '(".conkerorrc/" . javascript-mode)) ;; NOTE: open `.conkerorrc/' files in `javascript-mode'
(add-to-list 'auto-mode-alist '("bashrc" . shell-script-mode)) ;; NOTE: open `.bashrc' file in `shell-script-mode'
(add-to-list 'auto-mode-alist '("stumpwmrc" . common-lisp-mode)) ;; NOTE: open `.stumpwmrc' file in `common-lisp-mode'
;; (add-to-list 'auto-mode-alist '("stumpwmrc" . stumpwm-mode)) ;; NOTE: open `.stumpwmrc' file in `stumpwm-mode'
(add-to-list 'auto-mode-alist '("README$" . org-mode)) ;; NOTE: open `README' files in `org-mode'
(add-to-list 'auto-mode-alist '("NEWS$" . org-mode)) ;; NOTE: open `NEWS' files in `org-mode'
;; (add-to-list 'auto-mode-alist '("/mutt" . mail-mode)) ;; NOTE: open `mutt'-related buffers in `mail-mode'
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; NOTE: open `*.org' files in `org-mode'
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode)) ;; NOTE: open `*.js' files in `javascript-mode'
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode)) ;; NOTE: open `*.hs' files in `haskell-mode'
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)) ;; NOTE: open `*.cabal' files in `haskell-cabal-mode'
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode)) ;; NOTE: open `*.py' files in `python-mode'
;; (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)) ;; NOTE: open `*.cs' files in `c-sharp-mode'
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode)) ;; NOTE: open `*.tex' files in `latex-mode'
;; (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word)) ;; NOTE: open `*.doc' documents with `antiword'
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)) ;; NOTE: open `*.lua' files in `lua-mode'
;; (add-to-list 'auto-mode-alist '("\\.ot$" . otter-mode)) ;; NOTE: open `*.ot' files in `otter-mode'
;; (add-to-list 'auto-mode-alist '("\\.in$" . otter-mode)) ;; NOTE: open`*.in' files in `otter-mode'
(add-to-list 'interpreter-mode-alist '("python" . python-mode)) ;; NOTE: open python files in a psuedo-python interpreter

;;; COMMENT: selection
(delete-selection-mode 1) ;; NOTE: replace (delete) selected region

;; TODO: these could possibly be moved to `appearance-config.el'
;;; COMMENT: visible alarm
;; (setq visible-bell t) ;; NOTE: flash the current frame on error (instead of beep)

;;; COMMENT: error bell
(setq ring-bell-function 'ignore) ;; NOTE: ignore error bell

;;; COMMENT: uniquify (unique buffer names)
;; SOURCE: `http://emacswiki.org/emacs/uniquify'
(require 'uniquify) ;; TODO: change this to an autoload

(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ;; NOTE: rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ;; NOTE: don't muck with special buffers

;;; COMMENT: ido mode
;; SOURCE: `http://emacswiki.org/emacs/InteractivelyDoThings'
(require 'ido) ;; TODO: change this to an autoload
(require 'ido-ubiquitous) ;; TODO: change this to an autoload

(ido-mode 'both) ;; NOTE: turn on interactive mode (files and buffers)
(ido-ubiquitous-mode t)

(setq ido-enable-flex-matching t ;; NOTE: enable fuzzy matching
      ;;`ido-everywhere t ;; NOTE: enable ido everywhere
      ido-create-new-buffer 'always ;; NOTE: create new buffers (if name does not exist)
      ido-ignore-extensions t ;; NOTE: ignore extentions
      ido-ignore-buffers '("\\` " "^\#[#]?" "^\*Mess" "^\*Back" ".*Completion" "^\*Ido"
			   "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*") ;; NOTE: ignore buffers matching regexp
      ido-work-directory-list `(,(expand-file-name user-home-directory)
      				,(expand-file-name user-documents-directory)
      				,(expand-file-name user-university-directory)
      				,(expand-file-name user-organisation-directory))
      ido-case-fold t ;; NOTE: enable case-insensitivity
      ido-enable-last-directory-history t ;; NOTE: enable directory history
      ido-max-work-directory-list 30 ;; NOTE: remember last used directories
      ido-max-work-file-list 50 ;; NOTE: ... and files
      ido-max-prospects 8 ;; NOTE: don't spam the mini buffer
      ido-show-dot-for-dired t ;; NOTE: enable `dired' with `ido-mode'
      ;;ido-use-virtual-buffers t ;; NOTE: enable virtual buffers
      confirm-nonexistent-file-or-buffer nil ;; NOTE: the confirmation is rather annoying
      )

(defun recentf-ido-find-file (&rest junk) ;; NOTE: replace recentf-open-files
  "Find a recent file using `ido-mode'."
  (interactive)
  (require 'recentf)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file (find-file file))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh `imenu' and jump to a place in the buffer using `ido-mode'."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol (ido-completing-read "Goto symbol: " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))

;;; COMMENT: smex mode
;; SOURCE: `http://emacswiki.org/emacs/Smex'
(autoload 'smex "smex" "Super-charge ido-mode." t)

(setq smex-save-file (concat user-emacs-directory "smex-items")
      smex-key-advice-ignore-menu-bar t)

(eval-after-load "smex" '(smex-initialize)) ;; NOTE: super-charge `ido-mode'

;;; COMMENT: find file at point
;; SOURCE: `http://emacswiki.org/emacs/FindFileAtPoint'
(autoload 'find-file-at-point "ffap" "" t)

;;; COMMENT: ibuffer
;; SOURCE: `http://www.emacswiki.org/emacs/IbufferMode'
(require 'ibuffer) ;; TODO: change this to an autoload
(require 'ibuf-ext) ;; TODO: change this to an autoload

;; NOTE: neither of the following works
(add-to-list 'ibuffer-never-show-predicates " ^\\*Minibuf-0\\*$")
(add-to-list 'ibuffer-never-show-predicates " ^\\*Minibuf-1\\*$")
(add-to-list 'ibuffer-never-show-predicates "^\\*Ibuffer\\*$")

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
	      (mode . lisp-mode)
	      (mode . common-lisp-mode)
	      (mode . emacs-lisp-mode)
	      (mode . inferior-lisp-mode)
	      (mode . inferior-emacs-lisp-mode)
	      (mode . nxml-mode)
	      (mode . ada-mode)
	      (mode . makefile-gmake-mode)
	      (mode . perl-mode)
	      (mode . fuzzy-completions-mode)
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
	  (or (mode . occur-mode)
	      (mode . customize-mode)
	      (mode . Custom-mode)
	      (mode . completion-list-mode)
	      (mode . finder-mode)
	      (mode . color-theme-mode)
	      (mode . browse-kill-ring-mode)
	      (name . "\\*scratch\\*$")
	      (name . "\\*Messages\\*$")
	      (name . "\\*Keys\\*$")
	      (name . "\\*Disabled Command\\*$")
	      (name . "\\*Apropos\\*$")
	      (name . "\\*tramp/ssh ssh\\*$")
	      (name . "\\*Help\\*$")
	      (name . "\\*Org PDF LaTeX Output\\*$"))))))

(setq ibuffer-show-empty-filter-groups nil ;; NOTE: do not display empty groups
      ibuffer-default-sorting-mode 'major-mode ;; NOTE: sort buffers by `major-mode'
      ibuffer-sorting-mode 'recency
      ibuffer-expert t ;; NOTE: do not ask for confirmation
      ;;ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer t ;; NOTE: always display the previous buffer
      ibuffer-use-header-line t
      ibuffer-display-summary t ;; NOTE: summarize ibuffer columns
      ;;ibuffer-default-shrink-to-minimum-size t ;; NOTE: minimize the size of the ibuffer window
      ibuffer-case-fold-search t ;; NOTE: ignore case when searching
      ;; TODO: investigate `ibuffer-directory-abbrev-list'
      ibuffer-old-time 72 ;; NOTE: number of hours before a buffer is considered "old"
      ibuffer-trunacte-lines t ;; NOTE: do not display continuation lines
      ibuffer-use-header-line t ;; NOTE: display a line containing current filters
      )

(add-hook 'ibuffer-mode-hook (lambda ()
			       (ibuffer-auto-mode 1) ;; NOTE: automatically update buffer list
			       ;; (ibuffer-switch-format)
			       (ibuffer-switch-to-saved-filter-groups "default")))

;;; COMMENT: auto-complete mode
;; SOURCE: `http://emacswiki.org/emacs/AutoComplete'
(when (require 'auto-complete-config nil 'noerror) ;; TODO: change this to an auto-load
  (add-to-list 'ac-dictionary-directories (concat (expand-file-name user-emacs-directory) "ac-dict"))
  (setq ac-comphist-file (concat (expand-file-name user-emacs-directory) "ac-comphist.dat"))
  (ac-config-default))

(setq ac-auto-start nil ;; NOTE: start auto-complete after five characters (modified)
      ac-ignore-case t ;; NOTE: always ignore case
      ac-auto-show-menu t) ;; NOTE: automatically show menu

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(ac-flyspell-workaround) ;; NOTE: apparently the flyspell-mode process disables auto-completion

(global-auto-complete-mode t) ;; NOTE: enable `auto-complete' where it makes sense

;; (define-globalized-minor-mode real-global-auto-complete-mode ;; NOTE: dirty fix for having AC everywhere
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;; 			   (auto-complete-mode 1))))

;; (real-global-auto-complete-mode t)

;;; COMMENT: smart tab
(defun smart-tab () ;; NOTE: implement a smarter TAB
  "This smart tab is minibuffer compliant: it acts as usual in the minibuffer.

If mark is active, indents region. Else if point is at the end of a symbol, expands it. Else indents the current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(auto-complete nil)) ;; NOTE: use `auto-complete'
	;; (hippie-expand nil)) ;; NOTE: use `hippie-expand'
	;; (dabbrev-expand nil)) ;; NOTE: use `dabbrev-expand'
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (auto-complete nil) ;; NOTE: use `auto-complete'
	  ;; (hippie-expand nil)) ;; NOTE: use `hippie-expand'
	  ;; (dabbrev-expand nil) ;; NOTE: use `dabbrev-expand'
	(indent-for-tab-command)))))

;;; COMMENT: enable/disable functions
(put 'overwrite-mode 'disabled t) ;; NOTE: disable `overwrite-mode'
(put 'dired-find-alternate-file 'disabled nil) ;; NOTE: enable re-use of dired buffers

;;; COMMENT: mini-buffer
(file-name-shadow-mode t) ;; NOTE: be smart about filenames in the mini-buffer
(fset 'yes-or-no-p 'y-or-n-p) ;; NOTE: changes all "yes/no" questions to "y/n"
;; (savehist-mode t) ;; NOTE: keep mini buffer history between session (IMPORTANT: may be deprecated)
(setq read-buffer-completion-ignore-case t) ;; NOTE: ignore case when reading a buffer name

;;; COMMENT: temporary buffers
(temp-buffer-resize-mode t) ;; NOTE: auto-fit the *Help* buffer to its contents

;;; COMMENT: stumpwm mode
;; SOURCE: `http://www.emacswiki.org/emacs/StumpWM'
(autoload 'stumpwm-mode "stumpwm-mode" "Major mode for editing StumpWM." t) ;; NOTE: not ideal

;; FIX: this doesn't appear to work with emacs 24...
;;; COMMENT: single line copy
;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (message "Copied line")
;;       (list (line-beginning-position) (line-beginning-position 2)))))

;; FIX: this doesn't appear to work with emacs 24...
;;; COMMENT: single line cut
;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (message "Killed line")
;;       (list (line-beginning-position) (line-beginning-position 2)))))

;;; COMMENT: tramp
;; SOURCE: `http://emacswiki.org/cgi-bin/wiki/TrampMode'
(autoload 'tramp "Remote file manipulation with Tramp." t)

(setq tramp-default-method "ssh") ;; NOTE: use ssh for tramp

;;; COMMENT: version control
;; SOURCE: `http://www.emacswiki.org/emacs/Magit'
(autoload 'magit-status "magit" "Version control with Git." t) ;; NOTE: magit for use with github

(setq magit-save-some-buffers t ;; NOTE: ask me to save buffers before running magit-status
      magit-process-popup-time 4) ;; NOTE: popup the process buffer if command takes too long

;;; COMMENT: backups
(setq-default delete-old-versions t) ;; NOTE: delete excess file backups silently

(setq ;; backup-by-copying t ;; NOTE: don't clobber symlinks
      ;; backup-inhibited t ;; NOTE: disable backup
      ;; auto-save-default nil ;; NOTE: disable auto save
      backup-directory-alist `(("." . ,(concat (expand-file-name user-emacs-directory) "save-files"))) ;; NOTE: don't litter the file-system
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ;; NOTE: use versioned backups

;;; COMMENT: recent files
;; SOURCE: `http://emacswiki.org/emacs/RecentFiles'
(autoload 'recentf-mode "recentf" "Recent files." t)

(setq recentf-save-file (concat (expand-file-name user-emacs-directory) "recentf") ;; NOTE: recently saved files
      recentf-max-saved-items 500 ;; NOTE: maximum saved items is 500
      recentf-max-menu-items 25) ;; NOTE: maximum 25 files in menu

(eval-after-load "recentf" '(recentf-mode t))

;;; COMMENT: desktop save mode
;; SOURCE: `http://emacswiki.org/emacs/DeskTop'
(autoload 'desktop-save-mode "desktop" "Save session file." t)

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

;; (restore-desktop-session) ;; NOTE: this is not asked so that `emacs --daemon' works
;; (desktop-save-mode 1) ;; NOTE: enable desktop save mode

(setq desktop-path (list (expand-file-name user-emacs-directory))
      desktop-dirname (expand-file-name user-emacs-directory)
      desktop-base-file-name "emacs-desktop"
      history-length 250)

(eval-after-load "desktop" '(add-to-list 'desktop-globals-to-save 'file-name-history))

;; ERROR: this (obviously) does not work as expected
;; (add-hook 'find-file-hook (lambda () (desktop-save-in-desktop-dir))) ;; NOTE: save the desktop everytime a (new) file is opened

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

;;; COMMENT: flyspell
;; SOURCE: `http://www.emacswiki.org/emacs/FlySpell'
(autoload 'flyspell-mode "flyspell" "On-the-fly spell checking" t)
;;(autoload 'flyspell-prog-mode "flyspell" "On-the-fly spell checking." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "..." t)

(eval-after-load "flyspell" '(add-hook 'text-mode-hook 'turn-on-flyspell)) ;; NOTE: turn on automatic spell check if in a `text-mode'

;;; COMMENT: ispell
(setq ispell-program-name "aspell" ;; NOTE: use aspell for automatic spelling
      ispell-parser 'tex
      ispell-extra-args '("--sug-mode=ultra"))

;;; COMMENT: doc-view
;; SOURCE: `http://www.emacswiki.org/emacs/DocViewMode'
(autoload 'doc-view-mode "doc-view" "Read PDFs with GNU Emacs." t)

(setq doc-view-continuous t)

;;; COMMENT: ansi-terminal
;; SOURCE: `http://www.emacswiki.org/emacs/AnsiTerm'
(getenv "TERM") ;; NOTE: the terminal used when GNU Emacs was started

;; TODO: add a prefix argument for `term-line-mode' and `term-char-mode'
(defun start-new-term (&optional term-mode)
  "Start a new `ansi-term' shell in the directory of current buffer."
  (ansi-term user-shell)
  ;; (ansi-term "/bin/bash")
  ;; (if (eq term-mode nil)
  ;;     ;; (term-char-mode) ;; NOTE: make it feel like a character terminal
  ;;     (term-line-mode)) ;; NOTE: make it feel like a GNU Emacs session
  (message "Starting terminal session."))

;;; COMMENT: `ansi-term' session management
(defun terminal-start-or-switch (program &optional use-existing)
  "Run program PROGRAM in a terminal buffer.

If USE-EXISTING is non-nil, and PROGRAM is already running, switch to that buffer instead of starting a new instance."
  (interactive "sEnter program: ")
  (let ((bufname (concat "*" program "*")))
    (when (not (and use-existing
		    (let ((buf (get-buffer bufname)))
		      (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term program program))))

(defmacro start-term-program-shortcut (program)
  "Macro to launch PROGRAM in TERMINAL."
  (let ((func (intern (concat "start-" program)))
	(doc (format "Launch %s in an `ansi-term' session." program)))
    `(defun ,func nil
       ,doc
       (interactive)
       (save-excursion
	 (save-current-buffer
	   (split-window-below)
	   (terminal-start-or-switch ,program t))))))

(defun kill-term (&rest junk)
  "Close an `ansi-shell' session and kill the remaining buffer."
  (interactive)
  (when (equal major-mode 'term-mode)
   (progn
      (term-kill-subjob)
      (kill-buffer))))

;;; COMMENT: remote terminal hosts
;; FIX: use the new functions above
(defun remote-term (new-buffer-name cmd &rest switches) ;; NOTE: use this for remote so I can specify command line arguments
  "Some documentation."
  (setq term-ansi-buffer-name (concat "" new-buffer-name ""))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))

;; TODO: I think I will need to learn `comint-mode' a bit more for this (I think the above fix is ugly)
(defun open-partch-connection (&rest junk) ;; FIX: does not work
  "Open an SSH connection to the server `partch.anu.edu.au' with a university login number."
  (remote-term "partch" "ssh" "u4537508@partch.anu.edu.au"))

;; FIX: make this work
;;(define-key term-mode-map (kbd "C-c q") 'kill-term) ;; TODO: map (kcd "C-c q") to `kill-term' function

(start-term-program-shortcut "bash") ;; NOTE: create command `start-bash'
(start-term-program-shortcut "htop") ;; NOTE: create command `start-htop'
;; (start-term-program-shortcut "mutt") ;; NOTE: create command `start-mutt'
;; (start-term-program-shortcut "aptitude") ;; NOTE: create command `start-aptitude'

;;; COMMENT: interaction with `transient-mark-mode'
(defadvice term-line-mode (after term-line-mode-fixes ()) ;; NOTE: enable transient mark modes in term-line-mode
  (set (make-local-variable 'transient-mark-mode) t))

(defadvice term-char-mode (after term-char-mode-fixes ()) ;; NOTE: disable transient mark modes in term-char-mode
  (set (make-local-variable 'transient-mark-mode) nil))

;;; COMMENT: transient-mark-mode interaction and ansi colour support
(eval-after-load "shell" '(progn
                            (ad-activate 'term-line-mode)
                            (ad-activate 'term-char-mode)
                            (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)))

;;; COMMENT: help mode
;; (require 'help-mode)
;; (load-library "help-mode")

;;; COMMENT: smart buffer switching
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.

User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.

User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

;;; COMMENT: switch to `*scratch*' buffer
(defun switch-to-scratch (&rest args)
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; COMMENT: quick open directories and files
(defvar user-files-alist '() "List of user's files.")

(add-to-list 'user-files-alist `("home" . ,user-home-directory))
(add-to-list 'user-files-alist `("scripts" . ,user-scripts-directory))
(add-to-list 'user-files-alist `("documents" . ,user-documents-directory))
(add-to-list 'user-files-alist `("news" . ,user-news-directory))
(add-to-list 'user-files-alist `("mail" . ,user-mail-directory))
(add-to-list 'user-files-alist `("audio" . ,user-audio-directory))
(add-to-list 'user-files-alist `("video" . ,user-video-directory))
(add-to-list 'user-files-alist `("programming" . ,user-programming-directory))
(add-to-list 'user-files-alist `("projects" . ,user-projects-directory))
(add-to-list 'user-files-alist `("reading" . ,user-reading-directory))
(add-to-list 'user-files-alist `("writing" . ,user-writing-directory))
(add-to-list 'user-files-alist `("organisation" . ,user-organisation-directory))
(add-to-list 'user-files-alist `("university" . ,user-university-directory))
(add-to-list 'user-files-alist `("org university" . ,user-org-university-file))
(add-to-list 'user-files-alist `("org notes" . ,user-org-notes-file))
(add-to-list 'user-files-alist `("org projects" . ,user-org-projects-file))
(add-to-list 'user-files-alist `("org archive" . ,user-org-archive-file))

(defun quick-open (&rest junk)
  "Open specific user files.

NOTE: See the variable `user-files-alist' for a list of user files."
  (interactive)
  (let ((target (ido-completing-read "Select target: " (mapcar #'(lambda (entry) (car entry)) user-files-alist))))
    (find-file (cdr (assoc target user-files-alist)))))

;;; COMMENT: encryption
;; TODO: set up `http://emacswiki.org/emacs/EasyPG'

;;; COMMENT: browse kill ring
(autoload 'browse-kill-ring "browse-kill-ring" "..." t)

(eval-after-load "browse-kill-ring" '(browse-kill-ring-default-keybindings))

(provide 'general-config)

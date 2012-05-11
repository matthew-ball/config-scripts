;; FILE: ~/.emacs.d/config-el/general-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: user variables
(defvar user-home-directory "~/" "Directory for user's home files.")

(defvar user-scripts-directory (concat user-home-directory ".conf-scripts/") "Directory for user's run-time scripts.")
(defvar user-documents-directory (concat user-home-directory "Documents/") "Directory for user's documents.")

;; TODO: set up the following ...
;; (defvar user-news-directory '() "Directory for user's news.")
;; (defvar user-mail-directory '() "Directory for user's mail.")
(defvar user-audio-directory (concat user-home-directory "Music/") "Directory for user's music.")
(defvar user-video-directory (concat user-home-directory "Videos/") "Directory for user's videos.")

(defvar user-projects-directory (concat user-home-directory "Projects/") "Directory for user's projects.")
(defvar user-reading-directory (concat user-documents-directory "Reading/") "Directory for user's reading material.")
(defvar user-writing-directory (concat user-documents-directory "Writing/") "Directory for user's writing material.")
(defvar user-organisation-directory (concat user-documents-directory "Organisation/") "Directory for user's organisation files.")
(defvar user-university-directory (concat user-documents-directory "ANU/") "Directory for user's university files.")

(defvar user-org-university-file (concat user-organisation-directory "school.org") "File for user's university organisation.")
(defvar user-org-notes-file (concat user-organisation-directory "notes.org") "File for user's notes organisation.")
(defvar user-org-projects-file (concat user-organisation-directory "projects.org") "File for user's projects organisation.")
(defvar user-org-archive-file (concat user-organisation-directory "archive.org") "File for user's archive organisation.")

(defvar user-university-id "u4537508" "University ID for the user.")
(defvar user-primary-email-address "mathew.ball@gmail.com" "Primary email address for the user.")
(defvar user-secondary-email-address (concat user-university-id "@anu.edu.au") "Secondary email address for the user.")

;;; COMMENT: user functions
(defun eval-and-replace ()
  "Replace the preceding s-expression with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; TODO: this works, but as I no longer have my whole dot emacs file in a single location, it serves no purpose
(defun switch-to-dot-emacs (&rest junk)
  "Switch to init.el file (or evaluate the buffer if the init.el file is present)."
  (interactive)
  ;; (config files)
  (if (equal (buffer-name) "init.el")
      (eval-buffer) ;; evaluate the current buffer
    (find-file (concat (expand-file-name user-emacs-directory) "init.el")))) ;; switch to the init.el file

;;; COMMENT: initial minibuffer message
(defun display-startup-echo-area-message (&rest junk)
  "Clear the message buffer initially."
  (message ""))

;;; COMMENT: default variable values
(setq inhibit-startup-message t ;; turn off startup message
      inhibit-startup-echo-area-message t ;; turn off startup echo area message
      initial-scratch-message (concat ";; For information about "
				      (substring (emacs-version) 0 16)
				      " and the GNU system, type C-h C-a.\n\n") ;; initial scratch message
      completion-ignore-case t ;; ignore case in auto-completing text
      read-file-name-completion-ignore-case t ;; ignore cases in filenames
      auto-compression-mode 1 ;; automatically parse an archive
      message-log-max 2000 ;; maximum number of lines to keep in the message log buffer (default is 100)
      show-trailing-whitespace 1 ;; show trailing whitespace
      scroll-margin 0 ;; use smooth scrolling
      scroll-conservatively 100000 ;; ... the defaults
      scroll-up-aggressively 0 ;; ... are very
      scroll-down-aggressively 0 ;; ... annoying
      scroll-preserve-screen-position t ;; preserve screen position with C-v/M-v
      auto-save-interval 1000 ;; change auto-save interval from 300 to 1000 keystrokes
      sentence-end-double-space 'nil ;; sentences end with a single space
      echo-keystrokes 0.1 ;; see what you are typing
      suggest-key-bindings nil) ;; do not show respective key-bindings

;;; COMMENT: default auto-mode list
;; (add-to-list 'auto-mode-alist '(".screenrc" . shell-script-mode)) ;; open .screenrc in shell script mode
;; (add-to-list 'auto-mode-alist '(".mpdconf/" . shell-script-mode)) ;; open any file in .mpdconf/ in shell script mode
;; (add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode)) ;; open .emacs in emacs lisp mode
;; (add-to-list 'auto-mode-alist '(".stumpwmrc$" . stumpwm-mode)) ;; open .stumpwmrc in stumpwm mode
;; (add-to-list 'auto-mode-alist '(".conkerorrc/" . javascript-mode)) ;; open any file in .conkerorrc/ in javascript mode
(add-to-list 'auto-mode-alist '("bashrc" . shell-script-mode)) ;; open bashrc file in shell script mode
(add-to-list 'auto-mode-alist '("stumpwmrc" . common-lisp-mode)) ;; open stumpwmrc file in common lisp mode
;; (add-to-list 'auto-mode-alist '("stumpwmrc" . stumpwm-mode)) ;; open stumpwmrc file in stumpwm mode
(add-to-list 'auto-mode-alist '("README$" . org-mode)) ;; open README files in org-mode
(add-to-list 'auto-mode-alist '("NEWS$" . org-mode)) ;; open NEWS files in org-mode
;; (add-to-list 'auto-mode-alist '("/mutt" . mail-mode)) ;; open mutt-related buffers in mail mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; open *.org files in org-mode
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode)) ;; open *.js files in javascript mode
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode)) ;; open *.hs files in haskell mode
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)) ;; open *.cabal files in haskell cabal mode
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode)) ;; open *.py files in python mode
;; (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)) ;; open *.cs files in c# mode
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode)) ;; open *.tex files in LaTeX mode
;; (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word)) ;; open word documents with antiword
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)) ;; open *.lua files in lua mode
;; (add-to-list 'auto-mode-alist '("\\.ot$" . otter-mode)) ;; open *.ot files in otter mode
;; (add-to-list 'auto-mode-alist '("\\.in$" . otter-mode)) ;; open *.in files in otter mode
(add-to-list 'interpreter-mode-alist '("python" . python-mode)) ;; open python files in a psuedo-python interpreter

;;; COMMENT: selection
(delete-selection-mode 1) ;; replace (delete) selected region

;;; COMMENT: uniquify (unique buffer names)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ;; NOTE: rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ;; NOTE: don't muck with special buffers

;;; COMMENT: ido mode
(require 'ido)
(require 'ido-ubiquitous)

(ido-mode 'both) ;; turn on interactive mode (files and buffers)
(ido-ubiquitous-mode t)

(setq ido-enable-flex-matching t ;; enable fuzzy matching
      ido-everywhere t ;; enable ido everywhere
      ido-create-new-buffer 'always ;; create new buffers (if name does not exist)
      ido-ignore-extensions t ;; ignore extentions
      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*") ;; ignore
      ido-work-directory-list `(,(expand-file-name user-home-directory)
      				,(expand-file-name user-documents-directory)
      				,(expand-file-name user-university-directory)
      				,(expand-file-name user-organisation-directory))
      ido-case-fold t ;; enable case-insensitivity
      ido-enable-last-directory-history t ;; enable directory history
      ido-max-work-directory-list 30 ;; remember last used directories
      ido-max-work-file-list 50 ;; ... and files
      ido-max-prospects 8 ;; don't spam the mini buffer
      ido-show-dot-for-dired t
      confirm-nonexistent-file-or-buffer nil) ;; the confirmation is rather annoying

(defun recentf-ido-find-file () ;; replace recentf-open-files
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file (find-file file))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
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

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.
    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

(defadvice completing-read ;; replace completing-read wherever possible, unless directed otherwise
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ;; manual override disable ido
	  (and (boundp 'ido-cur-list)
	       ido-cur-list)) ;; avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
	  (setq ad-return-value
		(ido-completing-read prompt allcomp nil require-match initial-input hist def))
	ad-do-it))))

;;; COMMENT: smex mode
(require 'smex)
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize) ;; super-charge ido mode

;;; COMMENT: ibuffer
(require 'ibuffer)
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
	  (filename . ,(expand-file-name user-writing-directory)))
	 ("Projects" ;; NOTE: project related buffers
	  (filename . ,(expand-file-name user-projects-directory)))
	 ("Programming" ;; NOTE: programming related buffers
	  (or (mode . c-mode)
	      (mode . c++-mode)
	      (mode . haskell-mode)
	      (mode . inferior-haskell-mode)
	      (mode . python-mode)
	      (mode . inferior-python-mode)
	      (mode . lisp-mode)
	      (mode . common-lisp-mode)
	      (mode . emacs-lisp-mode)
	      (mode . inferior-lisp-mode)
	      (mode . repl-mode)
	      (mode . slime-mode)
	      (mode . inferior-slime-mode)
	      (mode . fuzzy-completions-mode)
	      (mode . html-mode)
	      (mode . css-mode)
	      (mode . javascript-mode)
	      (mode . scheme-mode)
	      (mode . inferior-scheme-mode)
	      (mode . compilation-mode)
	      (mode . shell-script-mode)
	      (mode . sh-mode)
	      (mode . gist-list-mode)
	      (mode . gist-menu-mode)
	      (name . "^\\*slime-events\\*$")
	      (name . "^\\*slime-threads\\*$")
	      (name . "^\\*slime-connections\\*$")
	      (name . "^\\*slime-repl sbcl\\*$")
	      (name . "^\\*slime-compilation\\*$")
	      (name . "^\\*inferior-lisp\\*$")))
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
	 ("ERC" ;; NOTE: erc related buffers
	  (mode . erc-mode))
	 ("Web Browser" ;; NOTE: w3m related buffers
	  (mode . w3m-mode))
	 ("File Manager" ;; NOTE: dired related buffers
	  (or (mode . dired-mode)
	      (name . "^\\*Dired log\\*$")))
	 ("Shell" ;; NOTE: shell related buffers
	  (or (mode . eshell-mode)
	      (mode . shell-mode)
	      (mode . term-mode)
	      (mode . locate-mode)))
	 ("Mathematics and Science" ;; NOTE: buffers related to mathematics and science
	  (or (mode . calculator-mode)
	      (mode . calc-mode)
	      (mode . calc-trail-mode)
	      (mode . maxima-mode)
	      (mode . inferior-maxima-mode)))
	 ("Mail and News" ;; NOTE: mail (and news) related buffers
	  (or (mode . gnus-group-mode)
	      (mode . gnus-topic-mode)
	      (mode . gnus-browse-mode)
	      (mode . gnus-summary-mode)
	      (mode . gnus-server-mode)
	      (mode . gnus-article-mode)
	      (mode . gnus-edit-form-mode)
	      (mode . message-mode)
	      (name . "^\\*gnus trace\\*$")
	      (filename . ".newsrc-dribble$")))
	 ("Information" ;; NOTE: info related buffers
	  (or (mode . info-mode)
	      (mode . Info-mode)
	      (mode . apropos-mode)
	      (mode . Help-Mode)
	      (mode . help-mode)
	      (mode . Man-mode)
	      (mode . woman-mode)))
	 ("Process Manager" ;; NOTE: process manager related buffers
	  (or (mode . proced-mode)
	      (mode . process-menu-mode)))
	 ("Package Management" ;; NOTE: apt-mode and elpa related buffers
	  (or (mode . apt-mode)
	      (mode . package-menu-mode)
	      (name . "^\\*Package Info\\*$")))
	 ("Miscellaneous" ;; NOTE: miscellaneous special buffers
	  (or (mode . occur-mode)
	      (mode . customize-mode)
	      (mode . Custom-mode)
	      (mode . completion-list-mode)
	      (mode . finder-mode)
	      (name . "\\*scratch\\*$")
	      (name . "\\*Messages\\*$")
	      (name . "\\*Keys\\*$")
	      (name . "\\*Disabled Command\\*$")
	      (name . "\\*Apropos\\*$")
	      (name . "\\*tramp/ssh ssh\\*$")
	      (name . "\\*Help\\*$")
	      (name . "\\*Org PDF LaTeX Output\\*$"))))))

(setq ibuffer-show-empty-filter-groups nil ;; NOTE: do not display empty groups
      ibuffer-default-sorting-mode 'major-mode ;; NOTE: sort buffers by major-mode
      ibuffer-expert t ;; NOTE: don't ask for confirmation
      ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)

(add-hook 'ibuffer-mode-hook (lambda ()
			       (ibuffer-auto-mode 1) ;; NOTE: automatically update buffer list
			       (ibuffer-switch-to-saved-filter-groups "default")))

;;; COMMENT: auto-complete mode
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories (concat (expand-file-name user-emacs-directory) "ac-dict"))
  (setq ac-comphist-file (concat (expand-file-name user-emacs-directory) "ac-comphist.dat"))
  (ac-config-default))

(setq ac-auto-start 5 ;; NOTE: start auto-complete after five characters
      ac-ignore-case t ;; NOTE: always ignore case
      ac-auto-show-menu t) ;; NOTE: automatically show menu

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(ac-flyspell-workaround) ;; NOTE: apparently the flyspell-mode process disables auto-completion

(define-globalized-minor-mode real-global-auto-complete-mode ;; NOTE: dirty fix for having AC everywhere
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))))

(real-global-auto-complete-mode t)

;;; COMMENT: smart tab
(defun smart-tab () ;; NOTE: implement a smarter TAB
  "This smart tab is minibuffer compliant: it acts as usual in the minibuffer.
 Else, if mark is active, indents region. Else if point is at the end of a symbol, expands it.
 Else indents the current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(auto-complete nil)) ;; NOTE: use auto-complete
	;; (hippie-expand nil)) ;; NOTE: use hippie-expand
	;; (dabbrev-expand nil)) ;; NOTE: use dabbrev-expand
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (auto-complete nil) ;; NOTE: use auto-complete
	  ;; (hippie-expand nil)) ;; NOTE: use hippie-expand
	  ;; (dabbrev-expand nil) ;; NOTE: use dabbrev-expand
	(indent-for-tab-command)))))

;;; COMMENT: enable/disable functions
(put 'overwrite-mode 'disabled t) ;; NOTE: disable overwrite mode

;;; COMMENT: mini buffer
(file-name-shadow-mode t) ;; NOTE: be smart about filenames in the mini buffer
(fset 'yes-or-no-p 'y-or-n-p) ;; NOTE: changes all yes/no questions to y/n
;; (savehist-mode t) ;; NOTE: keep mini buffer history between session (IMPORTANT: may be deprecated)

;;; COMMENT: stumpwm mode
(autoload 'stumpwm-mode "/usr/share/doc/stumpwm/stumpwm-mode" "Major mode for editing StumpWM." t)

;; FIX: this doesn't appear to work ...
;;; COMMENT: single line copy
;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (message "Copied line")
;;       (list (line-beginning-position) (line-beginning-position 2)))))

;; FIX: this doesn't appear to work ...
;;; COMMENT: single line cut
;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (message "Killed line")
;;       (list (line-beginning-position) (line-beginning-position 2)))))

;;; COMMENT: tramp
(autoload 'tramp "Remote file manipulation with Tramp." t)
(setq tramp-default-method "ssh") ;; NOTE: use ssh for tramp

;;; COMMENT: version control
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
;; (require 'recentf)
(autoload 'recentf-mode "recentf" "Recent files." t)

(setq recentf-save-file (concat (expand-file-name user-emacs-directory) "recentf") ;; NOTE: recently saved files
      recentf-max-saved-items 500 ;; NOTE: maximum saved items is 500
      recentf-max-menu-items 25) ;; NOTE: maximum 25 files in menu

(eval-after-load "recentf" (recentf-mode t))

;;; COMMENT: desktop save mode
(autoload 'desktop-save-mode "desktop" "Save session file." t)

(defun restore-desktop-session (&rest junk)
  "Query the user to start the previous saved session or not."
  (interactive)
  (save-excursion
    (when (y-or-n-p (format "Restore desktop session?"))
      (progn
	(desktop-save-mode 1) ;; NOTE: enable desktop-save-mode also
	(desktop-read))))) ;; NOTE: read the desktop file

(defun save-desktop-session (&rest junk)
  "Saves the current GNU Emacs session."
  (interactive)
  (desktop-save-in-desktop-dir))

;; (restore-desktop-session) ;; NOTE: this is not asked so that `emacs --daemon' works
;; (desktop-save-mode 1) ;; NOTE: enable desktop save mode

(setq desktop-path `(,(expand-file-name user-emacs-directory))
      desktop-dirname (expand-file-name user-emacs-directory)
      desktop-base-file-name "emacs-desktop"
      history-length 250)

(eval-after-load "desktop" '(add-to-list 'desktop-globals-to-save 'file-name-history))

(setq desktop-globals-to-save ;; NOTE: save a bunch of variables to the desktop file (for lists specify the len of the maximal saved data also)
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
  "If pid is the process ID of an emacs process, return t, else nil. Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t) pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;;; COMMENT: paredit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

(defun override-slime-repl-bindings-with-paredit () ;; NOTE: stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

;; TODO: move hooks to programming-config.el
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;; COMMENT: flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(add-hook 'text-mode-hook 'turn-on-flyspell) ;; NOTE: turn on automatic spell check if in a text-mode

;;; COMMENT: ispell
(setq ispell-program-name "aspell" ;; NOTE: use aspell for automatic spelling
      ispell-parser 'tex
      ispell-extra-args '("--sug-mode=ultra"))

;;; COMMENT: doc-view
(autoload 'doc-view-mode "doc-view" "Read PDFs with GNU Emacs." t)

(setq doc-view-continuous t)

;;; COMMENT: ansi-terminal
(defun symbol-value-in-buffer (sym buf)
  "Return the value of 'sym' in 'buf'."
  (save-excursion
    (with-current-buffer buf
      (symbol-value sym))))

(defun start-term (&rest junk)
 "Start an `ansi-term' shell in the directory of current buffer."
 (ansi-term "/bin/bash")
 ;; (term-line-mode)
 (message "Starting terminal session."))

(defun switch-term (&rest junk)
  "Switch to an active shell (if one exists) or create a new shell (if none exists)."
  (interactive)
  (let ((found nil))
    (loop for b in (buffer-list)
	  if (eq (symbol-value-in-buffer 'major-mode b) 'term-mode)
	  do (switch-to-buffer b) (setq found t))
    (when (not found) (start-term))))

(defun kill-term (&rest junk)
  "Close an ansi shell session and kill the remaining buffer."
  (interactive)
  (when (equal major-mode (or 'term-mode 'eshell-mode))
      (progn
	(term-kill-subjob)
	(kill-buffer))))

;;; COMMENT: ansi-term session management
;; (defun djcb-term-start-or-switch (prg &optional use-existing)
;;   "Run program PRG in a terminal buffer.

;; If USE-EXISTING is non-nil, and PRG is already running, switch to that buffer instead of starting a new instance."
;;   (interactive)
;;   (let ((bufname (concat "*" prg "*")))
;;     (when (not (and use-existing
;; 		    (let ((buf (get-buffer bufname)))
;; 		      (and buf (buffer-name (switch-to-buffer bufname))))))
;;       (ansi-term prg prg))))

;; (defmacro djcb-program-shortcut (name key &optional use-existing)
;;   "Macro to create a key binding KEY to start some terminal program PRG. I

;; If USE-EXISTING is true, try to switch to an existing buffer."
;;   `(global-set-key ,key
;;      '(lambda()
;;         (interactive)
;;         (djcb-term-start-or-switch ,name ,use-existing))))

;;(djcb-program-shortcut "htop"  (kbd "<S-f4>") t)  ;; NOTE: start htop session (if and only if in a terminal session)

(defun switch-htop (&rest junk)
  "If running without an X window session, switch to a htop session."
  (when (eq window-system nil)
    ))

;;; COMMENT: interaction with transient mark mode
(defadvice term-line-mode (after term-line-mode-fixes ()) ;; NOTE: enable transient mark modes in term-line-mode
  (set (make-local-variable 'transient-mark-mode) t))

(ad-activate 'term-line-mode)

(defadvice term-char-mode (after term-char-mode-fixes ()) ;; NOTE: disable transient mark modes in term-char-mode
  (set (make-local-variable 'transient-mark-mode) nil))

(ad-activate 'term-char-mode)

;;; COMMENT: remote terminal hosts
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

(defun open-partch-connection (&rest junk)
  "Open an SSH connection to the server `partch.anu.edu.au' with my university login number."
  (interactive)
  (remote-term "partch" "ssh" "u4537508@partch.anu.edu.au"))

;;; COMMENT: ansi colour support
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; COMMENT: help mode
(require 'help-mode)
;; (load-library "help-mode")

(provide 'general-config)



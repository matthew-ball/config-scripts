;; ======================================
;; ~/.emacs.d/config-el/general-config.el
;; Matthew Ball (copyleft 2012)
;; ======================================

;;; user variables
(defvar user-home-directory "~/" "Directory for user's home files.")

(defvar user-scripts-directory (concat user-home-directory ".conf-scripts/") "Directory for user's run-time scripts.")
(defvar user-projects-directory (concat user-home-directory "Projects/") "Directory for user's projects.")
(defvar user-documents-directory (concat user-home-directory "Documents/") "Directory for user's documents.")

(defvar user-audio-directory (concat user-home-directory "Music/") "Directory for user's music.")
(defvar user-video-directory (concat user-home-directory "Videos/") "Directory for user's videos.")

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

;;; user functions
(defun eval-and-replace ()
  "Replace the preceding s-expression with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun show-dot-file-structure (&rest junk)
  "Show the outline structure of a configuration file."
  (interactive)
  (progn
    (occur (concat "^" (make-string 3 (aref comment-start 0)) "+"))
    (other-window 1)))

(defun show-bugs-fixes-todos (&rest junk)
  "Show the outline-mode structure listing any bugs, fixes or TODOs in source code comments."
  (interactive)
  (progn
    (occur "\\<\\(FIXME\\|TODO\\|BUG\\): ")
    (other-window 1)))

(defun switch-to-dot-emacs (&rest junk)
  "Switch to init.el file (or evaluate the buffer if the init.el file is present)."
  (interactive)
  (if (equal (buffer-name) "init.el")
      (eval-buffer) ;; evaluate the current buffer
    (find-file (concat (expand-file-name user-emacs-directory) "init.el")))) ;; switch to the init.el file

(defun display-startup-echo-area-message (&rest junk)
  "Clear the message buffer initially."
  (message ""))

;;; default variable values
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

;;; default browser
(setq browse-url-browser-function 'browse-url-generic
      ;; browse-url-generic-program "conkeror") ;; default web browser set to conkeror
      browse-url-generic-program "chromium-browser") ;; default web browser set to chromium-browser

;;; default auto-mode list
;; (add-to-list 'auto-mode-alist '(".screenrc" . shell-script-mode)) ;; open .screenrc in shell script mode
;; (add-to-list 'auto-mode-alist '(".bash_aliases" . shell-script-mode)) ;; open .bash_aliases in shell script mode
;; (add-to-list 'auto-mode-alist '(".mpdconf/" . shell-script-mode)) ;; open any file in .mpdconf/ in shell script mode
;; (add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode)) ;; open .emacs in emacs lisp mode
;; (add-to-list 'auto-mode-alist '(".stumpwmrc$" . stumpwm-mode)) ;; open .stumpwmrc in stumpwm mode
;; (add-to-list 'auto-mode-alist '(".conkerorrc/" . javascript-mode)) ;; open any file in .conkerorrc/ in javaescript mode
(add-to-list 'auto-mode-alist '("bashrc" . shell-script-mode)) ;; open bashrc file in shell script mode
(add-to-list 'auto-mode-alist '("stumpwmrc" . common-lisp-mode)) ;; open stumpwmrc file in common lisp mode
;; (add-to-list 'auto-mode-alist '("stumpwmrc" . stumpwm-mode)) ;; open stumpwmrc file in stumpwm mode
(add-to-list 'auto-mode-alist '("README$" . org-mode)) ;; open README files in org-mode
(add-to-list 'auto-mode-alist '("NEWS$" . org-mode)) ;; open NEWS files in org-mode
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode)) ;; open mutt-related buffers in mail mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; open *.org files in org-mode
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode)) ;; open *.js files in javascript mode
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode)) ;; open *.hs files in haskell mode
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)) ;; open *.cabal files in haskell cabal mode
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode)) ;; open *.py files in python mode
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)) ;; open *.cs files in c# mode
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode)) ;; open *.tex files in LaTeX mode
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word)) ;; open word documents with antiword
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)) ;; open *.lua files in lua mode
(add-to-list 'auto-mode-alist '("\\.ot$" . otter-mode)) ;; open *.ot files in otter mode
(add-to-list 'auto-mode-alist '("\\.in$" . otter-mode)) ;; open *.in files in otter mode
(add-to-list 'interpreter-mode-alist '("python" . python-mode)) ;; open python files in a psuedo-python interpreter

;;; selection
(delete-selection-mode 1) ;; replace (delete) selected region

;;; ido mode
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

;;; smex mode
(require 'smex)
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize) ;; super-charge ido mode

;;; ibuffer
(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      `(("default"
	 ("Configuration" ;; run-time configuration related buffers
	  (or (filename . ,(expand-file-name user-emacs-directory))
	      (filename . ,(expand-file-name user-scripts-directory))))
	 ("University" ;; university related buffers
	  (filename . ,(expand-file-name user-university-directory)))
	 ("Reading" ;; reading (material and notes) related buffers
	  (filename . ,(expand-file-name user-reading-directory)))
	 ("Writing" ;; writing related buffers
	  (filename . ,(expand-file-name user-writing-directory)))
	 ("Projects" ;; project related buffers
	  (filename . ,(expand-file-name user-projects-directory)))
	 ("Programming" ;; programming related buffers
	  (or (mode . c-mode)
	      (mode . c++-mode)
	      (mode . haskell-mode)
	      (mode . inferior-haskell-mode)
	      (mode . python-mode)
	      (mode . inferior-python-mode)
	      (mode . lisp-mode)
	      (mode . common-lisp-mode)
	      (mode . inferior-lisp-mode)
	      (mode . slime-mode)
	      (mode . inferior-slime-mode)
	      (mode . scheme-mode)
	      (mode . inferior-scheme-mode)
	      (mode . maxima-mode)
	      (mode . inferior-maxima-mode)))
	 ("Organisation" ;; org-mode related buffers
	  (or (mode . org-mode)
	      (mode . org-agenda-mode)
	      (mode . calendar-mode)
	      (filename . ,(expand-file-name user-organisation-directory))))
	 ("ERC" ;; erc related buffers
	  (mode . erc-mode))
	 ("Dired" ;; dired related buffers
	  (or (mode . dired-mode)
	      (name . "^\\*Dired log\\*$")))
	 ("Mail and News" ;; mail (and news) related buffers
	  (or (mode . gnus-group-mode)
	      (mode . gnus-topic-mode)
	      (mode . gnus-browse-mode)
	      (mode . gnus-summary-mode)
	      (mode . gnus-article-mode)))
	 ("Version Control" ;; version control related buffers
	  (or (mode . diff-mode)
	      (mode . magit-status-mode)
	      (mode . vc-mode)
	      (mode . vc-dir-mode)
	      (mode . vc-log-entry-mode)
	      (name . "^\\*magit-process\\*$")
	      (name . "^\\*magit-log-edit\\*$")))
	 ("Shell" ;; shell related buffers
	  (or (mode . eshell-mode)
	      (mode . shell-mode)
	      (mode . term-mode)
	      (mode . locate-mode)))
	 ("Emacs Lisp Package Archiver" ;; elpa related buffers
	  (or (mode . package-menu-mode)
	      (name . "^\\*Package Info\\*$")))
	 ("Miscellaneous" ;; miscellaneous special buffers
	  (or (mode . Info-mode)
	      (mode . apropos-mode)
	      (mode . Help-Mode)
	      (mode . help-mode)
	      (mode . Man-mode)
	      (mode . woman-mode)
	      (mode . occur-mode)
	      (mode . customize-mode)
	      (mode . Custom-mode)
	      (mode . completion-list-mode)
	      (name . "\\*scratch\\*$")
	      (name . "\\*Messages\\*$")
	      (name . "\\*Keys\\*$")
	      (name . "\\*Disabled Command\\*$")
	      (name . "\\*Org PDF LaTeX Output\\*$"))))))

(setq ibuffer-show-empty-filter-groups nil ;; do not display empty groups
      ibuffer-default-sorting-mode 'major-mode ;; sort buffers by major-mode
      ibuffer-expert t ;; don't ask for confirmation
      ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)

(add-hook 'ibuffer-mode-hook (lambda ()
			       (ibuffer-auto-mode 1) ;; automatically update buffer list
			       (ibuffer-switch-to-saved-filter-groups "default")))

;;; auto-complete mode
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories (concat (expand-file-name user-emacs-directory) "ac-dict"))
  (setq ac-comphist-file (concat (expand-file-name user-emacs-directory) "ac-comphist.dat"))
  (ac-config-default))

(setq ac-auto-start 5 ;; start auto-complete after five characters
      ac-ignore-case t ;; always ignore case
      ac-auto-show-menu t) ;; automatically show menu

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(ac-flyspell-workaround) ;; apparently the flyspell-mode process disables auto-completion

(define-globalized-minor-mode real-global-auto-complete-mode ;; dirty fix for having AC everywhere
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))))

(real-global-auto-complete-mode t)

;;; smart tab
(defun smart-tab () ;; implement a smarter TAB
  "This smart tab is minibuffer compliant: it acts as usual in the minibuffer.
 Else, if mark is active, indents region. Else if point is at the end of a symbol, expands it.
 Else indents the current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(auto-complete nil)) ;; use auto-complete
	;; (hippie-expand nil)) ;; use hippie-expand
	;; (dabbrev-expand nil)) ;; use dabbrev-expand
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (auto-complete nil) ;; use auto-complete
	  ;; (hippie-expand nil)) ;; use hippie-expand
	  ;; (dabbrev-expand nil) ;; use dabbrev-expand
	(indent-for-tab-command)))))

;;; enable/disable functions
(put 'overwrite-mode 'disabled t) ;; disable overwrite mode

;;; mini buffer
(file-name-shadow-mode t) ;; be smart about filenames in the mini buffer
(fset 'yes-or-no-p 'y-or-n-p) ;; changes all yes/no questions to y/n
(savehist-mode t) ;; keep mini buffer history between session

;;; stumpwm mode
(autoload 'stumpwm-mode "/usr/share/doc/stumpwm/stumpwm-mode" "Major mode for editing StumpWM." t)

;; FIXME: fix
;;; single line copy
;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (message "Copied line")
;;       (list (line-beginning-position) (line-beginning-position 2)))))

;; FIXME: fix
;;; single line cut
;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (message "Killed line")
;;       (list (line-beginning-position) (line-beginning-position 2)))))

;;; tramp
(autoload 'tramp "Remote file manipulation with Tramp." t)
(setq tramp-default-method "ssh") ;; use ssh for tramp

;;; version control
(autoload 'magit-status "magit" "Version control with Git." t) ;; magit for use with github

(setq magit-save-some-buffers t ;; ask me to save buffers before running magit-status
      magit-process-popup-time 4) ;; Popup the process buffer if command takes too long

;;; backups
(setq-default delete-old-versions t) ;; delete excess file backups silently

(setq ;; backup-by-copying t ;; don't clobber symlinks
      ;; backup-inhibited t ;; disable backup
      ;; auto-save-default nil ;; disable auto save
      backup-directory-alist `(("." . ,(concat (expand-file-name user-emacs-directory) "save-files"))) ;; don't litter the fs
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ;; use versioned backups

;;; recent files
(require 'recentf)
(setq recentf-save-file (concat (expand-file-name user-emacs-directory) "recentf") ;; recently saved files
      recentf-max-saved-items 500 ;; maximum saved items is 500
      recentf-max-menu-items 25) ;; maximum 25 files in menu

(recentf-mode t)

;;; desktop save mode
(desktop-save-mode 1) ;; enable desktop save mode

(setq desktop-path `(,(expand-file-name user-emacs-directory))
      desktop-dirname (expand-file-name user-emacs-directory)
      desktop-base-file-name "emacs-desktop"
      history-length 250)

(add-to-list 'desktop-globals-to-save 'file-name-history)

(setq desktop-globals-to-save ;; save a bunch of variables to the desktop file (for lists specify the len of the maximal saved data also)
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

(defun emacs-process-p (pid) ;; over-ride stale lock
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

;;; paredit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;; TODO: move hooks to programming-config.el
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))

(defun override-slime-repl-bindings-with-paredit () ;; stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;; flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(add-hook 'text-mode-hook 'turn-on-flyspell) ;; turn on automatic spell check if in a text-mode

;;; ispell
(setq ispell-program-name "aspell" ;; use aspell for automatic spelling
      ispell-parser 'tex
      ispell-extra-args '("--sug-mode=ultra"))

;;; ansi-terminal
(defun symbol-value-in-buffer (sym buf)
  "Return the value of 'sym' in 'buf'."
  (save-excursion
    (with-current-buffer buf
      (symbol-value sym))))

(defun start-term (&rest junk)
 "Start an `ansi-term' shell in the directory of current buffer."
 (ansi-term "/bin/bash")
 ;; (term-line-mode)
 )

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

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; eshell
;; TODO: move this into a separate configuration file
(require 'ansi-color)
(require 'eshell)
(require 'em-smart)

(setq eshell-prompt-function (lambda () (concat (user-login-name) "@" (system-name) ":" (eshell/pwd) (if (= (user-uid) 0) "# " "$ "))) ;; modify eshell prompt
      eshell-prompt-regexp "^[^#$\n]*[#$] " ;; fix shell auto-complete
      eshell-cmpl-cycle-completions nil ;; avoid cycle-completion
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'" ;; ignore file prefixes
      eshell-save-history-on-exit t ;; save eshell history on exit
      eshell-where-to-jump 'begin ;; jump to beginning of line
      eshell-review-quick-commands nil ;; enable quick review
      eshell-smart-space-goes-to-end t) ;; save buffer history

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start eshell-last-output-end))

(defun eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color) ;; enable colours in eshell sessions

;;; indicate empty lines
;; (toggle-indicate-empty-lines)

;;; project management
;; (require 'eproject) ;; FIXME: change this to an autoload
;; TODO: learn eproject

;;; highlight special comments
(setq special-mode-hooks '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook shell-script-mode))

(mapc (lambda (mode-hook) (add-hook mode-hook (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))))) special-mode-hooks)

(provide 'general-config)


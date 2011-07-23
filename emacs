;; =================================
;; custom .emacs file
;; Matthew Ball (copyleft 2008-2011)
;; =================================

;; ==========
;;; load path
;; ==========
(add-to-list 'load-path "~/.emacs.d/") ;; user load path

;; =============
;;; colour theme
;; =============
(require 'color-theme)
(require 'zenburn)

(when window-system ;; if using a windows system
  (set-face-attribute 'default nil :height 90) ;; change font size
  (setq frame-title-format "%b"
  	icon-title-format "%b")
  (eval-after-load "color-theme" '(zenburn))) ;; apply zenburn colour theme

;; ============
;;; common lisp
;; ============
(eval-when-compile (require 'cl)) ;; load the common-lisp packages

;; ========================
;;; default function values
;; ========================
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; hide the menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; hide the tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; hide the scroll bar
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1)) ;; turn off blinking cursor
(when (fboundp 'tooltip-mode) (tooltip-mode -1)) ;; turn off tooltip
(when (fboundp 'fringe-mode) (fringe-mode -1)) ;; turn off the fringe
(set-language-environment "UTF-8")
(delete-selection-mode 1) ;; delete selected region
;; (display-time-mode t) ;; display time in the mode-bar
;; (display-battery-mode t) ;; display battery status in the mode-bar
;; (which-function-mode t) ;; show the current function in the mode-bar

;; ========================
;;; default variable values
;; ========================
(setq inhibit-startup-message t ;; turn off startup message
      inhibit-startup-echo-area-message t ;; turn off startup echo area message
      initial-scratch-message (concat ";; For information about "
				      (substring (emacs-version) 0 16)
				      " and the GNU system, type C-h C-a.\n\n") ;; initial scratch message
      line-number-mode 1 ;; turn on line numbers in the mode-bar
      column-number-mode 1 ;; turn on column numbers in the mode-bar
      completion-ignore-case t ;; ignore case in auto-completing text
      read-file-name-completion-ignore-case t ;; ignore cases in filenames
      auto-compression-mode 1 ;; automatically parse an archive
      size-indication-mode t ;; show file size in mode-bar
      display-time-day-and-date t ;; display date in the mode-bar
      display-time-24hr-format t ;; display time in the 24-hour format
      message-log-max 2000 ;; default is 100
      show-trailing-whitespace 1 ;; show trailing whitespace
      scroll-margin 0 ;; use smooth scrolling
      scroll-conservatively 100000 ;; ... the defaults
      scroll-up-aggressively 0 ;; ... are very
      scroll-down-aggressively 0 ;; ... annoying
      scroll-preserve-screen-position t ;; preserve screen position with C-v/M-v
      auto-save-interval 1000 ;; change auto-save interval from 300 to 1000 keystrokes
      compile-command "make" ;; set default compile command
      sentence-end-double-space 'nil ;; sentences end with a single space
      next-line-add-newlines t ;; automatically add a newline if attempting to move past the end of file
      echo-keystrokes 0.1 ;; see what you are typing
      suggest-key-bindings nil) ;; do not show respective key-bindings

(setq-default comment-column 65) ;; use a wide window (put comments further right)

;; ================
;;; default browser
;; ================
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror") ;; default web browser set to conkeror
      ;; browse-url-generic-program "chromium-browser") ;; default web browser set to chromium-browser

;; =======================
;;; default auto-mode list
;; =======================
(add-to-list 'auto-mode-alist '(".screenrc" . shell-script-mode)) ;; open .screenrc in shell script mode
(add-to-list 'auto-mode-alist '(".bash_aliases" . shell-script-mode)) ;; open .bash_aliases in shell script mode
(add-to-list 'auto-mode-alist '(".mpdconf/" . shell-script-mode)) ;; open any file in .mpdconf/ in shell script mode
(add-to-list 'auto-mode-alist '(".emacs" . emacs-lisp-mode)) ;; open .emacs in emacs lisp mode
(add-to-list 'auto-mode-alist '(".stumpwmrc$" . stumpwm-mode)) ;; open .stumpwmrc in stumpwm mode
(add-to-list 'auto-mode-alist '(".conkerorrc/" . javascript-mode)) ;; open any file in .conkerorrc/ in javaescript mode
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
(add-to-list 'interpreter-mode-alist '("python" . python-mode)) ;; open python files in a psuedo-python interpreter

;; ===================
;;; default major mode
;; ===================
;; (setq default-major-mode 'org-mode) ;; set org-mode as the default major mode
(setq default-major-mode 'fundamental-mode) ;; set fundamental-mode as the default major mode

;; =================
;;; enable functions
;; =================
(put 'downcase-region 'disabled nil) ;; enable downcase-region function
(put 'upcase-region 'disabled nil) ;; enable upcase-region function
(put 'capitalize-region 'disabled nil) ;; enable capitalize-region function
(put 'dired-find-alternate-file 'disabled nil) ;; enable re-use of dired buffers
(put 'narrow-to-region 'disabled nil) ;; enable restricted editing in the active buffer
(put 'erase-buffer 'disabled nil) ;; enable delete the contents of the active buffer
(put 'eval-expression 'disabled nil) ;; enable evaluating lisp s-expressions
(put 'set-goal-column 'disabled nil) ;; set the current pointer position as a goal
(put 'overwrite-mode 'disabled t) ;; disable overwrite mode

;; =================
;;; custom variables
;; =================
(custom-set-variables
 '(case-fold-search t)
 '(clean-buffer-list-delay-general 1)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock)))

;; =============
;;; custom faces
;; =============
(custom-set-faces
 (font-lock-add-keywords 'emacs-lisp-mode
  			 '(("setq" . font-lock-keyword-face)
			   ("autoload" . font-lock-keyword-face)))
 (font-lock-add-keywords 'lisp-interaction-mode
			 '(("setf" . font-lock-keyword-face))))

;; =========================
;;; global keyboard bindings
;; =========================
(global-set-key (kbd "C-c %") 'jump-to-matching-parenthesis) ;; jump to the matching parenthesis
(global-set-key (kbd "TAB") 'smart-tab) ;; use smart-tab
(global-set-key (kbd "<f3>") 'erc-start-or-switch) ;; start an ERC session (or switch to the most active buffer) [this is handled in stumpwmrc]
(global-set-key (kbd "M-<f3>") 'show-bugs-fixes-todos) ;; show any TODO items in the source code comments of a file
;; (global-set-key (kbd "C-<f3>") 'some-function) ;; ...
(global-set-key (kbd "<f4>") 'switch-to-dot-emacs) ;; switch to ~/.emacs file (or evaluate-buffer if already present)
(global-set-key (kbd "C-<f4>") 'show-dot-file-structure) ;; show the file structure
(global-set-key (kbd "<f5>") 'slime) ;; start slime
(global-set-key (kbd "M-<f5>") 'magit-status) ;; view the git-status of the current file with magit
;; (global-set-key (kbd "<f5>") 'compile) ;; compile the current file
;; (global-set-key (kbd "M-<f5>") 'next-error) ;; jump to the next error
;; (global-set-key (kbd "C-<f5>") 'previous-error) ;; jump to the previous error
(global-set-key (kbd "<f6>") 'eshell) ;; open (or switch to) an eshell session
(global-set-key (kbd "M-<f6>") 'ido-goto-symbol) ;; go to a symbol in the current buffer
(global-set-key (kbd "C-<f6>") 'eval-and-replace) ;; evaluate a lisp expression and replace with the value
(global-set-key (kbd "<f7>") 'gnus) ;; open gnus (this is handled in stumpwmrc)
;; (global-set-key (kbd "<f7>") 'org-agenda) ;; view org-agenda *(taken care of by stumpwm)
;; (global-set-key (kbd "M-<f7>") 'org-capture) ;; capture tasks and store them in relative files *(taken care of by stumpwm)
;; (global-set-key (kbd "C-<f7>") 'calendar) ;;  view calendar *(taken care of by stumpwm)
;; (global-set-key (kbd "<f8>") 'ido-dired) ;; launch ido-dired *(taken care of by stumpwm)
(global-set-key (kbd "M-<f8>") 'elisp-index-search) ;; search for the documentation of an emacs lisp function
(global-set-key (kbd "C-<f8>") 'emacs-index-search) ;; search for the documentation of an emacs command
(global-set-key (kbd "<f9>") 'package-list-packages) ;; list available ELPA packages
(global-set-key (kbd "M-<f9>") 'google-define) ;; define the word currently under the cursor
(global-set-key (kbd "C-<f9>") 'google-it) ;; search for a string on google
(global-set-key (kbd "<f12>") 'linum-mode) ;; turn on line numbering
(global-set-key (kbd "M-<f12>") (lambda () (interactive) (switch-to-buffer "*scratch*"))) ;; switch to the scratch buffer
(global-set-key (kbd "C-<f12>") 'regexp-builder) ;;  start regular-expression builder
(global-set-key (kbd "M-x") 'smex) ;; smex improves default ido at the minibuffer
(global-set-key	(kbd "M-X") 'smex-major-mode-commands) ;; available major mode commands
(global-set-key	(kbd "C-c C-c M-x") 'execute-extended-command) ;; original M-x command
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; open ibuffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; shows a list of recently opened files
(global-set-key (kbd "C-x f") 'recentf-ido-find-file) ;; use ido to navigate recentf files
(global-set-key (kbd "C-x w") 'word-count) ;; count the words in the active region
(global-set-key (kbd "C-c l") 'org-store-link) ;; save file link
(global-set-key (kbd "C-c b") 'org-switchb) ;; switch through available org buffers
(global-set-key (kbd "C-z") 'undo) ;; undo some previous change
;; (global-unset-key (kbd "C-z")) ;; remove binding on C-z (suspend-frame)

;; ===============
;;; pretty lambdas
;; ===============
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(lambda\\)\\>" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?λ)))))) ;; FIXME: fix
(font-lock-add-keywords 'lisp-interaction-mode '(("(\\(lambda\\)\\>" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?λ))))))

;; ==========
;;; debugging
;; ==========
(setq debug-on-error t)

;; ===========
;;; slime mode
;; ===========
(autoload 'slime "slime" "The Superior Lisp Interaction mode for Emacs" t)
(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations
	   '((sbcl ("/usr/bin/sbcl"))
	     (clisp ("/usr/bin/clisp"))))
     '(slime-setup '(slime-repl
		     slime-asdf
		     ;; slime-autodoc
		     slime-editing-commands
		     slime-fancy-inspector
		     slime-fontifying-fu
		     slime-fuzzy
		     slime-indentation
		     slime-mdot-fu
		     slime-package-fu
		     slime-references
		     slime-sbcl-exts
		     slime-scratch
		     slime-xref-browser))))

;; (slime-autodoc-mode)
(setq inferior-lisp-program "/usr/bin/sbcl" ;; use sbcl as the lisp environment
      slime-net-coding-system 'utf-8-unix
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(defun start-slime-automatically () ;; automatically start slime when opening a lisp file
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'start-slime-automatically)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; ========
;;; stumpwm
;; ========
(autoload 'stumpwm-mode "/usr/share/doc/stumpwm/stumpwm-mode" "Major mode for editing StumpWM." t)

;; =====
;;; info
;; =====
(require 'info-look) ;; TODO: change this to an autoload

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))

;; =========
;;; antiword
;; =========
(defun no-word ()
  "Run antiword on the entire buffer."
  (shell-command-on-region (point-min) (point-max) "antiword - " t t))

;; =================
;;; package archiver
;; =================
(when (load (expand-file-name "~/.emacs.d/elpa/package.el")) (package-initialize))
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org-mode" . "http://orgmode.org/pkg/daily/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;; =================
;;; single line copy
;; =================
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (message "Copied line")
      (list (line-beginning-position) (line-beginning-position 2)))))

;; ================
;;; single line cut
;; ================
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (message "Killed line")
      (list (line-beginning-position) (line-beginning-position 2)))))

;; ======
;;; tramp
;; ======
(autoload 'tramp "Remote file manipulation with Tramp." t)
(setq tramp-default-method "ssh") ;; use ssh for tramp

;; ================
;;; version control
;; ================
(autoload 'mercurial "Version control with Mercurial." t) ;; mercurial for use with bitbucket
(autoload 'magit-status "magit" "Version control with Git." t) ;; magit for use with github

;; ========
;;; backups
;; ========
(setq-default delete-old-versions t) ;; delete excess file backups silently

(setq ;; backup-by-copying t ;; don't clobber symlinks
      ;; backup-inhibited t ;; disable backup
      ;; auto-save-default nil ;; disable auto save
      backup-directory-alist '(("." . "~/.emacs.d/save-files")) ;; don't litter my fs tree
      kept-new-versions 6
      kep-old-versions 2
      version-control t) ;; use versioned backups

;; ==============
;;; diminish mode
;; ==============
(require 'diminish) ;; turn off the mode indicator in the mode-line
(eval-after-load "flyspell" '(diminish 'flyspell-mode ""))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode ""))
(eval-after-load "autopair" '(diminish 'autopair-mode ""))
(eval-after-load "simple" '(diminish 'visual-line-mode ""))
(eval-after-load "simple" '(diminish 'global-visual-line-mode ""))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode ""))
(eval-after-load "yasnippet-bundle" '(diminish 'yas/minor-mode ""))

;; =================
;;; visual line mode
;; =================
(setq global-visual-line-mode t) ;; enable visual line mode for all buffers

;; =============
;;; code folding
;; =============
;; (hs-minor-mode t) ;; turn on hide-show mode

(defvar hs-special-modes-alist
  (mapcar 'purecopy
  '((c-mode "{" "}" "/[*/]" nil nil)
    (c++-mode "{" "}" "/[*/]" nil nil)
    (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
    (java-mode "{" "}" "/[*/]" nil nil)
    (js-mode "{" "}" "/[*/]" nil))))

(setq hs-hide-comments nil ;; hide the comments too when you do a 'hs-hide-all'
      hs-isearch-open 'code) ;; set whether isearch opens folded comments, code, both (t), or neither (nil)

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))

;; ====================
;;; syntax highlighting
;; ====================
(setq-default transient-mark-mode t) ;; highlight the selected region
(when (fboundp 'show-paren-mode) ;; highlight matching parenthesis
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis
	show-paren-delay 0.0))
;; (when (fboundp 'global-hl-line-mode) ;; highlight the current line
;;   (global-hl-line-mode t))

;; =============
;;; line numbers
;; =============
(autoload 'linum-mode "linum" "Minor mode for line numbers" t) ;; show vertical bar with line numbers
(add-hook 'linum-before-numbering-hook (lambda () (setq linum-format "%d ")))

;; ===================
;;; save place in file
;; ===================
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(autoload 'saveplace "Record position in file." t)

;; ====================
;;; unique buffer names
;; ====================
(autoload 'uniquify "Uniquely name buffers." t)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;==============
;;; recent files
;;==============
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/recentf" ;; list recently saved files
      recentf-max-saved-items 500 ;; maximum saved items is 500
      recentf-max-menu-items 25) ;; maximum 25 files in menu
(recentf-mode t)

;; ==========
;;; bookmarks
;; ==========
(require 'bookmark) ;; TODO: change to an autoload
(setq bookmark-save-flag 1) ;; save all bookmarks whenever a bookmarked is created or deleted

;; ==========
;;; yasnippet
;; ==========
(autoload 'yasnippet-bundle "Code and text snippet manager." t)

;; =====
;;; smex
;; =====
(require 'smex)
(smex-initialize) ;; super-charge ido mode

;; ========
;;; flymake
;; ========
;; TODO: set this up
;; (require 'flymake) ;; TODO: change this to an autoload

;; (setq flymake-no-changes-timeout 3)

;; TODO: comment out these lines in haskell-mode.el:
;; (eval-after-load "flymake"
;;       '(add-to-list 'flymake-allowed-file-name-masks
;;                 '("\\.l?hs\\'" haskell-flymake-init)))

;; (defun flymake-Haskell-init () ;; flymake for haskell
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-Haskell-cmdline))

;; (defun flymake-get-Haskell-cmdline (source base-dir)
;;   (list "ghc" (list "--make" "-fbyte-code"
;; 		    (concat "-i" base-dir) ;; can be expanded for additional -i options as in the Perl script
;;               source)))

;; (defvar multiline-flymake-mode nil)
;; (defvar flymake-split-output-multiline nil)

;; (defadvice flymake-split-output ;; this needs to be advised as flymake-split-string is used in other places and I don't know of a better way to get at the caller's details
;;   (around flymake-split-output-multiline activate protect)
;;   (if multiline-flymake-mode
;;       (let ((flymake-split-output-multiline t))
;; 	ad-do-it)
;;     ad-do-it))

;; (defadvice flymake-split-string
;;   (before flymake-split-string-multiline activate)
;;   (when flymake-split-output-multiline
;;     (ad-set-arg 1 "^\\s *$")))

;; (eval-after-load "flymake"
;;   '(progn
;;      (add-to-list 'flymake-allowed-file-name-masks
;;                   '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
;;      (add-to-list 'flymake-err-line-patterns
;;                   '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
;;                     1 2 3 4))))

;; (add-hook 'haskell-mode-hook '(lambda () (set (make-local-variable 'multiline-flymake-mode) t)))

;; (defun flymake-elisp-init () ;; flymake for elisp
;;   (unless (string-match "^ " (buffer-name))
;;     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;            (local-file  (file-relative-name
;;                          temp-file
;;                          (file-name-directory buffer-file-name))))
;;       (list
;;        (expand-file-name invocation-name invocation-directory)
;;        (list
;;         "-Q" "--batch" "--eval"
;;         (prin1-to-string
;;          (quote
;;           (dolist (file command-line-args-left)
;;             (with-temp-buffer
;;               (insert-file-contents file)
;;               (condition-case data
;;                   (scan-sexps (point-min) (point-max))
;;                 (scan-error
;;                  (goto-char(nth 2 data))
;;                  (princ (format "%s:%s: error: Unmatched bracket or quote\n"
;;                                 file (line-number-at-pos)))))))))
;;         local-file)))))

;; (push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)

;; (add-hook 'emacs-lisp-mode-hook
;;           ;; workaround for (eq buffer-file-name nil)
;;           (function (lambda () (if buffer-file-name (flymake-mode))))))

;; (defun flymake-get-tex-args (file-name) ;; flymake for latex
;;     (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; =========
;;; flyspell
;; =========
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(add-hook 'text-mode-hook 'turn-on-flyspell) ;; turn on automatic spell check if in a text-mode

;; =======
;;; ispell
;; =======
(setq ispell-program-name "aspell" ;; use aspell for automatic spelling
      ispell-parser 'tex
      ispell-extra-args '("--sug-mode=ultra"))

;; ====
;;; ido
;; ====
(autoload 'ido "Interactively do things." t)
(ido-mode 'both) ;; turn on interactive mode (files and buffers)
(setq ido-enable-flex-matching t ;; enable fuzzy matching
      ido-everywhere t ;; enable ido everywhere
      ido-create-new-buffer 'always ;; create new buffers (if name does not exist)
      ido-ignore-extensions t ;; ignore extentions
      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*") ;; ignore these buffers
      ido-work-directory-list '("~/" "~/Documents/ANU/" "~/Documents/Organisation/")
      ido-case-fold t ;; enable case-insensitivity
      ido-enable-last-directory-history t ;; enable directory history
      ido-max-work-directory-list 30 ;; remember last used directories
      ido-max-work-file-list 50 ;; ... and files
      ido-max-prospects 8 ;; don't spam the minibuffer
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

;; ==========
;;; icomplete
;; ==========
(autoload 'icomplete "Preview command input." t)

(eval-after-load "icomplete" (icomplete-mode t)) ;; turn on icomplete mode

(setq icomplete-prospects-height 1 ;; don't spam the minibuffer
      icomplete-compute-delay 0) ;; turn off icomplete delay

;; ========
;;; ibuffer
;; ========
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Emacs Miscellaneous" ;; emacs related special buffers
		(or (name . "^\\*scratch\\*$")
		    (name . "^\\*Messages\\*$")
		    (name . "NEWS$")))
	       ("Configuration" ;; run-time configuration related buffers
		(or (filename . ".emacs$")
		    (filename . ".gnus$")
		    (filename . ".stumpwmrc$")
		    (filename . ".bashrc$")
		    (filename . ".bash_aliases$")
		    (filename . ".screenrc$")
		    (filename . ".inputrc$")
		    (filename . ".muttrc$")
		    (filename . ".mpd.conf$")
		    (filename . ".irssi/config")
		    (filename . ".aptitude/config")
		    (filename . ".ncmpcpp/config")
		    (filename . "conkeror-dir/*")))
	       ("Project (Introduction to Computer Science)" ;; introduction to computer science related buffers
		(filename . "/home/chu/Documents/Papers/other/Computer Science/Introduction to Computer Science/")) ;; unfortunately have to hardcode the user here
	       ("Project (Metaphysics)" ;; metaphysics related buffers
		(filename . "/home/chu/Documents/Papers/other/Philosophy/Metaphysics")) ;; i.e. Cannot use ~/Documents/...
	       ("University" ;; buffers related to ANU
		(filename . "/home/chu/Documents/ANU/")) ;; this is a bit problematic
	       ("LaTeX" ;; latex related buffers
		(or (mode . latex-mode)
		    (mode . LaTeX-mode)
		    (mode . bibtex-mode)
		    (mode . reftex-mode)
		    (name . "^\\*toc\\*$")
		    (name . "^\\*RefTex Help\\*$")
		    (name . "^\\*RefTex Select\\*$")
		    (name . "^\\*tex-shell\\*$")))
	       ("Programming" ;; programming related buffers
		(or (mode . c-mode)
		    (mode . c++-mode)
		    (mode . haskell-mode)
		    (mode . inferior-haskell-mode)
		    (mode . haskell-cabal-mode)
		    (mode . python-mode)
		    (mode . inferior-python-mode)
		    (mode . compilation-mode)
		    (mode . shell-script-mode)
		    (mode . makefile-mode)
		    (mode . makefile-gmake-mode)
		    (mode . scheme-mode)
		    (mode . inferior-scheme-mode)
		    (mode . lisp-mode)
		    (mode . emacs-lisp-mode)
		    (mode . lisp-interaction-mode)
		    (mode . inferior-emacs-lisp-mode)
		    (filename . ".emacs.d/")))
	       ("Slime" ;; slime related buffers
		(or (mode . REPL)
		    (mode . sldb-mode)
		    (name . "^\\*slime-events\\*$")
		    (name . "^\\*slime-repl sbcl\\*$")
		    (name . "^\\*inferior-lisp\\*$")))
	       ("Logic For Fun" ;; logic4fun related buffers
		(or (name . "^\\*FINDER\\*$")
		    (name . "^\\*DIAGNOSE\\*$")))
	       ("Reading Notes" ;; reading notes related buffers
		(filename . "Documents/Organisation/books/notes/"))
	       ("Organisation" ;; org-mode related buffers
		(or (mode . org-mode)
		    (mode . diary-mode)
		    (name . "^\\*Fancy Diary Entries\\*$")
		    (name . "^\\*Calendar\\*$")
		    (name . "^diary$")
		    (name . "^\\*Agenda")
		    (name . "^\\*org-")
		    (name . "^\\*Org")
		    (filename . "OrgMode")))
	       ("Debugging" ;; debugging related buffers
		(or (mode . gdb-script-mode)
		    (mode . debugger-mode)))
	       ("Calculator" ;; calculator related buffers
		(or (mode . calc-mode)
		    (mode . calc-trail-mode)))
	       ("Regular Expressions" ;; regular expression related buffers
		(mode . reb-mode))
	       ("Bookmarks" ;; bookmark related buffers
		(or (name . "^\\*Bookmark List\\*$")
		    (name . "^\\*Bookmark Annotation\\*$")
		    (mode . bookmark-bmenu-mode)))
	       ("Completions" ;; completion related buffers
		(or (name . "^\\*Completions\\*$")
		    (name . "^\\*Ido Completions\\*$")
		    (mode . completion-list-mode)))
	       ("IBuffer" ;; ibuffer related buffers
		(or (name . "^\\*IBuffer Diff\\*$")
		    (name . "^\\*buffer-selection\\*$")))
	       ("Shell" ;; shell related buffers
		(or (mode . eshell-mode)
		    (mode . Man-mode)
		    (mode . locate-mode)))
	       ("Version Control" ;; version control related buffers
		(or (mode . diff-mode)
		    (mode . magit-mode)
		    (name . "^\\*magit-process\\*$")
		    (name . "^\\*magit-log-edit\\*$")
		    (mode . vc-mode)
		    (mode . vc-dir-mode)
		    (mode . vc-log-entry-mode)))
	       ("Mail" ;; mail related buffers
		(or (mode . gnus-group-mode)
		    (mode . gnus-article-mode)
		    (mode . gnus-summary-mode)
		    (name . "^\\*imap-log\\*$")
		    (name . ".newsrc-dribble$")
		    (mode . message-mode)
		    (mode . mail-mode)))
	       ("Dired" ;; dired related buffers
		(or (mode . dired-mode)
		    (name . "^\\*Dired log\\*$")))
	       ("ERC" ;; ERC related buffers
		(mode . erc-mode))
	       ("Emacs Lisp Package Archiver" ;; ELPA related buffers
		(or (mode . package-menu-mode)
		    (name . "^\\*Package Info\\*$")))
	       ("Help" ;; help related buffers
		(or (mode . Info-mode)
		    (mode . apropos-mode)
		    (mode . Help-Mode)
		    (mode . help-mode)
		    (mode . man-mode)
		    (mode . woman-mode)
		    (mode . occur-mode)
		    (mode . customize-mode)
		    (mode . Custom-mode)
		    (name . "\\*Keys\\*$")
		    (name . "\*Disabled Command\*")))))))

(setq ibuffer-show-empty-filter-groups nil ;; do not display empty groups
      ibuffer-default-sorting-mode 'major-mode ;; sort buffers by major-mode
      ibuffer-expert t ;; don't ask for confirmation
      ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)

;; (require 'ibuf-ext)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*") ;; don't show special buffers in ibuffer menu
(add-hook 'ibuffer-mode-hook (lambda ()
			       (ibuffer-auto-mode 1) ;; automatically update buffer list
			       (ibuffer-switch-to-saved-filter-groups "default")))

(defadvice ibuffer (around ibuffer-point-to-most-recent) () ;; switching to ibuffer puts the cursor on the most recent buffer
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
  (ad-activate 'ibuffer)

(defun make-ibuffer-projects-list (prefix dir)
  "Generate list of projects for ibuffer interface."
  (reduce (lambda (list prj)
            (cons
             `(,(concat prefix (car prj)) (filename . ,(cdr prj))) list))
          (find-projects dir)
          :initial-value nil))

(defun my-ibuffer-load-hook ()
  "Hook for when ibuffer is loaded."
  (define-ibuffer-filter unsaved-file-buffers
    "Only show unsaved buffers backed by a real file."
    (:description "unsaved file buffers")
    (and (buffer-local-value 'buffer-file-name buf)
	 (buffer-modified-p buf)))
  (define-key ibuffer-mode-map (kbd "/ *") 'ibuffer-filter-by-unsaved-file-buffers))

;; (add-hook 'ibuffer-load-hook 'my-ibuffer-load-hook)

(eval-after-load 'ibuf-ext '(my-ibuffer-load-hook))

;; TODO: fix the following four functions
;; (defun ibuffer-get-current-buffers (&optional including-empty-groups sort-mode) ;; FIXME: fix
;;   "Return active ibuffer filter groups."
;;   (with-temp-buffer
;;     (flet ((message (f &rest args)))
;;       (ibuffer nil (current-buffer)))
;;     (let* ((ibuffer-default-sorting-mode (or sort-mode 'alphabetic))
;;            (buffers (ibuffer-generate-filter-groups (ibuffer-current-state-list))))
;;       (remove-if-not (lambda (x) (if including-empty-groups t (cdr x))) buffers))))

;; (defun ibuffer-get-current-group () ;; FIXME: fix
;;   "Return current ibuffer filter group."
;;   (remove-if-not (lambda (group)
;;                    (remove-if-not (lambda (buffer) (equal (current-buffer)
;;                                                      (car buffer))) (cdr group)))
;;                  (ibuffer-get-current-buffers)))

;; (defun ibuffer-next-buffer () ;; FIXME: fix
;;   "Change to the next buffer in the current ibuffer filter group."
;;   (interactive)
;;   (let* ((all (ibuffer-get-current-group))
;;          (buffers (cdar all))
;;          (current (current-buffer))
;;          (next (mod (loop for b in buffers summing 1 into i
;;                           when (equal (car b) current) return i) (length buffers)))
;;          (names (loop for b in buffers and i = 0 then (1+ i)
;; 		      collecting (if (= i next) (list (car b)) (car b)))))
;;     (set-window-buffer (selected-window) (car (nth next buffers)))
;;     (message "%s: %s" (caar all) names)))

;; (defun ibuffer-next-group () ;; FIXME: fix
;;   "Change to the next ibuffer filter group."
;;   (interactive)
;;   (let* ((group (caar (ibuffer-get-current-group)))
;;          (groups (ibuffer-get-current-buffers nil 'recency))
;; 	 (ignore '("Default" "Help" "Completions" "Regular Expressions" "Bookmarks" "Emacs Lisp Package Archiver")) ;; ignore these groups
;; 	 (groups (remove-if (lambda (n) (member (car n) ignore)) groups))
;;          (next (nth (loop for g in (mapcar (lambda (n) (car n)) groups) summing 1 into i
;;                           when (string= g group) return (mod i (length groups))) groups))
;;          (names (mapcar (lambda (n) (if (string= (car next) (car n))
;; 				   (list (car n)) (car n))) groups)))
;;     (set-window-buffer (selected-window) (caadr next))
;;     (message "%s" names)))

;; ===========
;;; minibuffer
;; ===========
(file-name-shadow-mode t) ;; be smart about filenames in the mini-buffer
(fset 'yes-or-no-p 'y-or-n-p) ;; changes all yes/no questions to y/n
(savehist-mode t) ;; keep minibuffer history between session
(setq file-name-shadow-tty-properties '(invisible t)) ;; electric minibuffer

;; ===========
;;; save hooks
;; ===========
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ;; make script executable (must include #!)

;; ============
;;; auto-insert
;; ============
(auto-insert-mode t) ;; templates for new files

;; ==============
;;; auto-complete
;; ==============
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-start 5 ;; start auto-complete after five characters
      ac-ignore-case t ;; always ignore case
      ac-auto-show-menu t) ;; automatically show menu

(let ((fn-list '(latex-mode org-mode eshell-mode haskell-mode Python-mode cc-mode shell-script-mode csharp-mode)))
  (mapc (lambda (fn) (add-to-list 'ac-modes fn)) fn-list)) ;; enable auto-complete in selected modes

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
(ac-flyspell-workaround) ;; apparently the flyspell-mode process disables auto-completion

(define-globalized-minor-mode real-global-auto-complete-mode ;; dirty fix for having AC everywhere
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))))

(real-global-auto-complete-mode t)

(when (require 'auto-complete-config nil 'noerror) ;; create and add new words to the dictionary on the fly
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default))

;; ===================
;;; partial completion
;; ===================
(partial-completion-mode 1) ;; auto-complete minibuffer commands

;; ==========
;;; auto-pair
;; ==========
(require 'autopair)

(eval-after-load "autopair" '(progn (autopair-global-mode 1)))

(setq autopair-autowrap t ;; enable autopair for quote marks
      autopair-blink nil) ;; turn off blinking match

(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

;;(add-hook 'latex-mode-hook #'(lambda () (push '(?$ . ?$) (getf autopair-extra-pairs :code)))) ;; add extra matches to autopair for latex documents

;; ============================
;;; compilation finish variable
;; ============================
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            (message "compilation errors, type 'M-g n' to visit") ;; if there are no errors, kill the compilation window after 1.5 sec
          (run-at-time 1.5 nil 'delete-windows-on buf)
          (message "No compilation errors!"))))

;; =================
;;; calendar / diary
;; =================
(setq diary-file "~/Documents/Organisation/journal.org"
      cal-tex-diary t ;; include diary entries
      calendar-date-display-form '((if dayname (concat dayname ", ")) day " " monthname " " year)
      calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
      calendar-week-start-day 1
      ;; calendar-view-diary-initially-flag t ;; show diary with calendar
      ;; calendar-mark-diary-entries-flag t ;; show all visible dates with diary entries
      calendar-week-start-day 1 ;; a week begins on Monday
      calendar-latitude 35.18 ;; set location (latitude)
      calendar-longitude 148.10 ;; ... (longitude)
      calendar-location-name "Canberra, ACT" ;; ... (name)
      diary-mail-addr "mathew.ball"
      diary-mail-days 7
      diary-list-include-blanks t ;; include blank dates in diary listing
      ;; diary-show-holidays-flag nil ;; speed up the diary display
      calendar-date-style 'european ;; set timestamp %dd:%mm:%yyyy
      diary-abbreviated-year-flag nil ;; years must be written in full
      number-of-diary-entries 14) ;; show 14 days (two weeks) of diary entries

(add-hook 'today-visible-calendar-hook 'calendar-mark-today) ;; marks the current day

(add-hook 'diary-display-hook 'fancy-diary-display)

(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

(add-hook 'calendar-load-hook
	  '(lambda () (setq mark-holidays-in-calendar t)
	     (define-key calendar-mode-map (kbd ">") 'scroll-calendar-left)
	     (define-key calendar-mode-map (kbd "<") 'scroll-calendar-right)
	     (define-key calendar-mode-map (kbd "C-x >") 'scroll-calendar-left)
	     (define-key calendar-mode-map (kbd "C-x <") 'scroll-calendar-right)))

;; =========
;;; org-mode
;; =========
(autoload 'org-install "Organise tasks with Org-Mode." t)
(autoload 'org-entities "Enable unicode support for Org-Mode." t)
;; (autoload 'org-protocol "Use org-mode with emacsclient." t)
(require 'org-protocol) ;; FIXME: change this to an autoload
(require 'org-latex) ;; FIXME: change this to an autoload

(setq org-support-shift-select 1 ;; enable using SHIFT + ARROW keys to highlight text
      org-return-follows-link t ;; use RETURN to follow links
      org-read-date-display-live nil ;; disable the live date-display
      org-log-done 'time ;; capture a timestamp for when a task changes state
      org-insert-mode-line-in-empty-file t
      org-deadline-warning-days 7
      org-timeline-show-empty-dates t
      org-completion-use-ido t ;; enable ido for target (buffer) completion
      org-log-into-drawer 'LOGBOOK ;; log changes in the LOGBOOK drawer
      org-refile-target '((org-agenda-files :maxlevel . 5) (nil :maxlevel .5)) ;; targets include this file and any file contributing to the agenda - up to 5 levels deep
      org-refile-use-outline-path 'file ;; targets start with the file name - allows creating level 1 tasks
      org-outline-path-complete-in-steps t ;; targets complete in steps so we start with filename, TAB shows the next level of targets etc
      org-refile-allow-creating-parent-nodes 'confirm ;; allow refile to create parent tasks with confirmation
      ;; org-indent-mode t ;; enable org indent mode
      ;; org-indent-indentation-per-level 2 ;; two indents per level
      ;; org-startup-indented t ;; indent text in org documents (WARNING: can crash emacs)
      ;; org-odd-levels-only t ;; use only odd levels for an outline
      ;; org-hide-leading-stars t ;; hide leading stars in a headline
      ;; org-treat-S-cursor-todo-selection-as-state-change nil ;; ignore processing
      ;; org-use-property-inheritance t ;; children tasks inherit properties from their parent
      org-archive-location "archive.org::* Archives" ;; file for archiving items
      ;; org-agenda-include-diary t ;; include entries from the emacs diary
      org-use-fast-todo-selection t ;; enable fast task state switching
      org-use-tag-inheritance nil ;; disable tag inheritance
      org-directory "~/Documents/Organisation/" ;; default directory for org mode
      org-default-notes-file (concat org-directory "notes.org") ;; file for quick notes
      org-agenda-span 28 ;; show four weeks of agendas (replaces `org-agenda-ndays')
      org-agenda-files (quote ("~/Documents/Organisation/notes.org"
			       "~/Documents/Organisation/school.org"
			       "~/Documents/Organisation/home.org"
			       "~/Documents/Organisation/books/books.org"
			       "~/Documents/Organisation/bookmarks.org"
			       "~/Documents/Organisation/projects.org")))

(setq org-todo-keywords (quote ((sequence "TODO(t)" "|" "DONE(d!\!)")
				(sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))) ;; custom states a task could be in

      org-tag-alist (quote (("HOME" . ?h) ("UNIVERSITY" . ?u) ("ASSIGNMENT" . ?a) ("READING" . ?r)
			    ("GENERAL" . ?g) ("PROJECT" . ?p) ("NOTES" . ?n) ("WEBSITE" . ?w) ("BOOKMARK" . ?b))) ;; tags for org-set-tags (C-c C-q)

      org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
				     ("DONE" :foreground "green" :weight bold)
				     ("WAITING" :foreground "yellow" :weight bold)
				     ("CANCELLED" :foreground "blue" :weight bold))) ;; colours for TODO keywords

      org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
					   ("WAITING" ("WAITING" . t))
					   (done ("WAITING"))
					   ("TODO" ("WAITING") ("CANCELLED"))
					   ("DONE" ("WAITING") ("CANCELLED")))) ;; state changes

      org-agenda-custom-commands (quote (("q" "Show Tasks" ((agenda "" ((org-agenda-ndays 7) ;; overview of tasks
									(org-agenda-start-on-weekday nil) ;; calendar begins today
									(org-agenda-repeating-timestamp-show-all t)
									(org-agenda-entry-types '(:timestamp :sexp))))
							    (agenda "" ((org-agenda-ndays 1) ;; daily agenda
									(org-deadline-warning-days 7) ;; seven day advanced warning for deadlines
									(org-agenda-todo-keyword-format "[ ]")
									(org-agenda-scheduled-leaders '("" ""))
									(org-agenda-prefix-format "%t%s")))
							    (todo "TODO" ;; todos searched by context
								  ((org-agenda-prefix-format "[ ] %T: ")
								   (org-agenda-sorting-strategy '(tag-up priority-down))
								   (org-agenda-todo-keyword-format "")
								   (org-agenda-overriding-header "\n Tasks by Context\n==================\n"))))
					  ((org-agenda-compact-blocks t)
					   (org-agenda-remove-tags t)))
					 ("h" "Home" ((org-agenda-list nil nil 1) (tags-todo "HOME") (tags-todo "GENERAL")) "HOME"
					  (org-agenda-files '("home.org" "notes.org"))) ;; tasks for HOME
					 ("u" "University" ((org-agenda-list nil nil 1) (tags "UNIVERSITY") (tags-todo "ASSIGNMENT")) "UNIVERSITY") ;; tasks for UNIVERSITY
					 ("p" "Projects" ((org-agenda-list nil nil 1) (tags-todo "PROJECTS")) "PROJECTS") ;; PROJECT tasks
					 ("g" "General" ((org-agenda-list nil nil 1) (tags-todo "GENERAL") (tags "NOTES")) "GENERAL") ;; tasks for GENERAL
					 ("r" "Reading" ((org-agenda-list nil nil 1) (tags "READING") (tags "WEBSITE")) "READING"))) ;; tasks for READING

      org-capture-templates (quote (("h" "Home" entry (file+headline "home.org" "Home")
				     "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
				    ("u" "University" entry (file+headline "school.org" "University")
				     "** %^{Course Code} %?%^g\n TITLE: %^{Course Title}\n LECTURER: %^{Lecturer}\n" :empty-lines 1 :immediate-finish 1)
				    ("a" "Assignment" plain (file+function "school.org" course-code)
				     "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
				    ;; ("b" "Book to Purchase" table-line (file+headline "books/books.org" "Books to Purchase")
				    ;;  "| [[%^{Link}][%^{Title}]] | %^{Author} | %^{Price} |" :immediate-finish 1) ;; manual version
				    ("b" "Book to Purchase" table-line (file+headline "books/books.org" "Books to Purchase")
				     "| %c | %i | %^{Price} |" :immediate-finish 1) ;; conkeror version
				    ("r" "Book to Read" table-line (file+headline "books/books.org" "Books to Read")
				     "| %^{Title} | %^{Author} |" :immediate-finish 1)
				    ("p" "Paper to Read" table-line (file+headline "books/books.org" "Papers to Read")
				     "| %^{Title} | %^{Author} |  %^{Journal} | %^{Year} |" :immediate-finish 1)
				    ("k" "Internet Bookmark" table-line (file+headline "bookmarks.org" "Internet Bookmarks")
				     "| %c |" :immediate-finish 1)
				    ("f" "File Bookmark" table-line (file+headline "bookmarks.org" "File Bookmarks")
				     "| [[file:%(if (not (buffer-file-name (get-buffer (car buffer-name-history)))) (dir-path) (file-path))][%(car buffer-name-history)]] |"
				     :immediate-finish 1)
				    ("w" "Website" table-line (file+headline "books/books.org" "Websites")
				     "| [[%^{Link}][%^{Title}]] |" :immediate-finish 1)
				    ("j" "Project" entry (file+headline "projects.org" "Projects")
				     "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
				    ("g" "General" entry (file+headline "notes.org" "General")
				     "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
				    ("n" "Note" entry (file+headline "notes.org" "Notes")
				     "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1))))

(defun add-course (&rest junk)
  "Capture a course via org-mode's `org-capture'."
  ;; "** %^{Course Code} %?%^g\n TITLE: %^{Course Title}\n LECTURER: %^{Lecturer}\n"
  (let ((course-code ""))
    (concat course-code "** " (read-from-minibuffer "Enter course code: ") "%?%^g\n"
	    " " (read-from-minibuffer "Enter course title: ") "\n"
	    " " (read-from-minibuffer "Enter course lecturer: ") "\n")))

(defun file-path (&rest junk)
  "Return the path of a file."
  (buffer-file-name (get-buffer (car buffer-name-history))))

(defun dir-path (&rest junk)
  "Return the path of a directory."
  (car (rassq (get-buffer (car buffer-name-history)) dired-buffers)))

(defun course-code (&rest junk)
  "Search for a COURSE-CODE appearing in 'school.org' and if found move the point to that location."
  (interactive)
  (switch-to-buffer "school.org")
  (goto-char (point-min))
  (let ((str (read-from-minibuffer "Enter course code: ")))
    (when (search-forward (concat "** " str "\t") nil nil)
      (forward-line 9))))

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
	     '("paper"
	       "\\documentclass{paper}
               [NO-DEFAULT-PACKAGES]
               [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-export-latex-listings t)

(add-to-list 'org-entities-user '("neg" "\\neg" t "&not;" "[angled dash]" nil "¬"))
(add-to-list 'org-entities-user '("vdash" "\\vdash" t "&vdash;" "[single turnstyle]" nil "⊢"))
(add-to-list 'org-entities-user '("iff" "\\iff" t "&iff;" "[if and only if]" nil "↔"))
(add-to-list 'org-entities-user '("top" "\\top" t "&top;" "[top (true)]" nil "⊤"))
(add-to-list 'org-entities-user '("bot" "\\bot" t "&bot;" "[bot (false)]" nil "⊥"))

(add-hook 'org-mode-hook 'org-toggle-pretty-entities) ;; toggle UTF-8 unicode symbols
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-M-j") 'org-insert-heading))) ;; M-RET inserts a new heading
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-c q") 'insert-org-quote))) ;; insert quote with C-c q

(define-skeleton insert-org-quote
  "Inserts an org-mode quote."
  "Quote text: "
  "#+BEGIN_QUOTE\n" str "\n#+END_QUOTE")

(defvar org-journal-file "~/Documents/Organisation/journal.org" "Path to org-mode journal file.")
(defvar org-journal-date-format "%Y-%m-%d" "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (unless (org-goto-local-search-headings today nil t)
      ((lambda ()
         (org-insert-heading)
         (insert today)
         (insert "\n\n  \n"))))
    (beginning-of-buffer)
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (backward-char 2)
    (unless (= (current-column) 2)
      (insert "\n\n  "))))

;; =======
;;; eshell
;; =======
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

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color) ;; enable colours in eshell commands

;; ================
;;; mail (and mutt)
;; ================
(autoload 'sendmail "sendmail" "A major mode for reading (and writing) emails." t)
(require 'gnus) ;; TODO: change this to an autoload

(defun mutt-mail-mode-hook ()
  (turn-on-auto-fill) ;; auto-fill is necessary for mails
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;; kills quoted signatures
  (not-modified) ;; haven't changed the buffer
  (mail-text) ;; jumps to the beginning of the mail text
  (setq make-backup-files nil)) ;; No backups necessary

(or (assoc "mutt-" auto-mode-alist) (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))

(add-hook 'mail-mode-hook (lambda () (define-key mail-mode-map (kbd "C-c k") (lambda () (interactive) (save-buffer) (server-edit)))))
(add-hook 'mail-mode-hook 'mutt-mail-mode-hook)

;; ======
;;; dired
;; ======
(require 'dired-x)
;; (require 'dired-details)
;; (dired-details-install)

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; was dired-advertised-find-file
			     (dired-omit-mode 1)
			     (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))) ;; was dired-up-directory

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; hide un-interesting files in dired
      dired-omit-extensions (append dired-latex-unclean-extensions
				    dired-bibtex-unclean-extensions
				    dired-texinfo-unclean-extensions))

(defadvice dired-advertised-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory." ;; stop dired making a new buffer when visiting a directory (i.e. use one buffer for all directories)
  (interactive)
  (let ((orig (current-buffer))
	(filename (dired-get-filename)))
    ad-do-it
    (when (and (file-directory-p filename)
	       (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(defadvice switch-to-buffer-other-window (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(defadvice switch-to-buffer (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(defadvice display-buffer (after auto-refresh-dired (buffer &optional not-this-window frame)  activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(defadvice other-window (after auto-refresh-dired (arg &optional all-frame) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(eval-after-load "dired"   ;; don't remove 'other-window'
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
	    (orig (current-buffer))
	    (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir)) ;; only try dired-goto-subdir if buffer has more than one dir
	   (and (cdr dired-subdir-alist)
		(dired-goto-subdir up))
	   (progn
	     (kill-buffer orig)
	     (dired up)
	     (dired-goto-file dir))))))

(setq dired-guess-shell-alist-user
      (list (list "\\.pdf$" "evince")
	    (list "\\.PDF$" "evince")))

(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map "b" 'dired-browser-find-file)))

(defun dired-browser-find-file ()
  "Dired function to view a file in default web browser."
  (interactive)
  (browse-url (browse-url-file-url (dired-get-filename))))

;; ====
;;; erc
;; ====
;; TODO: setup ERC
(autoload 'erc-select "erc" "IRC client." t)
(autoload 'doctor-doc "doctor") ;; for use with erc doctor
(autoload 'make-doctor-variables "doctor") ;; ...

(require 'erc-match) ;; TODO: change this to an autoload
(require 'erc-join) ;; TODO: change this to an autoload
(require 'erc-track) ;; TODO: change this to an autoload
(require 'erc-fill) ;; TODO: change this to an autoload
(require 'erc-ring) ;; TODO: change this to an autoload
(require 'erc-netsplit) ;; TODO: change this to an autoload
(require 'erc-spelling) ;; TODO: change this to an autoload
(require 'erc-pcomplete) ;; TODO: change this to an autoload
(require 'erc-highlight-nicknames) ;; TODO: change this to an autoload

(and (require 'erc-highlight-nicknames) (add-to-list 'erc-modules 'highlight-nicknames) (erc-update-modules))

(defface erc-prompt-face '((t (:foreground "yellow" :bold t))) "ERC prompt.")

(defvar erc-insert-post-hook)

(erc-autojoin-mode t) ;; enable autojoining
(erc-track-mode t)
(erc-match-mode t)
(erc-fill-mode 0) ;; disable ERC fill
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t) ;; enable ERC timestamp on
(erc-button-mode t)
(erc-spelling-mode t) ;; enable flyspell in ERC

(setq erc-server "irc.freenode.net" ;; default ERC server
      erc-port 6667 ;; default ERC port
      erc-nick "chu"
      erc-user-full-name user-full-name
      erc-email-userid "mathew.ball@gmail.com"
      erc-current-nick-highlight-type 'all ;; highlight the entire messahe where current nickname occurs
      erc-timestamp-format "[%H:%M] " ;; put timestamps on the left
      erc-fill-prefix "        " ;; ...
      ;; erc-full-mode 0 ;; again, disable ERC fill (not sure why I have done it in two places)
      ;; erc-fill-column 90
      erc-timestamp-right-column 61
      erc-track-showcount t ;; show count of unseen messages
      erc-timestamp-only-if-changed-flag nil ;; always show timestamp
      erc-insert-timestamp-function 'erc-insert-timestamp-left ;; insert timestamp in the left column
      erc-kill-buffer-on-part t ;; kill buffers for channels after /part
      erc-kill-queries-on-quit t ;; kill buffers for queries after quitting the server
      erc-kill-server-buffer-on-quit t ;; kill buffers for server messages after quitting the server
      erc-interpret-mirc-color t ;; interpret mIRC-style color commands in IRC chats
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT") ;; ignore JOIN, NICK, PART and QUIT messages
      erc-mode-line-format "%t %a" ;; display only the channel name on the mode-line
      erc-header-line-format nil ;; turn off the topic (header) bar
      header-line-format nil ;; turn off the topic (header) bar
      erc-max-buffer-size 20000 ;; truncate buffers (so they don't hog core)
      erc-truncate-buffer-on-save t
      erc-timestamp-format "[%H:%M] " ;; time format for ERC messages
      erc-input-line-position -1 ;; keep input at the bottom
      ;; erc-keywords '("") ;; highlight specific keywords
      erc-echo-notices-in-minibuffer-flag t ;; notices in minibuffer
      erc-prompt ;; channel specific prompt
      (lambda () (if (and (boundp 'erc-default-recipients) (erc-default-target))
		(erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
	      (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
      erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs" "#stumpwm" "#conkeror" "#lisp" "#org-mode" "#ubuntu-offtopic" "##club-ubuntu"))
      erc-join-buffer 'bury)

;; (add-hook 'erc-after-connect '(lambda (SERVER NICK) (erc-message "PRIVMSG" "NickServ identify password"))) ;; authentication details
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(add-hook 'erc-mode-hook '(lambda () (pcomplete-erc-setup) (erc-completion-mode 1)))
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

(remove-hook 'erc-text-matched-hook 'erc-hide-fools)
(setq erc-pals '()
      erc-fool-highlight-type 'nick ;; highlight entire message
      erc-fools '("ubottu" "fsbot" "rudybot" "lisppaste" "Oxymoron"))

(setq erc-remove-parsed-property nil)

(defvar erc-doctor-id "{Emacs doctor} ") ;; erc doctor

(defun erc-cmd-DOCTOR (&optional last-sender &rest ignore)
  "Get the last message in the channel and doctor it."
  (let ((limit (- (point) 1000))
        (pos (point))
        doctor-buffer
        last-message
        text)
    (when (< limit 0) (setq limit 0)) ;; make sure limit is not negative
    (while (and pos (not (let ((data (get-text-property pos 'erc-parsed))) ;; search backwards for text from someone
                           (and data
                                (string= (aref data 3) "PRIVMSG")
                                (or (not last-sender)
                                    (string= (car (split-string (aref data 2) "!"))
                                             last-sender))))))
      (setq pos (previous-single-property-change
                 pos 'erc-parsed nil limit))
      (when (= pos limit)
        (error "No appropriate previous message to doctor.")))
    (when pos
      (setq last-sender (car (split-string
                              (aref (get-text-property
                                     pos 'erc-parsed) 2) "!"))
            doctor-buffer (concat "*ERC Doctor: " last-sender "*")
            last-message (split-string
                          (replace-regexp-in-string ;; remove punctuation from end of sentence
                           "[ .?!;,/]+$" ""
                           (aref (get-text-property pos
                                                    'erc-parsed) 5)))
            text (mapcar (lambda (s)
                           (intern (downcase s)))
                         (if (string-match ;; remove salutation if it exists
                              (concat "^" erc-valid-nick-regexp
                                      "[:,]*$\\|[:,]+$")
                              (car last-message))
                             (cdr last-message)
                           last-message))))
    (erc-send-message
     (concat erc-doctor-id
             (if (not (erc-query-buffer-p)) ;; only display sender if not in a query buffer
                 (concat last-sender ": "))
             (save-excursion
               (if (get-buffer doctor-buffer)
                   (set-buffer doctor-buffer)
                 (set-buffer (get-buffer-create doctor-buffer))
                 (make-doctor-variables))
               (erase-buffer)
               (doctor-doc text)
               (buffer-string))))))

(defun erc-cmd-SHOW (&rest form)
  "Eval FORM and send the result and the original form as: FORM => (eval FORM)."
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

(defun erc-cmd-UNAME (&rest ignore)
  "Display the result of running `uname -a' to the current ERC
buffer."
  (let ((uname-output
         (replace-regexp-in-string
          "[ \n]+$" "" (shell-command-to-string "uname -a"))))
    (erc-send-message
     (concat "{uname -a} [" uname-output "]"))))

(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
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

(add-to-list 'erc-noncommands-list 'erc-cmd-SHOW)

(defun erc-start-or-switch (&rest junk)
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; if ERC is already active ...
      (erc-track-switch-buffer 1) ;; switch to last active buffer
    (erc :server "irc.freenode.net" :port erc-port :nick erc-nick :full-name erc-user-full-name))) ;; else, start ERC

;; ==========
;;; logic4fun
;; ==========
(autoload 'logic4fun-mode "logic4fun" "A major mode for logic4fun (FINDER)." t)

(defun logic4fun-reload ()
  "Reload logic4fun mode."
  (interactive)
  (if (featurep 'logic4fun)
      (unload-feature 'logic4fun))
  (logic4fun-mode))

;; ================
;;; lambda calculus
;; ================
(require 'lambdacalc) ;; TODO: change this to an autoload

;; =======================
;;; javascript programming
;; =======================
(autoload 'javascript-mode "javascript")
;; (add-hook 'javascript-mode-hook (lambda () (flyspell-prog-mode)))

;; ====================
;;; haskell programming
;; ====================
(autoload 'haskell-mode "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file" "Major mode for editing Haskell code." t)
(setq haskell-font-lock-symbols t) ;; enable unicode symbols for haskell

(let ((fn-list '(turn-on-haskell-doc-mode turn-on-haskell-indent)))
  (mapc (lambda (fn) (add-hook 'haskell-mode-hook fn)) fn-list)) ;; enable doc-mode and indentation for haskell-mode
;; (add-hook 'haskell-mode-hook (lambda () (flyspell-prog-mode)))

;; ==============
;;; c programming
;; ==============
(require 'cc-mode)

(defun linux-c-mode () ;; linus' kernel formatting
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t))

(setq compilation-window-height 8)

(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            (message "compilation errors, press C-x ` to visit") ;; there were errors
          (run-at-time 0.5 nil 'delete-windows-on buf) ;; no errors, dissolve compilation window (0.5 seconds)
          (message "No compilation errors."))))

(add-hook 'c-mode-common-hook '(lambda () (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))
;; (add-hook 'c-mode-hook (lambda () (flyspell-prog-mode)))

;; ===============
;;; c# programming
;; ===============
(autoload 'csharp-mode "csharp-mode" "C# editing code." t)
;; (add-hook 'csharp-mode-hook (lambda () (flyspell-prog-mode)))

;; ===================
;;; python programming
;; ===================
(autoload 'python-mode "python" "Python editing mode." t)
;; (add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))

;; ================
;;; lua programming
;; ================
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'hs-minor-mode)
;; (add-hook 'lua-mode-hook (lambda () (flyspell-prog-mode)))

;; =============
;;; latex markup
;; =============
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)

(setq reftex-enable-partial-scans t ;; make reftex faster
      reftex-save-parse-info t ;; ...
      reftex-use-multiple-selection-buffers t) ;; ...

;; (defun org-mode-reftex-setup () ;; FIXME: fix
;;   "Make reftex work with org-mode."
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;   (file-exists-p (buffer-file-name))
;;   (reftex-parse-all))
;;   (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; RefTeX formats for biblatex (not natbib)
(setq reftex-cite-format
      '((?\C-m . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?x . "[]{%l}")
        (?X . "{%l}")))

(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("cites" "[{}]")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{")
        ("citetitle" "[{")
        ("citetitles" "[{")
        ("headlessfullcite" "[{")))

(setq reftex-cite-prompt-optional-args nil
      reftex-cite-cleanup-optional-args t)

;; (if (eq window-system 'x)
;;     (require 'font-latex))

(setq latex-run-command "pdflatex" ;; use pdflatex to compile LaTeX documents
      reftex-extra-bindings t) ;; enable extra reftex bindings

(add-hook 'latex-mode-hook
	  (lambda () (turn-on-reftex) ;; enable reftex mode
	    (turn-on-custom-latex) ;; enable custom latex bindings
	    (setq ispell-parse 'tex)))

(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map (kbd "C-c C-v") 'latex-view))) ;; enable latex-view
(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map (kbd "C-c C-f") 'latex-file))) ;; enable latex-file
(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map (kbd "C-c C-b") 'latex-bibtex-file))) ;; enable latex-bibtex-file

(defun latex-bibtex-file (&rest junk)
  "Produce a bibliography for current LaTeX document."
  (interactive)
  (call-process "bibtex" nil 0 nil (file-name-sans-extension (buffer-file-name))) ;; execute asynchronous bibtex process
  (message (concat "bibtex called on " (buffer-file-name))))

(defun latex-file (&rest junk)
  "Produce current LaTeX document as PDF."
  (interactive)
  (tex-file)
  (switch-to-buffer (current-buffer)) ;; refocus on main window
  (delete-other-windows)) ;; delete remaining window

(defun latex-view (&rest junk)
  "Produce and preview current LaTeX document as PDF."
  (interactive)
  (tex-file)
  (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (message pdf-file) ;; print pdf-file in minibuffer
    (call-process "evince" nil 0 nil pdf-file)) ;; execute asynchronous evince process
    (switch-to-buffer (current-buffer)) ;; refocus on main window
    (delete-other-windows)) ;; delete remaining window

(defmacro latex-skeleton (code) ;; inserts a custom clause skeleton in a LaTeX document
  (let ((func (intern (concat "latex-skeleton-" code)))
	(doc (format "Inserts a %s clause in a LaTeX document." code)))
    `(define-skeleton ,func ,doc "String: "
       "\\" ,code  "{" str | "insert text" "}")))

(defun latex-smart-underscore ()
  "Smart \"_\" key: insert \"_{}\". If the underscore key is pressed a second time, \"_{}\" is removed and replaced by the underscore."
  (interactive)
  (let ((assign-len (length "_{")))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;; check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at "_{}")))
        (progn ;; if we are currently looking at ess-S-assign, replace it with _
          (forward-char)
          (delete-backward-char (+ 1 assign-len))
          (insert "_"))
      (delete-horizontal-space)
      (insert "_{}")
      (backward-char))))

(defun latex-smart-caret ()
  "Smart \"^\" key: insert \"^{}\". If the caret key is pressed a second time, \"^{}\" is removed and replaced by the caret."
  (interactive)
  (let ((assign-len (length "^{")))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;; check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at "\\^{}"))) ;; looking-at reads regexp, so need to escape the caret character
        (progn ;; if we are currently looking at ess-S-assign, replace it with ^
          (forward-char)
          (delete-backward-char (+ 1 assign-len))
          (insert "^"))
      (delete-horizontal-space)
      (insert "^{}")
      (backward-char))))

(defun latex-smart-period ()
  "Smart \".\" key: insert \".  \n\". If the period key is pressed a second time, \".  \n\" is removed and replaced by the period."
  (interactive)
  (let ((assign-len (length ".  %%\n")))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;; check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at "\\.  %%")))
        (progn ;; if we are currently looking at ess-S-assign, replace it with _
          (delete-backward-char assign-len)
          (insert "."))
      (delete-horizontal-space)
      (insert ".  %%\n"))))

(latex-skeleton "textbf") ;; inserts a bold clause
(latex-skeleton "footnote") ;; inserts a footnote
(latex-skeleton "texttt") ;; inserts a tele-type clause
(latex-skeleton "emph") ;; inserts an emphasis clause
(latex-skeleton "textsc") ;; inserts a small capitals clause

(defun turn-on-custom-latex () ;; activate custom latex bindings
  (define-key latex-mode-map (kbd "C-c b") 'latex-skeleton-textbf) ;; bind bold to keyboard
  (define-key latex-mode-map (kbd "C-c f") 'latex-skeleton-footnote) ;; bind footnote to keyboard
  (define-key latex-mode-map (kbd "C-c t") 'latex-skeleton-texttt) ;; bind tele-type to keyboard
  (define-key latex-mode-map (kbd "C-c e") 'latex-skeleton-emph) ;; bind emphasise to keyboard
  (define-key latex-mode-map (kbd "C-c s") 'latex-skeleton-textsc) ;; bind small-capitals to keyboard
  (define-key latex-mode-map (kbd "_") 'latex-smart-underscore) ;; bind _ to smart underscore
  (define-key latex-mode-map (kbd "^") 'latex-smart-caret) ;; bind ^ to smart caret
  (define-key latex-mode-map (kbd ".") 'latex-smart-period)) ;; bind . to smart period

;; =============
;;; desktop save
;; =============
(desktop-save-mode 1) ;; enable desktop save mode

(setq desktop-path '("~/.emacs.d/")
      desktop-dirname "~/.emacs.d/"
      desktop-base-file-name "emacs-desktop"
      history-length 250)

(add-to-list 'desktop-globals-to-save 'file-name-history)

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

;; ==========
;;; smart tab
;; ==========
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

;; =================
;;; google functions
;; =================
(defun google-define (&rest junk)
  "Search for the definition of the word under cursor on google."
  (interactive)
  (browse-url (concat "http://www.google.com/search?hl=en&q=define%3A+" (thing-at-point 'word))))

(defun google-it (search-string)
  "Search for 'search-string' on google."
  (interactive "sSearch for: ")
  (browse-url (concat "http://www.google.com/search?hl=en&q=" (url-hexify-string (encode-coding-string search-string 'utf-8)))))

;; ====================
;;; word count function
;; ====================
(defun word-count (&optional filename)
  "Returns the word count of the current buffer.  If 'filename' is not nil, returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4) (string= (substring lcase-file -4 nil) ".tex"))
                (progn ;; This is a LaTeX document, so DeTeX it!
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string (concat "detex < " buffer-file " > " filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (split-string (shell-command-to-string (concat "wc -w " filename)) " "))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result)))))

;; ===================
;;; dot file structure
;; ===================
(defun show-dot-file-structure (&rest junk)
  "Show the outline structure of a configuration file."
  (interactive)
  (progn
    (occur (concat "^" (make-string 3 (aref comment-start 0)) "+"))
    (other-window 1)))

;; =================
;;; bugs/fixes/todos
;; =================
(defun show-bugs-fixes-todos (&rest junk)
  "Show the outline-mode structure listing any bugs, fixes or TODOs in source code comments."
  (interactive)
  (progn
    (occur "\\<\\(FIXME\\|TODO\\|BUG\\): ")
    (other-window 1)))

;; ====================
;;; switch to dot emacs
;; ====================
(defun switch-to-dot-emacs (&rest junk)
  "Switch to ~/.emacs buffer (or evaluate the buffer if the ~/.emacs file is present)."
  (interactive)
  (if (equal (buffer-name) ".emacs")
      (eval-buffer) ;; evaluate the current buffer
    (find-file "~/.emacs"))) ;; switch to the ~/.emacs file

;; ================
;;; startup message
;; ================
(defun display-startup-echo-area-message (&rest junk)
  "Clear the message buffer initially."
  (message ""))

;; ===============
;;; tip of the day
;; ===============
(defun totd ()
  (interactive)
  (random t) ;; seed with time-of-day
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Tip of the day:\n===============\n\n"
               (describe-function command)
               "\n\nInvoke with:\n============\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

;; =====================
;;; evaluate and replace
;; =====================
(defun eval-and-replace ()
  "Replace the preceding s-expression with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; =================
;;; show parenthesis
;; =================
(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area. Has no effect if the character before point is not of the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    (if (char-equal (char-syntax (char-before (point))) ?\))
        (setq matching-text (blink-matching-open))) ;; only call 'blink-matching-open' if the character before point is a close parentheses type character
    (if (not (null matching-text))
        (message matching-text)))) ;; otherwise, there's not really any point, and 'blink-matching-open' would just echo "Mismatched parentheses", which gets really annoying

;; =============================
;;; jump to matching parenthesis
;; =============================
(defun jump-to-matching-parenthesis (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; ===========================
;;; highlight special comments
;; ===========================
(setq special-mode-hooks '(emacs-lisp-mode-hook lisp-interaction-mode-hook haskell-mode-hook shell-script-mode muttrc-mode))

(mapc (lambda (mode-hook) (add-hook mode-hook (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))))) special-mode-hooks)

;; ===================
;;; start emacs server
;; ===================
(require 'server) ;; TODO: change to an autoload
(when (and (functionp 'server-running-p) (not (server-running-p))) ;; don't start the server unless we can verify it isn't running
  (server-mode t) ;; enter server mode
  (server-start)) ;; FIXME: fix (should be handled in .stumpwmrc)

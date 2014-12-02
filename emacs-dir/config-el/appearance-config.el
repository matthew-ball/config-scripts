;;; appearance-config.el --- Configuration for user interface

;; Copyright (C) 2013  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: aesthetics

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

;; Configuration settings and options for the GNU Emacs user-interface.

;;; Code:

(defgroup user-appearance nil "Custom appearance variables." :group 'user-variables)

;;; IMPORTANT: X frame specific
(defun decorate-frame ()
  "Apply features to a new frame."
  ;; (set-face-attribute 'default nil :height 90 :font "Terminus-10")
  (set-face-attribute 'default nil :height 90)
  (setq frame-title-format "%b" ;; NOTE: set frame title properties
	icon-title-format "%b")
  (load-theme 'tango)
  ;;(set-face-attribute 'mode-line nil :box nil) ;; TODO: this doesn't activate
  )

;; NOTE: apply `load-frame' to a graphical emacs instance
(when (display-graphic-p)
  (decorate-frame))

;; SOURCE: `http://ubuntuforums.org/archive/index.php/t-183638.html'
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; NOTE: apply `load-frame' to an emacsclient frame
;; SOURCE: `http://www.emacswiki.org/emacs/ColorThemeQuestions'
(defun turn-on-frame-decorations (frame)
  "Decorate new frame FRAME with colour theme."
  (select-frame frame)
  (when (display-graphic-p)
    (decorate-frame)))

(add-hook 'after-make-frame-functions #'turn-on-frame-decorations)

;; NOTE: minimal GUI elements ...
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; NOTE: hide the menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; NOTE: hide the tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; NOTE: hide the scroll bar
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1)) ;; NOTE: turn off blinking cursor
(when (fboundp 'tooltip-mode) (tooltip-mode -1)) ;; NOTE: turn off tooltip
(when (fboundp 'fringe-mode) (set-fringe-mode '(1 . 0))) ;; NOTE: set fringe to 1px on left side only

;;; IMPORTANT: error bell
(setq ring-bell-function 'ignore) ;; NOTE: ignore error bell

;;; IMPORTANT: visual lines
;; SOURCE: `http://www.emacswiki.org/emacs/VisualLineMode'
;;(global-visual-line-mode t) ;; NOTE: enable visual line mode for all buffers (i.e. globally)

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

;;; IMPORTANT: show matching parenthesis
;; SOURCE: `http://emacswiki.org/emacs/ShowParenMode'
(show-paren-mode t)

(setq show-paren-style 'mixed
      ;;show-paren-style 'parenthesis
      show-paren-delay 0.0)

;;; IMPORTANT: line numbers
;; SOURCE: `http://www.emacswiki.org/emacs/line-num.el'
;; (autoload 'linum-mode "linum" "Display line numbers." t)

;; (add-hook 'text-mode-hook #'linum-mode)
;; (add-hook 'prog-mode-hook #'linum-mode)

;;; IMPORTANT: truncate lines
(defun turn-on-truncate-lines ()
  "..."
  (setq truncate-lines t))

(add-hook 'prog-mode-hook #'turn-on-truncate-lines)

;;; IMPORTANT: indicate empty lines
(toggle-indicate-empty-lines)

;;; IMPORTANT: mode line
;;(setq mode-line-format nil) ;; NOTE: removes the mode-line

;; SOURCE: `http://www.emacswiki.org/emacs/ModeLineConfiguration'
(setq line-number-mode t ;; NOTE: turn on line numbers in the mode line
      column-number-mode t ;; NOTE: turn on column numbers in the mode line
      size-indication-mode nil) ;; NOTE: do not show file size in mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayTime'
(require 'time)

(defcustom custom-cities '(("Australia/Canberra"  "Canberra")
			   ("America/Los_Angeles" "Los Angeles")
			   ("America/New_York"    "New York")
			   ("America/Montreal"    "Montreal")
			   ("Europe/London"       "London")
			   ("Europe/Amsterdam"    "Amsterdam")
			   ("Europe/Helsinki"     "Helsinki")
			   ("Europe/Moscow"       "Moscow")
			   ("Asia/Bangkok"        "Chiang Mai"))
  "Custom cities." :group 'user-appearance :type 'list)

(after "time"
  (setq display-time-default-load-average nil
	display-time-day-and-date nil
	display-time-24hr-format nil
	display-time-format "%I:%M%p"
	display-time-world-buffer-name "*world-clock*")

  (defun add-world-city (city) (push city display-time-world-list))
  (defun custom-world-cities () (mapc #'add-world-city custom-cities))

  (custom-world-cities)
  (display-time-mode t)) ;; NOTE: display time status in the mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayBatteryMode'
(after "battery"
  (setq battery-mode-line-format " [%b%p%\%]"
	battery-mode-line-limit 60))

;; SOURCE: `https://github.com/hh/emacs/blob/master/.emacs.d/wireless.el'
(after "wireless"
  (setq wireless-mode-line-format " [%k%\%]"))

;; IMPORTANT: laptop
(defcustom custom-laptop nil "This system is (or isn't) a laptop." :group 'user-appearance :type 'boolean)

(defun laptop ()
  (interactive)
  (require 'battery)
  (require 'wireless)

  (display-battery-mode t) ;; NOTE: display battery status in the mode line
  (display-wireless-mode t)  ;; NOTE: display wireless status in the mode line

  (message "Activating `battery' and `wireless' display."))

(when custom-laptop
  (laptop))

;;; IMPORTANT: text folding
;; SOURCE: `http://emacswiki.org/emacs/HideShow'
;;(autoload 'hs-minor-mode "hideshow" "Fold code with GNU Emacs." t)
(require 'hideshow)

(after "hideshow"
  ;; TODO: add custom modes
  ;; (defvar hs-special-modes-alist
  ;;   (mapcar 'purecopy
  ;;   '((c-mode "{" "}" "/[*/]" nil nil)
  ;;     (c++-mode "{" "}" "/[*/]" nil nil)
  ;;     (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
  ;;     (java-mode "{" "}" "/[*/]" nil nil)
  ;;     (js-mode "{" "}" "/[*/]" nil))))

  (setq hs-hide-comments-when-hiding-all nil) ;; NOTE: don't hide the comments when launching `hs-hide-all'
  (setq hs-isearch-open t) ;; NOTE: set isearch opens folded comments; where x is code, comments, t (both), or nil (neither)

  (add-to-list 'hs-special-modes-alist
	       '(ruby-mode "\\(class\\|def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#" (lambda (arg) (ruby-end-of-block)) nil))

  ;; (defvar hs-modes-hooks-list '(lisp-mode-hook
  ;; 				emacs-lisp-mode-hook
  ;; 				shell-script-mode-hook
  ;; 				haskell-mode-hook
  ;; 				latex-mode-hook))

  ;; (dolist (hook hs-modes-hooks-list)
  ;;   (add-hook hook #'hs-minor-mode))

  ;; (add-hook 'lisp-mode-hook       #'hs-minor-mode)
  ;; (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
  ;; (add-hook 'shell-script-mode    #'hs-minor-mode)
  ;; (add-hook 'haskell-mode-hook    #'hs-minor-mode)
  (add-hook 'latex-mode-hook #'hs-minor-mode)

  (setq hs-set-up-overlay #'display-code-line-counts))

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

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display (format "... / %d" (count-lines (overlay-start ov) (overlay-end ov))))))

;;; IMPORTANT: prettyify symbols
;; SOURCE: `http://ergoemacs.org/emacs/emacs_pretty_lambda.html'

(setq lisp--prettify-symbols-alist '(("lambda"  . ?λ)
				     (">=" . ?≥)
				     ("<=" . ?≤)
				     ("member" . ?∈)
				     ("forall" . ?∀)
				     ("exists" . ?∃)))

(global-prettify-symbols-mode 1)

;;; IMPORTANT: diminish
;; TODO: move to `user-config.el'
;; SOURCE: `http://www.emacswiki.org/emacs/DiminishedModes'
(autoload 'diminish "diminish" "Turn off the textual mode indicator in the mode line." t)

(defmacro diminish-minor-mode (package-name &optional mode-name)
  (let ((name (if (eq mode-name nil)
		   `',(intern (concat package-name "-mode"))
		 mode-name)))
    `(after ,package-name (diminish ,name))))

(diminish-minor-mode "abbrev")
(diminish-minor-mode "eldoc")
(diminish-minor-mode "auto-complete")
(diminish-minor-mode "cwarn")
(diminish-minor-mode "elisp-slime-nav")
(diminish-minor-mode "eproject")
(diminish-minor-mode "flyspell")
(diminish-minor-mode "flymake")
(diminish-minor-mode "glasses")
(diminish-minor-mode "haskell-doc")
(diminish-minor-mode "haskell-indent")
(diminish-minor-mode "haskell-indentation")
(diminish-minor-mode "longlines")
(diminish-minor-mode "org-indent")
(diminish-minor-mode "paredit")
(diminish-minor-mode "projectile")
(diminish-minor-mode "reftex")
(diminish-minor-mode "undo-tree")
(diminish-minor-mode "face-remap" 'buffer-face-mode)
(diminish-minor-mode "hideshow" 'hs-minor-mode)
(diminish-minor-mode "hilit-chg" 'highlight-changes-mode)
(diminish-minor-mode "magit" 'magit-auto-revert-mode)
(diminish-minor-mode "simple" 'visual-line-mode)
(diminish-minor-mode "yasnippet" 'yas-minor-mode)
(diminish-minor-mode "w3m-lnum")
(diminish-minor-mode "geiser-mode" 'geiser-mode)
(diminish-minor-mode "geiser-autodoc")
;; ---
;; (after "abbrev" (diminish 'abbrev-mode))
;; (after "eldoc" (diminish 'eldoc-mode))
;; (after "auto-complete" (diminish 'auto-complete-mode))
;; (after "cwarn" (diminish 'cwarn-mode))
;; (after "elisp-slime-nav" (diminish 'elisp-slime-nav-mode))
;; (after "eproject" (diminish 'eproject-mode))
;; (after "face-remap" (diminish 'buffer-face-mode))
;; (after "flyspell" (diminish 'flyspell-mode))
;; (after "flymake" (diminish 'flymake-mode))
;; (after "glasses" (diminish 'glasses-mode))
;; (after "haskell-doc" (diminish 'haskell-doc-mode))
;; (after "haskell-indent" (diminish 'haskell-indent-mode))
;; (after "haskell-indentation" (diminish 'haskell-indentation-mode))
;; (after "hideshow" (diminish 'hs-minor-mode))
;; (after "hilit-chg" (diminish 'highlight-changes-mode))
;; (after "longlines" (diminish 'longlines-mode))
;; (after "magit" (diminish 'magit-auto-revert-mode))
;; (after "org-indent" (diminish 'org-indent-mode))
;; (after "paredit" (diminish 'paredit-mode))
;; (after "projectile" (diminish 'projectile-mode))
;; (after "reftex" (diminish 'reftex-mode))
;; (after "simple" (diminish 'visual-line-mode))
;; (after "undo-tree" (diminish 'undo-tree-mode))
;; (after "w3m-lnum" (diminish 'w3m-lnum-mode))
;; (after "yasnippet" (diminish 'yas-minor-mode))
;; (after "geiser-mode" (diminish 'geiser-mode))
;; (after "geiser-autodoc" (diminish 'geiser-autodoc-mode))
;; TODO: ...
;;(after "gnus-sum" (diminish 'gnus-agent-mode))
;;(after "gnus-topic" (diminish 'gnus-topic-mode))

;; TODO: ...
(defmacro diminish-major-mode (package-name &optional mode-name)
  `(,package-name ,mode-name))

;; NOTE: this is unofficially `diminish' for major modes
(defcustom mode-line-cleaner-alist '((c-mode	   . "C")
				     (c++-mode	   . "C++")
				     (dired-mode	   . "Dired")
				     (lisp-mode	   . "Common Lisp")
				     (emacs-lisp-mode . "Emacs Lisp")
				     (gnus-group-mode . "Email")
				     (eshell-mode     . "Eshell")
				     (erc-mode        . "ERC")
				     (haskell-mode	   . "Haskell")
				     (help-mode	   . "Help")
				     (ibuffer-mode    . "iBuffer")
				     (org-mode	   . "Organisation")
				     (python-mode	   . "Python"))
  "Alist for `clean-mode-line'." :group 'user-appearance :type 'list)

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
	   do (let* ((mode (car cleaner))
		     (mode-str (cdr cleaner))
		     (old-mode-str (cdr (assq mode minor-mode-alist))))
		(when old-mode-str
		  (setcar old-mode-str mode-str))
		;; NOTE: major mode
		(when (eq mode major-mode)
		  (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook #'clean-mode-line)

;;; IMPORTANT: hide the mode-line
;; SOURCE: `http://bzg.fr/emacs-hide-mode-line.html'
;; (defvar-local hidden-mode-line-mode nil)

;; TODO: rename
;; (define-minor-mode hidden-mode-line-mode
;;   "Minor mode to hide the mode-line in the current buffer."
;;   :init-value nil
;;   :global t
;;   :variable hidden-mode-line-mode
;;   :group 'editing-basics
;;   (if hidden-mode-line-mode
;;       (setq hide-mode-line mode-line-format
;;             mode-line-format nil)
;;     (setq mode-line-format hide-mode-line
;;           hide-mode-line nil))
;;   (force-mode-line-update)
;;   ;; NOTE: apparently force-mode-line-update is not always enough to redisplay the mode-line
;;   (redraw-display)
;;   (when (and (called-interactively-p 'interactive)
;;              hidden-mode-line-mode)
;;     (run-with-idle-timer
;;      0 nil 'message
;;      (concat "Hidden Mode Line Mode enabled.  "
;;              "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; (defun hide-mode-line ()
;;   "Hide the mode-line in every buffer."
;;   (interactive)
;;   (hidden-mode-line-mode)
;;   (add-hook 'after-change-major-mode-hook #'hidden-mode-line-mode))

;; (defun show-mode-line ()
;;   "Show the mode-line in every buffer."
;;   (interactive)
;;   (remove-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)
;;   (hidden-mode-line-mode))

(provide 'appearance-config)
;;; appearance-config.el ends here

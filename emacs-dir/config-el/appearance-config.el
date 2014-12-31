;;; appearance-config.el --- Configuration for user interface

;; Copyright (C) 2008-2014  Matthew Ball

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
  ;; (load-theme 'tango)
  ;; (set-face-attribute 'default nil :height 90 :font "Terminus-10")
  ;; (set-face-attribute 'mode-line nil :box nil) ;; TODO: this doesn't activate
  (set-face-attribute 'default nil :height 90)
  (setq frame-title-format "%b" ;; NOTE: set frame title properties
	icon-title-format "%b"))

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
			   ("America/Montreal"    "Montreal")
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

  (custom-world-cities))

;; SOURCE: `https://github.com/hh/emacs/blob/master/.emacs.d/wireless.el'
;; (after "wireless"
;;   (setq wireless-mode-line-format "[w: %k%\%] "))

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayBatteryMode'
;; (after "battery"
;;   (setq ;;battery-mode-line-format "[b: %b%p%\%] "
;; 	battery-mode-line-limit 65))

;; IMPORTANT: laptop
;; (defcustom custom-laptop nil "This system is (or isn't) a laptop." :group 'user-appearance :type 'boolean)

;; (setq custom-laptop t)

;; (defun laptop-mode ()
;;   (interactive)
;;   (require 'wireless)

;;   ;; (display-time-mode t) ;; NOTE: display time status in the mode line
;;   (display-battery-mode t) ;; NOTE: display battery status in the mode line
;;   ;; (display-wireless-mode t)  ;; NOTE: display wireless status in the mode line

;;   (message "Enable laptop specific settings."))

;; (when custom-laptop
;;   (laptop-mode))

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
;; (setq lisp--prettify-symbols-alist '(("lambda"  . ?λ)
;; 				     (">=" . ?≥)
;; 				     ("<=" . ?≤)
;; 				     ("member" . ?∈)
;; 				     ("forall" . ?∀)
;; 				     ("exists" . ?∃)
;; 				     ("and" . ?∧)
;; 				     ("or" . ?∨)
;; 				     ;; ("if" . ?→)
;; 				     ("not" . ?¬)))

;; (when (display-graphic-p)
;;   (global-prettify-symbols-mode t))

;; TODO: ...
;; (defmacro diminish-major-mode (package-name &optional mode-name)
;;   `(,package-name ,mode-name))

;; NOTE: this is unofficially `diminish' for major modes
(defcustom mode-line-cleaner-alist '((c-mode . "C")
				     (c++-mode . "C++")
				     (dired-mode . "Dired")
				     (lisp-mode . "Common Lisp")
				     (emacs-lisp-mode . "Emacs Lisp")
				     (gnus-group-mode . "Email") ;; TODO: `user-config.el'
				     (eshell-mode . "Eshell")
				     (erc-mode . "ERC") ;; TODO: `user-config.el'
				     (haskell-mode . "Haskell") ;; TODO: `user-config.el'
				     (help-mode . "Help")
				     (ibuffer-mode . "iBuffer")
				     (org-mode . "Organisation")
				     (python-mode . "Python"))
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

(provide 'appearance-config)
;;; appearance-config.el ends here

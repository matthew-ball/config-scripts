;;; appearance-config.el --- Configuration for user interface

;; Copyright (C) 2013  Matthew Ball

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

;; Configuration settings and options for the GNU Emacs user-interface.

;;; Code:

;;; IMPORTANT: appearance
;; SOURCE: `http://emacswiki.org/emacs/ColorTheme'
;; SOURCE: `http://color-theme-select.heroku.com/'
;;(autoload 'color-theme-initialize "color-theme" "Colour theme for GNU Emacs." t)

(defun load-frame ()
  "Apply features to a new frame."
  (set-face-attribute 'default nil :height 80) ;; NOTE: reduce font-size slightly
  (setq color-theme-is-global t ;; ...
	frame-title-format "%b" ;; NOTE: set frame title properties
	icon-title-format "%b")
  ;; (color-theme-zenburn)
  (load-theme 'tango-dark)
  )

(after "color-theme"
  (load-frame))

;; IMPORTANT: X server specific
(when (display-graphic-p)
  (load-frame))

;; IMPORTANT: apply colour theme to a GNU Emacs frame (i.e. `emacsclient')
;; SOURCE: `http://www.emacswiki.org/emacs/ColorThemeQuestions'
(defun decorate-frame (frame)
  "Decorate new frame FRAME with colour theme."
  (select-frame frame)
  (when (display-graphic-p)
    (load-frame)))

(add-hook 'after-make-frame-functions 'decorate-frame)

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
(global-visual-line-mode t) ;; NOTE: enable visual line mode for all buffers (i.e. globally)
;; TODO: this needs to be activated only for selected modes
;; DEBUG: (add-to-hook 'text-mode-hook '(turn-on-visual-line-mode))

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

;;; IMPORTANT: show matching parenthesis
;; SOURCE: `http://emacswiki.org/emacs/ShowParenMode'
(show-paren-mode t)
(setq show-paren-style 'parenthesis
      show-paren-delay 0.0)

;;; IMPORTANT: line numbers
;; SOURCE: `http://www.emacswiki.org/emacs/line-num.el'
;; (autoload 'linum-mode "linum" "Display line numbers." t)

;; BUG: this does not like `doc-view-mode'
;; (add-hook 'find-file-hook (lambda () (linum-mode 1))) ;; NOTE: turn on linum-mode if in a file

;;; IMPORTANT: indicate empty lines
;; (toggle-indicate-empty-lines)

;;; IMPORTANT: mode line
;;(setq mode-line-format nil) ;; NOTE: removes the mode-line

;; SOURCE: `http://www.emacswiki.org/emacs/ModeLineConfiguration'
(setq line-number-mode t ;; NOTE: turn on line numbers in the mode line
      column-number-mode t ;; NOTE: turn on column numbers in the mode line
      size-indication-mode nil) ;; NOTE: do not show file size in mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayTime'
;; (require 'time)

;; (display-time-mode t) ;; NOTE: display time status in the mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayBatteryMode'
;; (require 'battery)

;; (display-battery-mode t) ;; NOTE: display battery status in the mode line

;; (setq battery-echo-area-format "Power %L, battery %B (%p%% load, remaining time %t)"
;;       battery-load-critical 10
;;       battery-load-low 25
;;       battery-mode-line-format " [%b%p%%]" ;; NOTE: default
;;       battery-update-interval 60
;;       battery-status-function 'battery-linux-sysfs ;; NOTE: default
;;       ;;battery-status-function 'battery-linux-proc-acpi
;;       ;;battery-status-function 'battery-linux-proc-apm
;;       ;;battery-mode-line-limit 60 ;; NOTE: display battery status when battery has less than 60% charge
;;       )

;;; IMPORTANT: text folding
;; SOURCE: `http://emacswiki.org/emacs/HideShow'
(autoload 'hs-minor-mode "hideshow" "Fold code with GNU Emacs." t)

(after "hideshow"
  ;; TODO: add custom modes
  ;; (defvar hs-special-modes-alist
  ;;   (mapcar 'purecopy
  ;;   '((c-mode "{" "}" "/[*/]" nil nil)
  ;;     (c++-mode "{" "}" "/[*/]" nil nil)
  ;;     (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
  ;;     (java-mode "{" "}" "/[*/]" nil nil)
  ;;     (js-mode "{" "}" "/[*/]" nil))))

  (setq hs-hide-comments nil) ;; NOTE: hide the comments too when you do a 'hs-hide-all'
  (setq hs-isearch-open 't) ;; NOTE: set isearch opens folded comments; where x is code, comments, t (both), or nil (neither)

  ;; (add-hook 'lisp-mode-hook       'hs-minor-mode)
  ;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  ;; (add-hook 'shell-script-mode    'hs-minor-mode)
  ;; (add-hook 'haskell-mode-hook    'hs-minor-mode)
  (add-hook 'latex-mode-hook 'hs-minor-mode)

  (setq hs-set-up-overlay 'display-code-line-counts))

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
    (overlay-put ov 'display
		 (format "... / %d"
			 (count-lines (overlay-start ov)
				      (overlay-end ov))))))

;;; IMPORTANT: diminish
;; SOURCE: `http://www.emacswiki.org/emacs/DiminishedModes'
(autoload 'diminish "diminish" "Turn off the textual mode indicator in the mode line." t)

(eval-after-load "flyspell" '(diminish 'flyspell-mode ""))
(eval-after-load "flymake" '(diminish 'flymake-mode ""))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode ""))
(eval-after-load "simple" '(diminish 'visual-line-mode ""))
(eval-after-load "simple" '(diminish 'global-visual-line-mode ""))
(eval-after-load "eldoc" '(diminish 'eldoc-mode ""))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode ""))
(eval-after-load "paredit" '(diminish 'paredit-mode ""))
(eval-after-load "haskell-doc" '(diminish 'haskell-doc-mode ""))
(eval-after-load "haskell-indent" '(diminish 'haskell-indent-mode ""))
(eval-after-load "reftex" '(diminish 'reftex-mode ""))
(eval-after-load "glasses" '(diminish 'glasses-mode ""))
(eval-after-load "face-remap" '(diminish 'buffer-face-mode ""))
(eval-after-load "abbrev" '(diminish 'abbrev-mode ""))
(eval-after-load "hilit-chg" '(diminish 'highlight-changes-mode ""))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode ""))
(eval-after-load "longlines" '(diminish 'longlines-mode ""))
(eval-after-load "org-indent" '(diminish 'org-indent-mode ""))
(eval-after-load "w3m-lnum" '(diminish 'w3m-lnum-mode ""))
(eval-after-load "cwarn" '(diminish 'cwarn-mode ""))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode ""))
;;(eval-after-load "workgroups" '(diminish 'workgroups-mode ""))

;;; IMPORTANT: adaptive text wrap
(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap" "Adaptive wrap for text mode buffers." t)

(add-hook 'text-mode-hook (lambda () (adaptive-wrap-prefix-mode t)))

(provide 'appearance-config)
;;; appearance-config.el ends here

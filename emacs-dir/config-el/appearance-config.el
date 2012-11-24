;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/appearance-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:05:45 EST

;;; COMMENT: appearance
;; SOURCE: `http://emacswiki.org/emacs/ColorTheme'
;; (autoload 'color-theme "color-theme" "Colour theme for GNU Emacs." t)
;; (autoload 'color-theme-zenburn "zenburn" "The `zenburn' colour theme for GNU Emacs." t)

;; COMMENT: apply colour theme to a GNU Emacs frame (i.e. `emacsclient')
;; SOURCE: `http://www.emacswiki.org/emacs/ColorThemeQuestions'
(defun decorate-frame (frame)
  "Decorate new frame FRAME with `zenburn' colour theme."
  (select-frame frame)
  (let ((colour-theme-is-global nil))
    (require 'color-theme)
    ;; (eval-after-load "color-theme"
    ;;   '(color-theme-zenburn)) ;; NOTE: apply `color-theme-zenburn' theme to a new frame ;; ERROR: doesn't work
    ;;(color-theme-zenburn)
    (when (window-system)
      (progn ;; NOTE: X session
	(set-face-attribute 'default nil :height 80)
	(setq frame-title-format "%b"
	      icon-title-format "%b")))))

(add-hook 'after-make-frame-functions 'decorate-frame)

;; COMMENT: this is for straight up emacs (i.e. `emacs')
;; (when (eq window-system 'x)  ;; NOTE: when using x windows system
;;   (require 'color-theme)
;;   (require 'zenburn)
;;   (eval-after-load "color-theme" '(color-theme-zenburn)) ;; NOTE: apply `color-theme-zenburn' colour theme
;;   (set-face-attribute 'default nil :height 90) ;; NOTE: change font size
;;   (setq frame-title-format "%b"
;; 	icon-title-format "%b"))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; NOTE: hide the menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; NOTE: hide the tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; NOTE: hide the scroll bar
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1)) ;; NOTE: turn off blinking cursor
(when (fboundp 'tooltip-mode) (tooltip-mode -1)) ;; NOTE: turn off tooltip
;;(when (fboundp 'fringe-mode) (fringe-mode -1)) ;; NOTE: turn off the fringe
(when (fboundp 'fringe-mode) (set-fringe-mode '(1 . 0))) ;; NOTE: set fringe to 1px on left side only

;;; COMMENT: visual lines
;; SOURCE: `http://www.emacswiki.org/emacs/VisualLineMode'
(global-visual-line-mode t) ;; NOTE: enable visual line mode for all buffers (i.e. globally)

;;; COMMENT: line numbers
;; SOURCE: `http://www.emacswiki.org/emacs/line-num.el'
;; (autoload 'linum-mode "linum" "Display line numbers." t)

;; (add-hook 'find-file-hook (lambda () (linum-mode 1))) ;; NOTE: turn on linum-mode if in a file ;; WARNING: this does not like `doc-view-mode'

;;; COMMENT: ruler mode
;; SOURCE: `http://www.emacswiki.org/emacs/RulerMode'
;; (autoload 'rule-mode "ruler-mode" "Display a ruler which measures columns." t)

;; (add-hook 'find-file-hook (lambda () (ruler-mode 1))) ;; NOTE: turn on ruler-mode if in a file

;;; COMMENT: indicate empty lines
;; (toggle-indicate-empty-lines)

;;; COMMENT: show matching parenthesis
;; SOURCE: `http://emacswiki.org/emacs/ShowParenMode'
(show-paren-mode t)
(setq show-paren-style 'parenthesis
      show-paren-delay 0.0)

;;; COMMENT: mode line
;; SOURCE: `http://www.emacswiki.org/emacs/ModeLineConfiguration'
(setq line-number-mode 1 ;; NOTE: turn on line numbers in the mode line
      column-number-mode 1 ;; NOTE: turn on column numbers in the mode line
      size-indication-mode t ;; NOTE: show file size in mode line
      )

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayTime'
(display-time-mode t) ;; NOTE: display time status in the mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayBatteryMode'
(display-battery-mode t) ;; NOTE: display battery status in the mode line

;; SOURCE: `http://gitorious.org/wireless'
(require 'wireless)
(display-wireless-mode t) ;; NOTE: display wireless connection strength in mode line

;; (which-function-mode t) ;; NOTE: show the current function in the mode line

;;; COMMENT: column identifiers
;; (setq fill-column 80) ;; NOTE: column width limit (DEFAULT: 80)

;;; COMMENT: fill column indicator
;; SOURCE: `http://emacswiki.org/emacs/FillColumnIndicator'
;; (require 'fill-column-indicator)
;; (fci-mode 1)

;;; COMMENT: whitespace mode
;; SOURCE: `http://emacswiki.org/emacs/WhiteSpace'
;; ;; NOTE: this mode highlights any text beyond the limit defined in `whitespace-line-column'
;; (require 'whitespace)
;; (autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
;; (autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)

;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t) ;; TODO: this should only be in the `turn-on-general-programming-mode' function

;;; COMMENT: highlight changes mode
;; SOURCE: `http://www.emacswiki.org/emacs/TrackChanges'
;; (global-highlight-changes-mode t) ;; NOTE: enable highlight changes mode in all buffers

;; TODO: create key-bindings to jump around changes.

;;; COMMENT: code folding
;; SOURCE: `http://emacswiki.org/emacs/HideShow'
;; (require 'hideshow)
(autoload 'hs-minor-mode "hideshow" "Fold code with GNU Emacs." t)

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

;; (add-hook 'lisp-mode-hook       'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'shell-script-mode    'hs-minor-mode)
;; (add-hook 'haskell-mode-hook    'hs-minor-mode)
(add-hook 'latex-mode-hook 'hs-minor-mode)

;;; COMMENT: diminish
;; SOURCE: `http://www.emacswiki.org/emacs/DiminishedModes'
(require 'diminish) ;; NOTE: turn off the textual mode indicator in the mode line

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
;; (eval-after-load "glasses" '(diminish 'glasses-mode ""))
(eval-after-load "face-remap" '(diminish 'buffer-face-mode ""))
(eval-after-load "abbrev" '(diminish 'abbrev-mode ""))
(eval-after-load "hilit-chg" '(diminish 'highlight-changes-mode ""))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode ""))
(eval-after-load "longlines" '(diminish 'longlines-mode ""))

;; TODO: consider removing the `mode-line' 
;;(setq mode-line-format nil) ;; NOTE: removes the mode-line

;; NOTE: original mode-line-format
;; (setq mode-line-format '("%e"
;; 			 (:eval
;; 			  (if
;; 			      (display-graphic-p)
;; 			      #(" " 0 1
;; 				(help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
;; 			    #("-" 0 1
;; 			      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
;; 			 mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
;; 			 #("   " 0 3
;; 			   (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
;; 			 mode-line-position
;; 			 (vc-mode vc-mode)
;; 			 #("  " 0 2
;; 			   (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
;; 			 mode-line-modes
;; 			 (which-func-mode
;; 			  ("" which-func-format
;; 			   #(" " 0 1
;; 			     (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
;; 			 (global-mode-string
;; 			  ("" global-mode-string
;; 			   #(" " 0 1
;; 			     (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
;; 			 (:eval
;; 			  (unless
;; 			      (display-graphic-p)
;; 			    #("-%-" 0 3
;; 			      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))))

(provide 'appearance-config)

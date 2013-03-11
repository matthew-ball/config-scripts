;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/appearance-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: appearance
;; SOURCE: `http://emacswiki.org/emacs/ColorTheme'
;; SOURCE: `http://color-theme-select.heroku.com/'
;; SOURCE: `http://www.emacswiki.org/emacs/PowerLine'
(autoload 'color-theme-initialize "color-theme" "Colour theme for GNU Emacs." t)
;;(autoload 'powerline-default-theme "powerline" "" t) ;; WARNING: `powerline' doesn't respect ERC colours

(eval-after-load "color-theme"
  (progn
    (set-face-attribute 'default nil :height 80) ;; NOTE: reduce font-size slightly
    (setq color-theme-is-global t ;; ...
          frame-title-format "%b" ;; NOTE: set frame title properties
          icon-title-format "%b")))

;; COMMENT: X server specific (apply `powerline' mode-line extension)
(when (display-graphic-p)
  ;; (color-theme-initialize)
  ;; (powerline-default-theme)
  )

;; COMMENT: apply colour theme to a GNU Emacs frame (i.e. `emacsclient')
;; SOURCE: `http://www.emacswiki.org/emacs/ColorThemeQuestions'
(defun decorate-frame (frame)
  "Decorate new frame FRAME with `zenburn' colour theme."
  (select-frame frame)
  (when (display-graphic-p)
    ;; (color-theme-initialize)
    ;; (color-theme-scintilla)
    ;; (color-theme-zenburn)
    ;; (load-theme 'whiteboard)
    ;; (powerline-default-theme)
    ))

(add-hook 'after-make-frame-functions 'decorate-frame)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; NOTE: hide the menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; NOTE: hide the tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; NOTE: hide the scroll bar
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1)) ;; NOTE: turn off blinking cursor
(when (fboundp 'tooltip-mode) (tooltip-mode -1)) ;; NOTE: turn off tooltip
;;(when (fboundp 'fringe-mode) (fringe-mode -1)) ;; NOTE: turn off the fringe
(when (fboundp 'fringe-mode) (set-fringe-mode '(1 . 0))) ;; NOTE: set fringe to 1px on left side only

;;; COMMENT: window configuration
;; SOURCE: `http://www.emacswiki.org/emacs/WinnerMode'
(require 'winner)

(winner-mode t)

;;; COMMENT: workgroups for windows
;; SOURCE: `http://www.emacswiki.org/emacs/WorkgroupsForWindows'
;; TODO: learn workgroups
;; (autoload 'workgroups-mode "workgroups" "Workgroups for windows." t)

;; (eval-after-load "workgroups" '(workgroups-mode 1))

;; (setq wg-prefix-key (kbd "C-c w"))

;;; COMMENT: visual lines
;; SOURCE: `http://www.emacswiki.org/emacs/VisualLineMode'
(global-visual-line-mode t) ;; NOTE: enable visual line mode for all buffers (i.e. globally)
;; TODO: this needs to be activated only for selected modes
;; EXAMPLE: (add-to-hook 'text-mode-hook '(turn-on-visual-line-mode))

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

;;; COMMENT: line numbers
;; SOURCE: `http://www.emacswiki.org/emacs/line-num.el'
;; (autoload 'linum-mode "linum" "Display line numbers." t)

;; WARNING: this does not like `doc-view-mode'
;; (add-hook 'find-file-hook (lambda () (linum-mode 1))) ;; NOTE: turn on linum-mode if in a file

;;; COMMENT: ruler mode
;; SOURCE: `http://www.emacswiki.org/emacs/RulerMode'
;; (autoload 'rule-mode "ruler-mode" "Display a ruler which measures columns." t)

;; (add-hook 'find-file-hook (lambda () (ruler-mode 1))) ;; NOTE: turn on ruler-mode if in a file

;;; COMMENT: mode-line directory tracking
;; SOURCE: `http://www.emacswiki.org/emacs/ModeLineDirtrack'
;; (defun add-mode-line-dirtrack ()
;;   "..."
;;   (add-to-list
;;    'mode-line-buffer-identification
;;    '(:propertize (" " default-directory " ") face dired-directory)))

;; NOTE: add the above function to the appropriate mode-hook to enable it for that mode
;;       i imagine something like (setq-default global-dirtrack-mode t) would allow it to be turned on for all modes
;; (add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;;; COMMENT: indicate empty lines
;; (toggle-indicate-empty-lines)

;;; COMMENT: show matching parenthesis
;; SOURCE: `http://emacswiki.org/emacs/ShowParenMode'
(show-paren-mode t)
(setq show-paren-style 'parenthesis
      show-paren-delay 0.0)

;;; COMMENT: mode line
;;(setq mode-line-format nil) ;; NOTE: removes the mode-line

;; SOURCE: `http://www.emacswiki.org/emacs/ModeLineConfiguration'
(setq line-number-mode t ;; NOTE: turn on line numbers in the mode line
      column-number-mode t ;; NOTE: turn on column numbers in the mode line
      size-indication-mode nil) ;; NOTE: do not show file size in mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayTime'
;; (display-time-mode t) ;; NOTE: display time status in the mode line

;; SOURCE: `http://www.emacswiki.org/emacs/DisplayBatteryMode'
;;(require 'battery

;; (display-battery-mode t) ;; NOTE: display battery status in the mode line

;; (setq battery-echo-area-format "Power %L, battery %B (%p%% load, remaining time %t)"
;;       battery-load-critical 10
;;       battery-load-low 25
;;       battery-mode-line-format "[%b%p%%]" ;; NOTE: default
;;       battery-update-interval 60
;;       ;;battery-status-function 'battery-linux-sysfs ;; NOTE: default
;;       ;;battery-status-function 'battery-linux-proc-acpi
;;       ;;battery-status-function 'battery-linux-proc-apm
;;       battery-mode-line-limit 100 ;; NOTE: display battery status when battery has less than 50% charge
;;       )

;; SOURCE: `http://gitorious.org/wireless'
;; (require 'wireless)
;; (display-wireless-mode t) ;; NOTE: display wireless connection strength in mode line

;; SOURCE: `http://www.emacswiki.org/emacs/WhichFunctionMode'
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

;;; COMMENT: text folding
;; SOURCE: `http://emacswiki.org/emacs/HideShow'
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
(eval-after-load "hideshow" '(add-hook 'latex-mode-hook 'hs-minor-mode))

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
		 (format "... / %d"
			 (count-lines (overlay-start ov)
				      (overlay-end ov))))))

(setq hs-set-up-overlay 'display-code-line-counts)

;;; COMMENT: diminish
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
(eval-after-load "workgroups" '(diminish 'workgroups-mode ""))

;;; COMMENT: adaptive text wrap
(require 'adaptive-wrap)

(add-hook 'org-mode-hook '(lambda () adaptive-wrap-prefix-mode t))

(provide 'appearance-config)

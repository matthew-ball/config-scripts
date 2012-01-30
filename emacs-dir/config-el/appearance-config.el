;; =========================================
;; ~/.emacs.d/config-el/appearance-config.el
;; Matthew Ball (copyleft 2012)
;; =========================================

;;; appearance
(require 'color-theme)
;; (require 'zenburn)

;; (autoload 'color-theme "color-theme" "Colour theme for GNU Emacs (GTK)." t)
(autoload 'zenburn "zenburn" "Zenburn colour theme for GNU Emacs." t)

(when window-system 'x ;; if using x windowing system
  (set-face-attribute 'default nil :height 90) ;; change font size
  (eval-after-load "color-theme" '(zenburn)) ;; apply zenburn colour theme
  (setq frame-title-format "%b"
  	icon-title-format "%b"))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; hide the menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; hide the tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; hide the scroll bar
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1)) ;; turn off blinking cursor
(when (fboundp 'tooltip-mode) (tooltip-mode -1)) ;; turn off tooltip
;;(when (fboundp 'fringe-mode) (fringe-mode -1)) ;; turn off the fringe
(when (fboundp 'fringe-mode) (set-fringe-mode '(1 . 0))) ;; set fringe to 1px on left side only

;;; visual lines
(global-visual-line-mode t) ;; enable visual line mode for all buffers (i.e. globally)

;;; indicate empty lines
;; (toggle-indicate-empty-lines)

;;; show matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis
      show-paren-delay 0.0)

;;; mode line
(setq line-number-mode 1 ;; turn on line numbers in the mode line
      column-number-mode 1 ;; turn on column numbers in the mode line
      size-indication-mode t) ;; show file size in mode line

(display-time-mode t) ;; display time status in the mode line
(display-battery-mode t) ;; display battery status in the mode line
;; (which-function-mode t) ;; show the current function in the mode line

;;; code folding
;; (hs-minor-mode t)
(autoload 'hs-toggle-hiding "hideshow" "Toggle code folding minor mode." t)

(global-set-key (kbd "C-+") 'hs-toggle-hiding)
;; (global-set-key (kbd "C-\\") 'toggle-selective-display)

(setq hs-hide-comments nil) ;; hide the comments too when you do a 'hs-hide-all'
(setq hs-isearch-open 'x) ;; set whether isearch opens folded comments, code, or both where x is code, comments, t (both), or nil (neither)

(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'shell-script-mode    'hs-minor-mode)
(add-hook 'haskell-mode-hook    'hs-minor-mode)
(add-hook 'latex-mode-hook      'hs-minor-mode)

;;; diminish
(require 'diminish) ;; turn off the textual mode indicator in the mode line

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

(provide 'appearance-config)

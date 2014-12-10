;;; programming-config.el --- Configuration for programming-related settings

;; Copyright (C) 2008-2014  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: configuration

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

;; Configuration for programming-related settings.

;;; Code:

(defgroup user-programming nil "Custom programming variables." :group 'user-variables)

;;; IMPORTANT: general programming
(defun general-programming (&rest junk)
  "General function for programming modes.

Enable the following minor modes:
1. `hs-minor-mode' - Fold comment blocks.
2. `electric-pair-mode' - Automatic pairing of parenthesis.
3. `flyspell-prog-mode' - Spell checking for programming modes."
  ;; (flymake-mode) ;; NOTE: turn on flymake mode
  ;; (glasses-mode) ;; NOTE: turn on glasses mode
  ;; (longlines-mode) ;; NOTE: enable long lines
  ;; (hl-line-mode) ;; NOTE: turn on line highlight mode
  ;; (which-function-mode t) ;; NOTE: keep track of active function
  ;; (auto-insert-mode)
  (modify-syntax-entry ?- "w") ;; NOTE: treat '-' as part of the word
  (flyspell-prog-mode) ;; NOTE: turn on spell checking of comments and strings
  (hs-minor-mode) ;; NOTE: turn on hide/show mode
  (electric-pair-mode))

(add-hook 'prog-mode-hook #'general-programming)

;;; IMPORTANT: stumpwm mode
;; SOURCE: `http://www.emacswiki.org/emacs/StumpWM'
;; (autoload 'stumpwm-mode "stumpwm-mode" "Major mode for editing StumpWM." t)

;;; IMPORTANT: emacs lisp programming
;; SOURCE: `http://www.emacwswiki.org/emacs/EmacsLisp'
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsLispIntro'
(autoload 'eldoc-mode "eldoc" "GNU Emacs lisp documentation minor mode." t)

(after "eldoc"
  ;; NOTE: make `eldoc' recognise `paredit' functions
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(after "lisp-mode"
  (defun custom-emacs-lisp-key-bindings ()
    (define-key emacs-lisp-mode-map (kbd "C-c f") 'forward-sexp)
    (define-key emacs-lisp-mode-map (kbd "C-c b") 'backward-sexp))
  
  (defun custom-emacs-lisp ()
    "Custom `emacs-lisp-mode' functionality."
    (turn-on-eldoc-mode)
    (paredit-mode t)
    (custom-emacs-lisp-key-bindings))

(add-hook 'emacs-lisp-mode-hook #'custom-emacs-lisp))

;;; IMPORTANT: interactive emacs lisp
(after "ielm"
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode))

;;; IMPORTANT: emacs byte-compiled code
;; SOURCE: `http://www.emacswiki.org/emacs/CompiledFile'
;;(byte-recompile-directory (expand-file-name user-emacs-directory) 0)

;;; IMPORTANT: common lisp programming
;; SOURCE: `http://emacswiki.org/emacs/CommonLisp'
(after "lisp-mode"
  ;; (defun scratch-lisp-file ()
  ;;   "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into the current buffer."
  ;;   (interactive)
  ;;   (goto-char 0)
  ;;   (let* ((file (file-name-nondirectory (buffer-file-name)))
  ;; 	   (package (file-name-sans-extension file)))
  ;;     (insert ";;; " file "\n")
  ;;     (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
  ;;     (insert "(in-package #:" package ")\n\n")))

  (defun custom-lisp ()
    ;; (define-key lisp-mode-map (kbd "C-c n") #'scratch-lisp-file)
    (paredit-mode t)
    (slime-mode t))

  (defun custom-lisp-interaction ()
    (turn-on-eldoc-mode)
    (paredit-mode t))

  (defun custom-inferior-lisp ()
    (paredit-mode t))

  (add-hook 'lisp-mode-hook #'custom-lisp)
  (add-hook 'lisp-interaction-mode-hook #'custom-lisp-interaction)
  (add-hook 'inferior-lisp-mode-hook #'custom-inferior-lisp))

;;; IMPORTANT: paredit
;; TODO: `user-config.el'
;; SOURCE: `http://emacswiki.org/emacs/ParEdit'
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

(after "paredit"
  (diminish-minor-mode "paredit"))

;;; IMPORTANT: elisp slime navigation
;; SOURCE: `https://github.com/purcell/elisp-slime-nav'
;; TODO: `user-config.el'
(autoload 'elisp-slime-nav-mode "elisp-slime-nav" "..." t)

(after "elisp-slime-nav"
  (diminish-minor-mode "elisp-slime-nav")
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

;;; IMPORTANT: slime/swank
;; TODO: `user-config.el'
(require 'slime-autoloads)

(after "slime"
  ;; NOTE: suppress the printing of any banner or other informational message at startup
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform --userinit=\"$HOME/.sbclrc\""
	slime-repl-history-file (expand-file-name (concat user-emacs-directory "slime-history.el"))
	slime-startup-animation nil
	slime-net-coding-system 'utf-8-unix
        slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  (slime-setup '(slime-fancy
                 slime-tramp
                 slime-banner
                 slime-compiler-notes-tree
                 slime-indentation
                 slime-fontifying-fu
                 slime-scratch
                 slime-xref-browser
                 slime-asdf
                 slime-repl
                 slime-editing-commands
                 slime-fuzzy
                 slime-autodoc
                 slime-sbcl-exts))

  (defun custom-slime ()
    (paredit-mode t))

  (add-hook 'slime-repl-mode-hook #'custom-slime t))

;;; IMPORTANT: clojure programming
(autoload 'clojure-mode "clojure-mode" "Major mode for editing clojure source code files." t)
(autoload 'nrepl-mode "nrepl" "Major mode for nREPL interactions." t)

(after "nrepl"
  (add-hook 'nrepl-interaction-mode-hook #'nrepl-turn-on-eldoc-mode)

  (setq nrepl-hide-special-buffers t)

  (add-hook 'nrepl-mode-hook #'paredit-mode))

(after "clojure-mode"
  (defun custom-clojure ()
    ""
    (paredit-mode t))

  (add-hook 'clojure-mode-hook #'custom-clojure))

;;; IMPORTANT: scheme (guile) programming
;; SOURCE: `http://emacswiki.org/emacs/Scheme'
(autoload 'scheme-mode "scheme" "Major mode for editing scheme source code files." t)
(require 'geiser) ;; TODO: find a `guile-mode' for scheme ...

(after "geiser"
  (diminish-minor-mode "geiser-mode" 'geiser-mode)
  (diminish-minor-mode "geiser-autodoc")
  (setq geiser-active-implementations '(guile)))

(after "scheme"
  (defun custom-scheme ()
    ""
    (paredit-mode t))
  
  (add-hook 'scheme-mode-hook #'custom-scheme))

;;; IMPORTANT: haskell programming
;; SOURCE: `http://www.emacswiki.org/emacs/Haskell'
(autoload 'haskell-mode "haskell-mode" "Major mode for editing haskell source code." t)

(after "haskell-mode"
  (defun custom-haskell ()
    (turn-on-haskell-doc-mode) ;; NOTE: enable haskell's documentation mode
    (turn-on-haskell-indentation))  ;; NOTE: enable haskell's indentation mode
  
  (diminish-minor-mode "haskell-doc")
  (diminish-minor-mode "haskell-indent")
  (diminish-minor-mode "haskell-indentation")
  (setq haskell-font-lock-symbols t) ;; NOTE: enable unicode symbols for haskell

  (add-hook 'haskell-mode-hook #'custom-haskell))

;;; IMPORTANT: shell script
;; SOURCE: `http://emacswiki.org/emacs/ShMode'
(autoload 'shell-script-mode "sh-mode" "Major mode for editing shell script source code." t)

;;; IMPORTANT: python programming
;; SOURCE: `http://emacswiki.org/emacs/PythonProgrammingInEmacs'
(autoload 'python-mode "python" "Major mode for editing python source code." t)

;;; IMPORTANT: javascript programming
;; SOURCE: `http://www.emacswiki.org/emacs/JavaScriptMode'
(autoload 'js-mode "js" "Major mode for editing javascript source code." t)

;;; IMPORTANT: C programming
;; SOURCE: `http://www.emacswiki.org/emacs/CcMode'
(autoload 'c-mode "cc-mode" "Major mode for editing C source code." t)

(after "cc-mode"
  (require 'cwarn)
  ;; TODO: set up CEDET
  ;; (require 'cedet) ;; NOTE: collection of emacs development environment tools

  (defun c-mode-settings ()
    (setq c-default-style "linux"
	  c-basic-offset 4))

  (defun custom-c ()
    ""
    (turn-on-cwarn-mode)
    (c-mode-settings))

  (add-hook 'c-mode-hook #'custom-c))

;;; IMPORTANT: gdb
(autoload 'gdb "gdb-mi" "Front-end to the GNU Debugger." t)

(after "gdb"
  (setq ;;gdb-many-windows t
   gdb-show-main t))

;;; IMPORTANT: prolog
(autoload 'run-prolog "prolog" "Prolog in GNU Emacs." t)

;;; IMPORTANT: maxima
;; SOURCE: `http://emacswiki.org/emacs/MaximaMode'
(autoload 'maxima-mode "maxima" "Major mode for interaction with maxima." t)
(autoload 'maxima "maxima" "Major mode for maxima interaction." t)
(autoload 'imaxima "imaxima" "Major mode frontend for maxima with image support." t)
(autoload 'imath-mode "imath" "Imath mode for math formula input." t)

(after "imaxima"
  (setq imaxima-use-maxima-mode-flag t))

;; TODO: `ruby-mode'

(provide 'programming-config)
;;; programming-config.el ends here

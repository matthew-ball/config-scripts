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

;; IMPORTANT: ...
;; SOURCE: `http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/'
;; (defun smarter-move-beginning-of-line (arg)
;;   "Move point back to indentation of beginning of line.

;; Move point to the first non-whitespace character on this line.
;; If point is already there, move to the beginning of the line.
;; Effectively toggle between the first non-whitespace character and
;; the beginning of the line.

;; If ARG is not nil or 1, move forward ARG - 1 lines first.  If
;; point reaches the beginning or end of the buffer, stop there."
;;   (interactive "^p")
;;   (setq arg (or arg 1))

;;   ;; Move lines first
;;   (when (/= arg 1)
;;     (let ((line-move-visual nil))
;;       (forward-line (1- arg))))

;;   (let ((orig-point (point)))
;;     (back-to-indentation)
;;     (when (= orig-point (point))
;;       (move-beginning-of-line 1))))

;; ;; remap C-a to `smarter-move-beginning-of-line'
;; (global-set-key [remap move-beginning-of-line]
;;                 'smarter-move-beginning-of-line)

;;; IMPORTANT: stumpwm mode
;; SOURCE: `http://www.emacswiki.org/emacs/StumpWM'
(autoload 'stumpwm-mode "stumpwm-mode" "Major mode for editing StumpWM." t) ;; NOTE: not ideal

;;; IMPORTANT: general programming
;; TODO: make this a `general-programming-mode-hook'
;; TODO: this should possibly just be activated in `prog-mode-hook'
(defun turn-on-general-programming-mode (&rest junk)
  "General function for programming modes.

Enable the following minor modes:
1. `hs-minor-mode' - Fold comment blocks.
2. `electric-pair-mode' - Automatic pairing of parenthesis.
3. `yas-minor-mode' - Expand snippets of code.
4. `auto-insert-mode' - Insert a template into new files."
  (modify-syntax-entry ?- "w") ;; NOTE: treat '-' as part of the word
  ;; (flymake-mode) ;; NOTE: turn on flymake mode
  (flyspell-prog-mode) ;; NOTE: turn on spell checking of comments and strings
  ;; (glasses-mode) ;; NOTE: turn on glasses mode
  ;; (longlines-mode) ;; NOTE: enable long lines
  ;;(hl-line-mode) ;; NOTE: turn on line highlight mode
  ;; (which-function-mode t) ;; NOTE: keep track of active function
  (hs-minor-mode) ;; NOTE: turn on hide/show mode
  (electric-pair-mode)
  ;;(auto-insert-mode)
  )

;; TODO: should just:
(add-hook 'prog-mode-hook #'turn-on-general-programming-mode)

;;; IMPORTANT: available modes for the which function mode-line tag
;; SOURCE: `http://www.emacswiki.org/emacs/WhichFuncMode'
;; SOURCE: `http://emacs-fu.blogspot.com.au/2009/01/which-function-is-this.html'
;; (autoload 'which-func-mode "which-func" "Display the current function name in the mode-line." t)

;; (eval-after-load "which-func"
;;   (add-to-list 'which-func-modes 'emacs-lisp-mode)
;;   (add-to-list 'which-func-modes 'lisp-mode))

;;; IMPORTANT: paredit
;; SOURCE: `http://emacswiki.org/emacs/ParEdit'
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;; IMPORTANT: emacs lisp programming
;; SOURCE: `http://www.emacwswiki.org/emacs/EmacsLisp'
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsLispIntro'
(autoload 'eldoc-mode "eldoc" "GNU Emacs lisp documentation minor mode." t)

(after "eldoc"
  ;; NOTE: make `eldoc' recognise `paredit' functions
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(after "lisp-mode"
  (defun custom-emacs-lisp-mode ()
    ""
    ;; (turn-on-general-programming-mode)
    (turn-on-eldoc-mode)
    (paredit-mode t)
    ;; NOTE: some key-bindings
    (define-key emacs-lisp-mode-map (kbd "C-c f") 'forward-sexp)
    (define-key emacs-lisp-mode-map (kbd "C-c b") 'backward-sexp))

(add-hook 'emacs-lisp-mode-hook #'custom-emacs-lisp-mode))

;;; IMPORTANT: interactive emacs lisp
(after "ielm"
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode))

;;; IMPORTANT: emacs byte-compiled code
;; SOURCE: `http://www.emacswiki.org/emacs/CompiledFile'
;;(byte-recompile-directory (expand-file-name user-emacs-directory) 0)

;;; IMPORTANT: common lisp programming
;; SOURCE: `http://emacswiki.org/emacs/CommonLisp'
(after "lisp-mode"
  (defun scratch-lisp-file ()
    "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into the current buffer."
    (interactive)
    (goto-char 0)
    (let* ((file (file-name-nondirectory (buffer-file-name)))
	   (package (file-name-sans-extension file)))
      (insert ";;;; " file "\n")
      (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
      (insert "(in-package #:" package ")\n\n")))

  (add-hook 'lisp-mode-hook #'(lambda ()
                               (turn-on-general-programming-mode)
                               (paredit-mode t)
			       ;; TODO: check for slime first?
			       (slime-mode t)
                               ))

  (add-hook 'lisp-interaction-mode-hook #'(lambda ()
                                           (turn-on-general-programming-mode)
                                           (turn-on-eldoc-mode)
                                           (paredit-mode t)))

  (add-hook 'inferior-lisp-mode-hook #'(lambda ()
                                        ;;(inferior-slime-mode t)
                                        (paredit-mode t))))

;; SOURCE: `http://www.xach.com/lisp/scratch-lisp-file.el'
;; (defun scratch-lisp-file ()
;;   "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
;;   the current buffer."
;;   (interactive)
;;   (goto-char 0)
;;   (let* ((file (file-name-nondirectory (buffer-file-name)))
;;          (package (file-name-sans-extension file)))
;;     (insert ";;;; " file "\n")
;;     (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
;;     (insert "(in-package #:" package ")\n\n")))

;;; IMPORTANT: elisp slime navigation
;; SOURCE: `https://github.com/purcell/elisp-slime-nav'
;; TODO: move to user-config.el
;;(autoload "elisp-slime-nav-mode" "elisp-slime-nav" t)
(require 'elisp-slime-nav)

(after "elisp-slime-nav"
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

;;; IMPORTANT: slime/swank
(add-to-list 'load-path (expand-file-name "~/Public/slime"))

(require 'slime-autoloads)
;; (when (member slime-autoloads features)
;;   (require 'slime-autoloads))

(after "slime"

  ;;IMPORTANT: ac-slime
  ;; (add-hook 'slime-mode-hook #'set-up-slime-ac)
  ;; (add-hook 'slime-repl-mode-hook #'set-up-slime-ac)

  ;; (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform --userinit=\"$HOME/.sbclrc\"") ;; NOTE: suppress the printing of any banner or other informational message at startup

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
                 slime-sbcl-exts
                 ;; ---
                 ;; slime-mdot-fu
                 ;; slime-package-fu
                 ;; slime-fancy-inspector
                 ;; slime-indentation-fu
                 ;; slime-references
                 ;; ---
		 ;; inferior-slime-mode
                 ))

  (setq slime-startup-animation nil
	slime-net-coding-system 'utf-8-unix
        slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  ;; (add-hook 'slime-mode-hook '(lambda ()
  ;;                               ;; (start-slime-automatically)
  ;;                               ;; (turn-on-general-programming-mode)
  ;;                               ;; (paredit-mode t)
  ;;                               ))

  (add-hook 'slime-repl-mode-hook #'paredit-mode t)

  ;;(define-key slime-repl-mode-map (kbd "<return>") #'slime-repl-return)

  ;; NOTE: stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit (&rest junk)
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))

  (add-hook 'slime-repl-mode-hook #'override-slime-repl-bindings-with-paredit))

;;; IMPORTANT: clojure programming
(autoload 'clojure-mode "clojure-mode" "Major mode for editing clojure source code files." t)
(autoload 'nrepl-mode "nrepl" "Major mode for nREPL interactions." t)

(after "nrepl"
  (add-hook 'nrepl-interaction-mode-hook #'nrepl-turn-on-eldoc-mode)

  (setq nrepl-hide-special-buffers t)

  (add-hook 'nrepl-mode-hook #'paredit-mode))

(after "clojure-mode"
  (defun custom-clojure-mode ()
    ""
    ;; (turn-on-general-programming-mode)
    (paredit-mode t))

  (add-hook 'clojure-mode-hook #'custom-clojure-mode))

;;; IMPORTANT: scheme (guile) programming
;; SOURCE: `http://emacswiki.org/emacs/Scheme'
(autoload 'scheme-mode "scheme" "Major mode for editing scheme source code files." t)
(require 'geiser) ;; TODO: find a `guile-mode' for scheme ...

(after "geiser"
  (setq geiser-active-implementations '(guile)))

(defun custom-scheme-mode ()
  ""
  ;; (turn-on-general-programming-mode)
  (paredit-mode t))

(after "scheme"
  (add-hook 'scheme-mode-hook #'custom-scheme-mode))

;;; IMPORTANT: haskell programming
;; SOURCE: `http://www.emacswiki.org/emacs/Haskell'
(autoload 'haskell-mode "haskell-mode" "Major mode for editing haskell source code." t)

(defun custom-turn-on-haskell-modes ()
  ;; (turn-on-general-programming-mode)
  (turn-on-haskell-doc-mode) ;; NOTE: enable haskell's documentation mode
  (turn-on-haskell-indentation))  ;; NOTE: enable haskell's indentation mode

(after "haskell-mode"
  (setq haskell-font-lock-symbols t) ;; NOTE: enable unicode symbols for haskell

  (add-hook 'haskell-mode-hook #'custom-turn-on-haskell-modes))

;;; IMPORTANT: shell script
;; SOURCE: `http://emacswiki.org/emacs/ShMode'
(autoload 'shell-script-mode "sh-mode" "Major mode for editing shell script source code." t)

;; (after "sh-mode"
;;   ;;(add-hook 'shell-script-mode #'turn-on-general-programming-mode)
;;   )

;;; IMPORTANT: python programming
;; SOURCE: `http://emacswiki.org/emacs/PythonProgrammingInEmacs'
(autoload 'python-mode "python" "Major mode for editing python source code." t)

;; (after "python"
;;   (add-hook 'python-mode-hook #'turn-on-general-programming-mode))

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

  (defun custom-c-mode ()
    ""
    ;; (turn-on-general-programming-mode)
    (turn-on-cwarn-mode)
    (c-mode-settings))

  (add-hook 'c-mode-hook #'custom-c-mode))

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

;;; IMPORTANT: speedbar
;; SOURCE: `'
;; (autoload 'speedbar "speedbar" "" t)

;; (after "speedbar"
;;   ;; (setq speedbar-mode-hook '(lambda () (interactive) (other-frame 0)))
;;   (speedbar-add-supported-extension ".hs")

;;   ;; (add-hook 'speedbar-mode-hook '(lambda () (set-face-attribute 'default nil :height 90)))
;;   )

;; (require 'sr-speedbar)

;; (after "sr-speedbar"
;;   (setq sr-speedbar-right-side nil))

(provide 'programming-config)
;;; programming-config.el ends here

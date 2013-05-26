;;; programming-config.el --- Configuration for programming-related settings

;; Copyright (C) 2013  Matthew Ball

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

;;; IMPORTANT: general programming
;; TODO: make this a `general-programming-mode-hook'
;; TODO: this should possibly just be activated in `prog-mode-hook'
(defun turn-on-general-programming-mode (&rest junk)
  "General function for programming modes."
  (modify-syntax-entry ?- "w") ;; NOTE: treat '-' as part of the word
  ;; (flymake-mode) ;; NOTE: turn on flymake mode
  ;; (flyspell-prog-mode) ;; NOTE: turn on spell checking of comments and strings (TODO: not sure about this function)
  ;; (glasses-mode) ;; NOTE: turn on glasses mode
  ;; (longlines-mode) ;; NOTE: enable long lines
  (hl-line-mode) ;; NOTE: turn on line highlight mode
  ;;(which-function-mode t) ;; NOTE: keep track of active function
  (hs-minor-mode) ;; NOTE: turn on hide/show mode
  )

;;; IMPORTANT: available modes for the which function mode-line tag
;; SOURCE: `http://www.emacswiki.org/emacs/WhichFuncMode'
;; SOURCE: `http://emacs-fu.blogspot.com.au/2009/01/which-function-is-this.html'
;; (autoload 'which-func-mode "which-func" "Display the current function name in the mode-line." t)

;; (eval-after-load "which-func"
;;   '(progn
;;   '(add-to-list 'which-func-modes 'emacs-lisp-mode)
;;   '(add-to-list 'which-func-modes 'lisp-mode)))

;;; IMPORTANT: paredit
;; SOURCE: `http://emacswiki.org/emacs/ParEdit'
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;; IMPORTANT: emacs lisp programming
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsLisp'
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsLispIntro'
;;(require 'eldoc)
(autoload 'eldoc-mode "eldoc" "GNU Emacs lisp documentation minor mode." t)

(eval-after-load "eldoc"
  '(eldoc-add-command 'paredit-backward-delete 'paredit-close-round) ;; NOTE: make `eldoc' recognise `paredit' functions
  )

(add-hook 'emacs-lisp-mode-hook '(lambda () ;; NOTE: active general programming mode
				   (turn-on-general-programming-mode)
                                   (turn-on-eldoc-mode)
                                   ;; NOTE: some key-bindings
                                   (define-key emacs-lisp-mode-map (kbd "C-c f") 'forward-sexp)
                                   (define-key emacs-lisp-mode-map (kbd "C-c b") 'backward-sexp)
				   (paredit-mode t)))

;;; IMPORTANT: interactive emacs lisp
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; IMPORTANT: emacs byte-compiled code
;; SOURCE: `http://www.emacswiki.org/emacs/CompiledFile'
;;(byte-recompile-directory (expand-file-name user-emacs-directory) 0)

;;; IMPORTANT: common lisp programming
;; SOURCE: `http://emacswiki.org/emacs/CommonLisp'
;;(setq inferior-lisp-program "/usr/bin/sbcl")
(setq inferior-lisp-program "/usr/bin/sbcl --noinform") ;; NOTE: suppress the printing of any banner or other informational message at startup

(add-hook 'lisp-mode-hook '(lambda ()
			     (turn-on-general-programming-mode)
			     (paredit-mode t)))

(add-hook 'lisp-interaction-mode-hook '(lambda ()
					 (turn-on-general-programming-mode)
					 (turn-on-eldoc-mode)
					 (paredit-mode t)))

(add-hook 'inferior-lisp-mode-hook '(lambda ()
				      ;; (inferior-slime-mode t)
				      (paredit-mode t)))
;;; IMPORTANT: slime/swank
;; IMPORTANT: requires `quicklisp' and (ql:quickload "quicklisp-slime-helper")
(defvar quicklisp-directory "~/quicklisp/dists/quicklisp/software/" "The directory path to `quicklisp'.")

(add-to-list 'load-path (expand-file-name (concat quicklisp-directory "slime-20120407-cvs"))) ;; TODO: this is not ideal

(require 'slime-autoloads)

(slime-setup '(slime-fancy
	       slime-tramp
	       slime-banner
	       slime-compiler-notes-tree
	       slime-indentation
	       slime-fontifying-fu
	       slime-mdot-fu
	       slime-scratch
	       slime-xref-browser
	       slime-asdf
	       ;; slime-package-fu
	       ;; slime-repl
	       ;; slime-editing-commands
	       ;; slime-fancy-inspector
	       ;; slime-fuzzy
	       ;; slime-autodoc
	       ;; slime-indentation-fu
	       ;; slime-references
	       slime-sbcl-exts))

(setq slime-net-coding-system 'utf-8-unix
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;; (defun start-slime-automatically () ;; NOTE: automatically start slime when opening a lisp file
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))

(add-hook 'slime-mode-hook '(lambda ()
			      ;; (start-slime-automatically)
			      (turn-on-general-programming-mode)
			      (paredit-mode t)
			      ))

(add-hook 'slime-repl-mode-hook '(lambda () (paredit-mode t)))

;;(define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-return)

;; NOTE: stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit (&rest junk)
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;; IMPORTANT: clojure programming
(autoload 'clojure-mode "clojure-mode" "Major mode for editing clojure source code files." t)
(autoload 'nrepl-mode "nrepl" "Major mode for nREPL interactions." t)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)

(add-hook 'nrepl-mode-hook 'paredit-mode)

(add-hook 'clojure-mode-hook '(lambda ()
                                (turn-on-general-programming-mode)
                                (paredit-mode t)))

;;; IMPORTANT: scheme programming
;; SOURCE: `http://emacswiki.org/emacs/Scheme'
;; (autoload 'scheme-mode "scheme" "Major mode for editing scheme source code files." t);; TODO: find a `guile-mode' for scheme ...

;; (add-hook 'scheme-mode-hook '(lambda ()
;; 			       (turn-on-general-programming-mode)
;; 			       (paredit-mode t)))

;;; IMPORTANT: haskell programming
;; SOURCE: `http://www.emacswiki.org/emacs/Haskell'
(autoload 'haskell-mode "haskell-site-file" "Major mode for editing haskell source code." t)

(setq haskell-font-lock-symbols t) ;; NOTE: enable unicode symbols for haskell

(defun custom-turn-on-haskell-modes (&rest junk)
  (turn-on-haskell-doc-mode) ;; NOTE: enable haskell's documentation mode
  (turn-on-haskell-indent)) ;; NOTE: enable haskell's indentation mode

(add-hook 'haskell-mode-hook '(lambda ()
				(turn-on-general-programming-mode)
				(custom-turn-on-haskell-modes)))

;;; IMPORTANT: shell script
;; SOURCE: `http://emacswiki.org/emacs/ShMode'
(autoload 'shell-script-mode "sh-mode" "Major mode for editing shell script source code." t)

(add-hook 'shell-script-mode '(lambda ()
				(turn-on-general-programming-mode)))

;;; IMPORTANT: python programming
;; SOURCE: `http://emacswiki.org/emacs/PythonProgrammingInEmacs'
(autoload 'python-mode "python" "Major mode for editing python source code." t)

(add-hook 'python-mode-hook '(lambda ()
			       (turn-on-general-programming-mode)))

;;; IMPORTANT: javascript programming
;; SOURCE: `http://www.emacswiki.org/emacs/JavaScriptMode'
(autoload 'js-mode "js" "Major mode for editing javascript source code." t)

;;; IMPORTANT: C programming
;; SOURCE: `http://www.emacswiki.org/emacs/CcMode'
(autoload 'c-mode "cc-mode" "Major mode for editing C source code." t)

(add-hook 'c-mode-hook '(lambda () (turn-on-general-programming-mode)))

;;; IMPORTANT: maxima
;; SOURCE: `http://emacswiki.org/emacs/MaximaMode'
(autoload 'maxima-mode "maxima" "Major mode for interaction with maxima." t)
(autoload 'maxima "maxima" "Major mode for maxima interaction." t)
(autoload 'imaxima "imaxima" "Major mode frontend for maxima with image support." t)
(autoload 'imath-mode "imath" "Imath mode for math formula input." t)

(setq imaxima-use-maxima-mode-flag t)

;;; IMPORTANT: flymake
;; SOURCE: `http://www.emacswiki.org/emacs/FlyMake'
;; (autoload 'flymake-mode "flymake" "On the fly compiling in GNU Emacs." t)
;; TODO: set `flymake-display-err-menu-for-current-line' to key-chord
;; TODO: set `flymake-goto-next-error' to key-chord
;; (setq flymake-log-level 3)

(provide 'programming-config)
;;; programming-config.el ends here

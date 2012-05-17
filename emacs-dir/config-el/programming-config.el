;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/programming-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:05:14 EST

;;; COMMENT: flymake
;; SOURCE: `http://www.emacswiki.org/emacs/FlyMake'
(autoload 'flymake-mode "flymake" "On the fly compiling in GNU Emacs." t)

;;; COMMENT: paredit
;; SOURCE: `http://emacswiki.org/emacs/ParEdit'
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;; COMMENT: general programming
;; TODO: make this a `programming-mode-hook'
(defun turn-on-general-programming-mode (&rest junk)
  "General function for programming modes."
  (modify-syntax-entry ?- "w") ;; NOTE: treat '-' as part of the word
  ;; (flymake-mode) ;; NOTE: turn on flymake mode
  (flyspell-prog-mode) ;; NOTE: turn on spell checking of comments and strings (TODO: not sure about this function)
  ;; (glasses-mode) ;; NOTE: turn on glasses mode
  (hs-minor-mode) ;; NOTE: turn on hide/show mode
  )

;;; COMMENT: emacs lisp programming
;; SOURCE: `http://emacswiki.org/emacs/EmacsLispIntro'
(autoload 'eldoc-mode "eldoc" "GNU Emacs lisp documentation minor mode." t)

(defun turn-on-byte-compile-file (&rest junk)
  "Automatically byte compile `*.el' files."
  (if (eq major-mode 'emacs-lisp-mode)
      (save-excursion (byte-compile-file buffer-file-name))))

(eldoc-add-command 'paredit-backward-delete 'paredit-close-round) ;; NOTE: make `eldoc' recognise `paredit' functions

(add-hook 'after-save-hook '(lambda ()
			     ;; (turn-on-byte-compile-file) ;; NOTE: automatically byte compile `*.el' files
			     ))

(add-hook 'emacs-lisp-mode-hook '(lambda () ;; NOTE: active general programming mode
				   (turn-on-general-programming-mode)
				   (turn-on-eldoc-mode)
				   (paredit-mode t)))

(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; COMMENT: common lisp programming
;; SOURCE: `http://emacswiki.org/emacs/CommonLisp'
(setq inferior-lisp-program "/usr/bin/sbcl")

(add-hook 'lisp-mode-hook '(lambda ()
			     (turn-on-general-programming-mode)
			     (paredit-mode t)))

(add-hook 'lisp-interaction-mode-hook '(lambda ()
					 (turn-on-general-programming-mode)
					 ;; (turn-on-eldoc-mode) ;; NOTE: ???
					 (paredit-mode t)))

(add-hook 'inferior-lisp-mode-hook '(lambda ()
				      ;; (inferior-slime-mode t)
				      (paredit-mode t)))
;;; COMMENT: slime/swank
;; IMPORTANT: requires `quicklisp' and (ql:quickload "quicklisp-slime-helper")
(add-to-list 'load-path "/home/chu/quicklisp/dists/quicklisp/software/slime-20120208-cvs") ;; TODO: this is not ideal

(require 'slime-autoloads)

(slime-setup '(slime-fancy
	       slime-tramp
	       slime-banner
	       slime-compiler-notes-tree
	       slime-package-fu
	       slime-indentation
	       slime-repl
	       slime-editing-commands
	       slime-fancy-inspector
	       slime-fontifying-fu
	       slime-fuzzy
	       slime-mdot-fu
	       slime-scratch
	       slime-xref-browser
	       slime-asdf
	       ;; slime-autodoc
	       ;; slime-indentation-fu
	       slime-references
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

;; NOTE: stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit (&rest junk)
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;; COMMENT: scheme programming
;; SOURCE: `http://emacswiki.org/emacs/Scheme'
(autoload 'scheme-mode "scheme" "Major mode for editing scheme source code files");; TODO: find a `guile-mode' for scheme ...

(add-hook 'scheme-mode-hook '(lambda ()
			       (turn-on-general-programming-mode)
			       (paredit-mode t)))

;;; COMMENT: haskell programming
;; SOURCE: `http://www.emacswiki.org/emacs/Haskell'
(autoload 'haskell-mode "haskell-site-file" "Major mode for editing haskell source code." t)

(setq haskell-font-lock-symbols t) ;; NOTE: enable unicode symbols for haskell

(defun custom-turn-on-haskell-modes (&rest junk)
  (turn-on-haskell-doc-mode) ;; NOTE: enable haskell's documentation mode
  (turn-on-haskell-indent)) ;; NOTE: enable haskell's indentation mode

(add-hook 'haskell-mode-hook '(lambda ()
				(turn-on-general-programming-mode)
				(custom-turn-on-haskell-modes)))

;;; COMMENT: shell script
;; SOURCE: `http://emacswiki.org/emacs/ShMode'
(autoload 'shell-script-mode "sh-mode" "Major mode for editing shell script source code." t)

(add-hook 'shell-script-mode '(lambda ()
				(turn-on-general-programming-mode)))

;;; COMMENT: python programming
;; SOURCE: `http://emacswiki.org/emacs/PythonProgrammingInEmacs'
(autoload 'python-mode "python" "Major mode for editing python source code." t)

(add-hook 'python-mode-hook '(lambda ()
			       (turn-on-general-programming-mode)))

;;; COMMENT: javascript programming
;; SOURCE: `http://www.emacswiki.org/emacs/JavaScriptMode'
;; (autoload 'javascript-mode "javascript" "Major mode for editing javascript source code." t)
(autoload 'js-mode "js" "Major mode for editing javascript source code." t)

;;; COMMENT: C/C++ programming
;; SOURCE: `http://www.emacswiki.org/emacs/CcMode'
(autoload 'c-mode "cc-mode" "Major mode for editing C source code." t)
(autoload 'c++-mode "cc-mode" "Major mode for editing C++ source code." t)
(autoload 'c-turn-on-eldoc-mode "c-edloc" "Minor mode for viewing function arguments." t)

(add-hook 'c-mode-hook '(lambda ()
			  (c-turn-on-eldoc-mode)
			  (turn-on-general-programming-mode)))

;;; COMMENT: maxima
;; SOURCE: `http://emacswiki.org/emacs/MaximaMode'
(autoload 'maxima-mode "maxima" "Major mode for interaction with maxima." t)
(autoload 'maxima "maxima" "Major mode for maxima interaction." t)
(autoload 'imaxima "imaxima" "Major mode frontend for maxima with image support." t)
(autoload 'imath-mode "imath" "Imath mode for math formula input." t)

(setq imaxima-use-maxima-mode-flag t)

;;; COMMENT: CEDET (collection of emacs development environment tools)
;; SOURCE: `http://emacswiki.org/emacs/CollectionOfEmacsDevelopmentEnvironmentTools'

(provide 'programming-config)

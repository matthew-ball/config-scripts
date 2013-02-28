;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/programming-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: flymake
;; SOURCE: `http://www.emacswiki.org/emacs/FlyMake'
(autoload 'flymake-mode "flymake" "On the fly compiling in GNU Emacs." t)
;; TODO: set `flymake-display-err-menu-for-current-line' to key-chord
;; TODO: set `flymake-goto-next-error' to key-chord
;; (setq flymake-log-level 3)

;; TODO: write some sort of `flymake' setup configuration function in elisp
;; TODO: set up a project thingy (keep track of files in project, create makefile based on project)
;; TODO: this is probably of sufficient size to now warrant its own project file

(defvar compiler-list  '() "List of available compilers for `flymake-mode'.")
(defvar compiler-flags '() "List of compiler flags to set.")
(defvar link-flags     '() "List of libraries to link during compilation phase.")

(add-to-list 'compiler-list  '"gcc")       ;; NOTE: C programming
(add-to-list 'compiler-list  '"python")    ;; NOTE: python programming
(add-to-list 'compiler-list  '"ghc")       ;; NOTE: haskell programming
;;(add-to-list 'compiler-flags '"-Wall")     ;; NOTE: compile with warnings (all)
;;(add-to-list 'compiler-flags '"-ggdb")     ;; NOTE: compile with debug information
;;(add-to-list 'link-flags     '"-lpthread") ;; NOTE: link with pthreads library

(defun generate-makefile (projectname &rest junk)
  "..."
  (interactive "sEnter project name: ")
  (let ((cc       (ido-completing-read "Select compiler: " compiler-list))
	(cflags   "-Wall -ggdb")
	(ldflags  "-lpthread"))
    (insert-custom-header-text)
    (add-makefile-compiler-string cc cflags ldflags)
    (add-makefile-project)
    (add-makefile-default-directory-files)
    (add-makefile-suffix-string)
    (add-makefile-clean-string)
    (add-makefile-flymake-string)))

(defun add-makefile-project (&rest junk)
  "..."
  (insert (concat "\nall: " projectname
		  "\n"
		  "\n" projectname ": "
		  "\n"
		  "\n")))

(defun add-makefile-default-directory-files (&rest junk)
  "..."
  (let (files result)
    (setq files (directory-files default-directory t "\.c$" t))
    (dolist (file-name files)
      (when (and (file-readable-p file-name) (not (file-directory-p file-name)))
	(insert (concat "# " file-name "\n"))
	(setq result (cons file-name result))))
    result))

(defun add-makefile-suffix-string (&rest junk)
  "..."
  (insert (concat "\n.SUFFIXES: .c .o"
		  "\n.c.o:"
		  "\n\t$(CC) $(CFLAGS) -c $*.c"
		  "\n")))

(defun add-makefile-clean-string (&rest junk)
  "..."
  (insert (concat "\nclean:"
		  "\n\trm *.o"
		  "\n")))

(defun add-makefile-compiler-string (compiler flags library-flags &rest junk)
  "..."
  (insert (concat "\nCC      = " compiler
		  "\nCFLAGS  = " flags
		  "\nLDFLAGS = " library-flags
		  "\n")))

(defun add-makefile-flymake-string (&rest junk)
  "..."
  (insert (concat "\n # flymake-mode"
		  "\ncheck-syntax:"
		  "\n\t" "$(CC) -o nul -S ${CHK_SOURCES}"
		  "\n")))

;; TODO: move this to the `generel-programming-hook' function
;; (add-hook 'find-file-hook 'flymake-find-file-hook) ;; NOTE: start `flymake' when a new file is opened

;;; COMMENT: paredit
;; SOURCE: `http://emacswiki.org/emacs/ParEdit'
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;; COMMENT: general programming
;; TODO: make this a `general-programming-mode-hook'
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

;;; COMMENT: available modes for the which function mode-line tag
;; SOURCE: `http://www.emacswiki.org/emacs/WhichFuncMode'
;; SOURCE: `http://emacs-fu.blogspot.com.au/2009/01/which-function-is-this.html'
;; TODO: populate this variable
;; (setq which-func-modes
;;       '(emacs-lisp-mode
;; 	lisp-mode c-mode c++-mode python-mode objc-mode
;; 	perl-mode cperl-mode makefile-mode sh-mode
;; 	fortran-mode f90-mode ada-mode diff-mode))

;; TODO: ... maybe use this form ...
;; (eval-after-load "which-func"
;;   '(add-to-list 'which-func-modes 'lisp-mode)
;;   '(add-to-list 'which-func-modes 'emacs-lisp-mode))

;;; COMMENT: emacs lisp programming
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsLisp'
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsLispIntro'
(require 'eldoc)
;;(autoload 'eldoc-mode "eldoc" "GNU Emacs lisp documentation minor mode." t)

(eldoc-add-command 'paredit-backward-delete 'paredit-close-round) ;; NOTE: make `eldoc' recognise `paredit' functions

(defun turn-on-byte-compile-file (&rest junk)
  "Automatically byte compile `*.el' files."
  (if (eq major-mode 'emacs-lisp-mode)
      (save-excursion (byte-compile-file buffer-file-name))))

(add-hook 'after-save-hook '(lambda ()
			     ;; (turn-on-byte-compile-file) ;; NOTE: automatically byte compile `*.el' files
			     ))

(add-hook 'emacs-lisp-mode-hook '(lambda () ;; NOTE: active general programming mode
				   (turn-on-general-programming-mode)
                                   (turn-on-eldoc-mode)
				   (paredit-mode t)))

;;; COMMENT: interactive emacs lisp
;; SOURCE: 
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; COMMENT: emacs byte-compiled code
;; SOURCE: `http://www.emacswiki.org/emacs/CompiledFile'
;;(byte-recompile-directory (expand-file-name user-emacs-directory) 0)

;;; COMMENT: common lisp programming
;; SOURCE: `http://emacswiki.org/emacs/CommonLisp'
(setq inferior-lisp-program "/usr/bin/sbcl")

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
;;; COMMENT: slime/swank
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
(autoload 'c-turn-on-eldoc-mode "c-eldoc" "Minor mode for viewing function arguments." t)

(add-hook 'c-mode-hook '(lambda ()
			  ;;(c-turn-on-eldoc-mode)
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

;;; COMMENT: ctags/etags
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsTags'
;; SOURCE: `http://emacswiki.org/emacs/TagsFile'
;; SOURCE: `http://www.emacswiki.org/BuildTags'
;; TODO: add more source links

;;; COMMENT: bnf form
(define-generic-mode 'bnf-mode
  '("#") 
  nil 
  '(("^<.*?>" . 'font-lock-variable-name-face) 
    ("<.*?>" . 'font-lock-keyword-face) 
    ("::=" . 'font-lock-warning-face) 
    ("\|" . 'font-lock-warning-face))
  '("\\.bnf\\.pybnf\\'") 
  nil 
  "Major mode for BNF highlighting.")

(provide 'programming-config)

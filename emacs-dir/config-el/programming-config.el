;; ==========================================
;; ~/.emacs.d/config-el/programming-config.el
;; Matthew Ball (copyleft 2012)
;; ==========================================

;;; general programming
(defun turn-on-general-programming-mode ()
  "General function for programming modes."
  (modify-syntax-entry ?- "w") ;; treat '-' as part of the word
  ;; (flymake-mode) ;; turn on flymake mode
  (hs-minor-mode))

;;; emacs lisp programming
(autoload 'eldoc-mode "eldoc" "GNU Emacs lisp documentation minor mode." t)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(add-hook 'emacs-lisp-mode-hook '(lambda ()
				   (turn-on-general-programming-mode)
				   (eldoc-mode t)))

;;; common lisp programming
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

;;; slime
(autoload 'slime "slime" "The Superior Lisp Interaction mode for Emacs" t)

(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
     '(slime-setup '(slime-repl
		     slime-asdf
		     ;; slime-autodoc
		     slime-editing-commands
		     slime-fancy-inspector
		     slime-fontifying-fu
		     slime-fuzzy
		     slime-indentation
		     slime-mdot-fu
		     slime-package-fu
		     slime-references
		     slime-sbcl-exts
		     slime-scratch
		     slime-xref-browser))))

;; (slime-autodoc-mode)
(setq inferior-lisp-program "/usr/bin/sbcl" ;; use sbcl as the lisp environment
      slime-net-coding-system 'utf-8-unix
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(defun start-slime-automatically () ;; automatically start slime when opening a lisp file
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook '(lambda ()
			      (turn-on-general-programming-mode)
			      (start-slime-automatically)))

;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;; haskell programming
(autoload 'haskell-mode "haskell-site-file" "Major mode for editing Haskell code." t)

(setq haskell-font-lock-symbols t) ;; enable unicode symbols for haskell

(defun custom-turn-on-haskell-modes (&rest junk)
  (turn-on-haskell-doc-mode) ;; enable haskell's documentation mode
  (turn-on-haskell-indent)) ;; enable haskell's indentation mode

(add-hook 'haskell-mode-hook '(lambda ()
				(turn-on-general-programming-mode)
				(custom-turn-on-haskell-modes)))

;;; shell script
(add-hook 'shell-script-mode '(lambda ()
				(turn-on-general-programming-mode)))

;;; python programming
(autoload 'python-mode "python" "Python editing mode." t)

(add-hook 'python-mode-hook '(lambda ()
			       (turn-on-general-programming-mode)))

;;; maxima
(autoload 'maxima-mode "maxima" "Maxima mode." t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support." t)
(autoload 'maxima "maxima" "Maxima interaction." t)
(autoload 'imath-mode "imath" "Imath mode for math formula input." t)

(setq imaxima-use-maxima-mode-flag t)

(provide 'programming-config)

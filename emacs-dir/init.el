;; FILE: /home/chu/.conf-scripts/emacs-dir/init.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:03:46 EST

;;; COMMENT: load path
;; SOURCE: `http://emacswiki.org/emacs/LoadPath'
(add-to-list 'load-path (expand-file-name user-emacs-directory)) ;; NOTE: add `~/.emacs.d/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "config-el")) ;; NOTE: add `~/.emacs.d/config-el/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "apt-el")) ;; NOTE: add `~/.emacs.d/apt-el/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "my-modes")) ;; NOTE: add `~/.emacs.d/my-modes/' to `load-path' variable

;; WARNING: this requires `ELPA' has created its directory
;; TODO: I should just create the ELPA directory during install
(let ((default-directory (concat (expand-file-name user-emacs-directory) "elpa/")))
  (normal-top-level-add-subdirs-to-load-path)) ;; NOTE: recursively add sub-directories to `load-path' variable

;;; COMMENT: common lisp
;; SOURCE: `http://emacswiki.org/emacs/CommonLispForEmacs'
(eval-when-compile (require 'cl))

;;; COMMENT: customize configuration file
;; SOURCE: `http://www.emacswiki.org/emacs/CustomFile'
(setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))
(load custom-file 'noerror)

;;; COMMENT: configuration files
(require 'package-config)
(require 'appearance-config)
(require 'dired-config)
(require 'eshell-config)
(require 'general-config)
(require 'key-bindings-config)
(require 'programming-config)
(require 'org-config)
(require 'latex-config)
(require 'erc-config)
(require 'gnus-config)
(require 'user-config)

;;; COMMENT: start emacs server
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsAsDaemon'
;; (require 'server)
;; (when (and (functionp 'server-running-p) (not (server-running-p))) ;; NOTE: don't start the server unless we know it isn't running
;;   ;; (server-mode t) ;; enter server mode
;;   (server-start))

;;; COMMENT: shutdown emacs server
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsAsDaemon'
(defun server-shutdown (&rest junk)
  "Save buffers, quit, and shutdown (kill) GNU Emacs server."
  (interactive)
  (save-some-buffers) ;; NOTE: ask to save any modified buffers
  (desktop-save-in-desktop-dir) ;; NOTE: save desktop session
  (kill-emacs)) ;; NOTE: kill GNU Emacs instance

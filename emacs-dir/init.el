;; FILE: /home/chu/.conf-scripts/emacs-dir/init.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; TODO: read `http://www.mygooglest.com/fni/.emacs'

;;; COMMENT: load path
;; SOURCE: `http://emacswiki.org/emacs/LoadPath'
(add-to-list 'load-path (expand-file-name user-emacs-directory)) ;; NOTE: add `~/.emacs.d/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "config-el")) ;; NOTE: add `config-el/' to `load-path' variable
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "apt-el")) ;; NOTE: add `apt-el/' to `load-path' variable
;; TODO: replace `~/.emacs.d/my-modes/' with `~/.emacs.d/extras-el/' directory
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "my-modes")) ;; NOTE: add `my-modes/' to `load-path' variable

;; NOTE: this works, but the directory stuff should probably be moved into a single location
(add-to-list 'load-path "/home/chu/.emacs.d/dictem-1.0.4") ;; TODO: move to "../elisp/dictem-el/"
(add-to-list 'load-path "/home/chu/Programming/lisp/elisp/w3m-el/w3m")
(add-to-list 'load-path (expand-file-name "~/Programming/lisp/common-lisp/stumpwm/contrib")) ;; TODO: this is for `stumpwm'
;;(add-to-list 'load-path "/home/chu/.emacs.d/erc-extras") ;; TODO: move to "../elisp/erc-extras-el/"
;;(add-to-list 'load-path "/home/chu/Programming/lisp/elisp/wireless/wireless") ;; TODO: move to "../elisp/wireless-el/"
;;(add-to-list 'load-path "/home/chu/Programming/scheme/guile/guile/emacs") ;; TODO: ... this is for `guile-emacs' I imagine
;;(add-to-list 'Info-directory-list (expand-file-name "~/Programming/lisp/common-lisp/stumpwm/"))

;; WARNING: this requires `ELPA' has created its directory
(let ((default-directory (concat (expand-file-name user-emacs-directory) "elpa/")))
  (if (file-exists-p default-directory) ;; NOTE: if the directory `~/.emacs.d/elpa/' exists ...
      (normal-top-level-add-subdirs-to-load-path) ;; NOTE: ... then recursively add sub-directories to `load-path' variable
    (make-directory (expand-file-name "~/.emacs.d/elpa/")))) ;; NOTE: ... else create directory

;;; COMMENT: common lisp
;; SOURCE: `http://emacswiki.org/emacs/CommonLispForEmacs'
(eval-when-compile (require 'cl))

;;; COMMENT: customize configuration file
;; SOURCE: `http://www.emacswiki.org/emacs/CustomFile'
(setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))
(load custom-file 'noerror)

;;; COMMENT: load configuration files
(defun require-package (name)
  "Print a loading message and call `require' on package referred to by NAME."
  (let ((package (concat name "-config")))
    (message "Loading %s configuration" name)
    (funcall 'require (intern package))))

(require-package "package")
(require-package "dired")
(require-package "eshell")
(require-package "general")
(require-package "key-bindings")
(require-package "programming")
(require-package "org")
(require-package "latex")
(require-package "gnus")
(require-package "erc")
(require-package "user")
(require-package "appearance")

;;; COMMENT: shutdown emacs server
;; SOURCE: `http://www.emacswiki.org/emacs/EmacsAsDaemon'
(defun server-shutdown (&rest junk)
  "Save buffers, quit, and shutdown (kill) GNU Emacs server."
  (interactive)
  (kill-emacs)) ;; NOTE: kill GNU Emacs instance

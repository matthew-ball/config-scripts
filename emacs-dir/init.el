;; ~/.conf-scripts/emacs-dir/init.el
;; Matthew Ball (copyleft 2012)

;;; COMMENT: load path
(add-to-list 'load-path (expand-file-name user-emacs-directory)) ;; add "~/.emacs.d/" to user load path
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "config-el")) ;; add "~/.emacs.d/config-el" to user load-path
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "apt-el")) ;; add "~/.emacs.d/apt-el" to user load-path

(let ((default-directory (concat (expand-file-name user-emacs-directory) "elpa/"))) (normal-top-level-add-subdirs-to-load-path)) ;; add sub-directories to load-path

;;; COMMENT: common lisp
(eval-when-compile (require 'cl))

;;; COMMENT: customize configuration file
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
(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p))) ;; don't start the server unless we know it isn't running
  ;; (server-mode t) ;; enter server mode
  (server-start))

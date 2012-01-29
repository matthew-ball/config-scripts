;; ======================================
;; ~/.emacs.d/config-el/package-config.el
;; Matthew Ball (copyleft 2012)
;; ======================================

;;; package manager
(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;; TODO: set `core-packages' and `user-packages' variables and functions ...
;; (defvar core-packages (list '...) "Core packages to be installed through ELPA.")
;; (defvar user-packages (list '...) "User packages to be installed through ELPA.") ;; FIXME: implement this over a custom list - variable in user-config.el (???)

(defvar my-packages (list 'magit 'haskell-mode 'diminish 'color-theme 'slime 'smex 'zenburn 'paredit 'ido-ubiquitous) "Libraries to be installed by default.")

;; TODO: this seems redundant ...
;; (defun add-core-elpa-package (package)
;;   "Add individual packages to the `core-packages' list."
;;   (add-to-list 'core-packages 'package))

;; (defun add-user-elpa-package (package)
;;   "Add packages to the `my-packages' list."
;;   (add-to-list 'user-packages 'package))

(defun add-user-elpa-package (package)
  "Add packages to the `my-packages' list."
  (add-to-list 'my-packages 'package))

(add-user-elpa-package 'emms) ;; add user-config settings

(defun emacs-custom-elpa-package-install ()
  "Install all custom configuration packages from ELPA which are not installed."
  (interactive)
  (dolist (package my-packages)
    (message "Package %s" (symbol-name package))
    (unless (or (member package package-activated-list)
		(functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package)
      (message "Package %s installed" (symbol-name package)))))

(defun check-internet-status ()
  "Check to see if computer is online. (This might not work on Windows)"
  (if (and (functionp 'network-interface-list) (network-interface-list))
      (some (lambda (iface)
	      (unless (equal "lo" (car iface))
		(member 'up (first (last (network-interface-info (car iface)))))))
	    (network-interface-list)) t))

;; FIXME: this is a bit buggy .. debugging /appears/ to give desired outputs though ... (???)
;; (when (check-internet-status) ;; this should not be called too often ...
;;   (unless package-archive-contents ;; if the package-archive-contents are out of date ...
;;     (package-refresh-contents)) ;; ... check to make sure package archives are updated (WARNING, this is a bit painful ...)
;;   (emacs-custom-elpa-package-install)) ;; install custom packages

(provide 'package-config)

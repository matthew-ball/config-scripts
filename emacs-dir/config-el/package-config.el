;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/package-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:05:54 EST

;;; COMMENT: emacs package manager
;; SOURCE: http://emacswiki.org/emacs/ELPA
(autoload 'package "package" "GNU Emacs lisp package management." t)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;; FIX: store `core' and `user' package lists in an external source (a variable in `init.el' and `user-config.el' perhaps)
(defvar core-packages (list 'magit 'haskell-mode 'diminish 'color-theme 'slime 'smex 'zenburn 'paredit 'ido-ubiquitous) "Core packages to be installed through ELPA.") ;; TODO: this is out of date
(defvar user-packages (list 'emms 'eproject) "User packages to be installed through ELPA.") ;; TODO: this is out of date
(defvar list-packages (list '()) "Packages to be installed through ELPA.")

;; NOTE: add core and user packages together
(add-to-list 'list-packages (append core-packages user-packages)) ;; TODO: make a choice whether to download core and user packages?

(defun emacs-custom-elpa-package-install (&rest junk)
  "Install all custom configuration packages from ELPA.

NOTE: This function only needs to be called the first time emacs is run under this setup."
  (interactive)
  (dolist (package list-packages)
    (message "Package %s" (symbol-name package))
    (unless (or (member package package-activated-list)	(functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package)
      (message "Package %s installed" (symbol-name package)))))

(defun check-internet-status (&rest junk)
  "Check to see if computer is connected to the internet.

NOTE: This function might not work on Windows."
  (if (and (functionp 'network-interface-list) (network-interface-list))
      (some (lambda (iface)
	      (unless (equal "lo" (car iface))
		(member 'up (first (last (network-interface-info (car iface)))))))
	    (network-interface-list)) t))

(defun run-initial-setup (&rest junk) ;; FIX: debugging /appears/ to give desired outputs though (???)
  "If the computer is connected to the internet then update package archives and install custom packages.

NOTE: This function only needs to be called the first time emacs is run under this setup."
  (when (check-internet-status) ;; this should not be called too often ...
    (unless package-archive-contents ;; if the package-archive-contents are out of date ...
      (package-refresh-contents)) ;; ... check to make sure package archives are updated (WARNING, this is a bit painful ...)
    (emacs-custom-elpa-package-install))) ;; install custom packages

;;; COMMENT: system package manager
(autoload 'apt "apt" "Debian (Ubuntu) package management major mode for GNU Emacs." t) ;; TODO: clean this up
;; (autoload 'arch "arch-mode" "Arch package management major mode for GNU Emacs." t) ;; NOTE: create file (???)

(defun check-dist-name (name &rest junk)
  "Return true if distribution name matches NAME string, false otherwise.

NOTE: distribution name is 16 characters into the output of 'lsb_release -i'."
  (let ((dist-name (substring (shell-command-to-string "/usr/bin/lsb_release -i") 16)))
    (string= (read dist-name) name)))

(defun run-package-manager (&rest junk)
  "Run the system package manager inside GNU Emacs.

If a debian or ubuntu system, run `apt'.
If an arch system, run `pacman'.
Otherwise, use no system."
  (interactive)
  (cond ((or (check-dist-name "Debian") (check-dist-name "Ubuntu")) (apt)) ;; NOTE: start apt-mode (debian/ubuntu)
	((checkdist-name "Arch") (pacman)) ;; NOTE: start pacman-mode (arch)
	(t (message "no system"))))

(defun pacman (&rest junk) ;; TODO: find emacs mode for pacman (arch linux)
  "dummy pacman message"
  (message "pacman"))

(provide 'package-config)

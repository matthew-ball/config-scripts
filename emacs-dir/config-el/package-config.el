;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/package-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:05:54 EST

;;; COMMENT: emacs package manager
;; SOURCE: `http://emacswiki.org/emacs/ELPA'
(autoload 'package "package" "GNU Emacs lisp package management." t)

;; NOTE: set download repositories
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;; NOTE: to be honest, I don't know why I have this distinction
;; FIX: store `core-packages' and `user-packages' lists in an external source:
;; - a variable in `init.el' and `user-config.el' respectively
(defvar core-packages nil "Core packages to be installed through ELPA.")
(defvar user-packages nil "User packages to be installed through ELPA.")
(defvar list-packages nil "Packages to be installed through ELPA.")

;; FIX: set these values elsewhere (???)
(setq core-packages (list 'diminish 'color-theme 'smex 'zenburn 'paredit 'ido-ubiquitous)) ;; NOTE: this is updated
(setq user-packages (list 'emms 'eproject 'fill-column-indicator 'magit 'gh 'gist 'c-eldoc 'haskell-mode)) ;; NOTE: this is updated
(setq list-packages (append core-packages user-packages)) ;; NOTE: add `core-packages' and `user-packages' together

(defun emacs-custom-elpa-package-install (&rest junk)
  "Install all custom configuration packages from ELPA.

NOTE: This function only needs to be called the first time GNU Emacs is run under this setup."
  (interactive)
  (dolist (package list-packages)
    (message "Package %s" (symbol-name package))
    (unless (or (member package package-activated-list)	(functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package)
      (message "Package %s installed" (symbol-name package)))))

(defun run-initial-setup (&rest junk) ;; FIX: debugging /appears/ to give desired outputs though (???)
  "If the computer is connected to the internet then update package archives and install custom packages.

NOTE: This function only needs to be called the first time GNU Emacs is run under this setup."
  (unless package-archive-contents ;; NOTE: if the package-archive-contents are out of date ...
    (package-refresh-contents)) ;; NOTE: ... check to make sure package archives are updated ...
  (emacs-custom-elpa-package-install)) ;; NOTE: ... else ... install custom packages

;;; COMMENT: system package manager
(autoload 'apt "apt" "Debian (Ubuntu) package management major mode for GNU Emacs." t) ;; TODO: clean this up
;; (autoload 'arch "arch" "Arch package management major mode for GNU Emacs." t) ;; NOTE: create file (???)

(defun check-dist-name (name &rest junk)
  "Return `true' if distribution name matches NAME string, `false' otherwise.

NOTE: distribution name is 16 characters into the output of 'lsb_release -i'."
  (let ((dist-name (substring (shell-command-to-string "/usr/bin/lsb_release -i") 16)))
    (string= (read dist-name) name)))

(defun run-package-manager (&rest junk)
  "Run the system package manager inside GNU Emacs.

If a Debian or ubuntu system, run `apt'.
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

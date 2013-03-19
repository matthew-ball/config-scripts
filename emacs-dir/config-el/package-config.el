;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/package-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: emacs package manager
;; SOURCE: `http://emacswiki.org/emacs/ELPA'
;;(autoload 'package-installed-p "package" "GNU Emacs lisp package management." t)
(require 'package)

;; NOTE: set download repositories
(setq package-archives '(;; ("elpa" . "http://tromey.com/elpa/") ;; NOTE: being phased out as of 2012-07-03
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar list-packages nil "Packages to be installed through ELPA.")

(setq list-packages '(bbdb
                      color-theme
		      deft
		      diminish
		      ebib
		      emms
		      fill-column-indicator
		      gh
		      gist
		      haskell-mode
		      ido-ubiquitous
		      logito
		      magit
		      paredit
		      pcache
		      smex
		      thesaurus
		      undo-tree
		      wget
		      zenburn))

(defvar custom-packages-alist nil "Packages to be installed through ELPA.")

(setq custom-packages-alist '(("adaptive-wrap" . 0.2)
			      ("bbdb" . 20130216.1043)
			      ("color-theme" . 20080305.834)
			      ("deft" . 20130220.40)
			      ("diminish" . 20091203.1912)
			      ("dired-details+" . 20121228.2028)
			      ("dired-details" . 20130122.14)
			      ("ebib" . 20130310.1719)
			      ("emms" . 20121219.1939)
			      ("ess" . 20130225.1754)
			      ("gh" . 20130301.1723)
			      ("gist" . 20130310.1342)
			      ("gnomenm" . 0.0.3)
			      ("haskell-mode" . 20130211.2254)
			      ("ido-ubiquitous" . 20121214.2145)
			      ("logito" . 20120225.2155)
			      ("magit" . 20130304.2017)
			      ("paredit" . 20110508.1256)
			      ("pcache" . 20120408.1206)
			      ("powerline" . 20130304.1323)
			      ("smex" . 20120915.2041)
			      ("thesaurus" . 20121125.1937)
			      ("undo-tree" . 20130119.926)
			      ("wget" . 1.94)
			      ("workgroups" . 20110724.1825)
			      ))

;; ERROR: somethng funny in this function
(defun emacs-custom-elpa-package-install (&rest junk)
  "Install all custom configuration packages from ELPA.

NOTE: This function only needs to be called the first time GNU Emacs is run under this setup."
  (interactive)
  (dolist (package list-packages)
    (when (not (package-installed-p package))
      (message "[ELPA] installing package %s" (symbol-name package))
      ;;(package-install package)
      )))

(defun run-initial-setup (&rest junk) ;; FIX: debugging /appears/ to give desired outputs though (???)
  "If the computer is connected to the internet then update package archives and install custom packages.

NOTE: This function only needs to be called the first time GNU Emacs is run under this setup."
  (unless package-archive-contents ;; NOTE: if the package-archive-contents are out of date ...
    (message "[ELPA] refreshing package database")
    (package-refresh-contents)) ;; NOTE: ... check to make sure package archives are updated ...
  (emacs-custom-elpa-package-install)) ;; NOTE: ... else ... install custom packages

;;; COMMENT: system package manager
;; NOTE: the following is *probably* useless
;;(autoload 'apt "apt" "Debian (Ubuntu) package management major mode for GNU Emacs." t) ;; TODO: clean this up
;;(autoload 'arch "arch" "Arch package management major mode for GNU Emacs." t) ;; NOTE: create file (???)

;; (defun check-dist-name (name &rest junk)
;;   "Return `true' if distribution name matches NAME string, `false' otherwise.

;; NOTE: distribution name is 16 characters into the output of 'lsb_release -i'."
;;   (let ((dist-name (substring (shell-command-to-string "/usr/bin/lsb_release -i") 16)))
;;     (string= (read dist-name) name)))

;; (defun run-package-manager (&rest junk)
;;   "Run the system package manager inside GNU Emacs.

;; If a Debian or ubuntu system, run `apt'.
;; If an arch system, run `pacman'.
;; Otherwise, use no system."
;;   (interactive)
;;   (cond ((or (check-dist-name "Debian") (check-dist-name "Ubuntu")) (apt)) ;; NOTE: start apt-mode (debian/ubuntu)
;; 	((checkdist-name "Arch") (pacman)) ;; NOTE: start pacman-mode (arch)
;; 	(t (message "no system"))))

;; (defun pacman (&rest junk) ;; TODO: find emacs mode for pacman (arch linux)
;;   "dummy pacman message"
;;   (message "pacman"))

(provide 'package-config)

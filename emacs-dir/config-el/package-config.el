;; ======================================
;; ~/.emacs.d/config-el/package-config.el
;; Matthew Ball (copyleft 2012)
;; ======================================

;;; package manager
(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar my-packages '(magit haskell-mode diminish color-theme slime smex zenburn paredit ido-ubiquitous emms) "Libraries to be installed by default.")

;; (when (not package-archive-contents) (package-refresh-contents)) ;; check to make sure package archives are updated

;; (dolist (p my-packages) (when (not (package-installed-p p)) (package-install p))) ;; install packages in my-packages list

(provide 'package-config)

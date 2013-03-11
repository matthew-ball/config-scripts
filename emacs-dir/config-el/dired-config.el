;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/dired-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: dired
;; SOURCE: `http://emacswiki.org/emacs/DiredMode'
;; (require 'dired)
(require 'dired-x) ;; FIX: change this to an autoload

;; (setq dired-listing-switches "-al") ;; NOTE: default listing switches

;; NOTE: make sizes human-readable by default, sort version numbers correctly, and put dotfiles and capital-letters first
(setq dired-listing-switches "-DaGghlv --group-directories-first --time-style=long-iso")

;;(setq dired-recursive-copies 'always)

;; NOTE: try suggesting dired targets
(setq dired-dwim-target t)

(add-hook 'dired-load-hook (lambda ()
			     (load "dired-x")
			     ;; NOTE: set `dired-x' variables here
			     ;; (setq dired-guess-shell-gnutar "tar")
			     ;; (setq dired-bind-jump t)
			     ;; (setq dired-bind-man t)
			     ;; (setq dired-bind-info t)
			     ;; (setq dired-x-hands-off-my-keys t)
			     ))

(add-hook 'dired-mode-hook (lambda ()
			     (turn-on-dired-find-alternate-file)
			     ;; NOTE: set `dired-x' buffer-local variables here
			     (dired-omit-mode 1)))

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; NOTE: hide un-interesting files in dired
      dired-omit-extensions (append dired-latex-unclean-extensions
                                    dired-tex-unclean-extensions
                                    dired-patch-unclean-extensions
				    dired-bibtex-unclean-extensions
				    dired-texinfo-unclean-extensions))

;; (eval-after-load "dired" '(setq dired-mode-map-orig dired-mode-map))
;; (eval-after-load "dired-x" '(setq dired-mode-map dired-mode-map-orig))

(defun turn-on-dired-find-alternate-file (&rest junk)
  "Enable `dired-find-alternate-file' function and modifies `dired-up-directory'."
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; NOTE: was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ;; NOTE: was dired-up-directory

;;; COMMENT: directory details
;; SOURCE: `http://www.emacswiki.org/emacs/DiredDetails'
(require 'dired-details+)

(setq dired-details-hidden-string "[...] ")

(dired-details-install)

(provide 'dired-config)

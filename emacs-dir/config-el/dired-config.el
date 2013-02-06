;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/dired-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: dired
;; SOURCE: `http://emacswiki.org/emacs/DiredMode'
;;(require 'dired)
(require 'dired-x) ;; FIX: change this to an autoload
;;(autoload 'dired-jump "dired-x" "Jump to dired buffer corresponding to current buffer." 'interactive)
;; (autoload 'dired-jump-other-window "dired-x" "Like \\[dired-jump] (`dired-jump') but in other window." 'interactive)

;; (setq dired-listing-switches "-al") ;; NOTE: default listing switches

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
				    dired-bibtex-unclean-extensions
				    dired-texinfo-unclean-extensions))

;; (eval-after-load "dired" '(setq dired-mode-map-orig dired-mode-map))
;; (eval-after-load "dired-x" '(setq dired-mode-map dired-mode-map-orig))

(defun turn-on-dired-find-alternate-file (&rest junk)
  "Enable `dired-find-alternate-file' function and modifies `dired-up-directory'."
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; NOTE: was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ;; NOTE: was dired-up-directory

(provide 'dired-config)

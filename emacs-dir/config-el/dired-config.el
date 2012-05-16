;; FILE: ~/.emacs.d/config-el/dired-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: dired
;; SOURCE: http://emacswiki.org/emacs/DiredMode
(put 'dired-find-alternate-file 'disabled nil) ;; NOTE: enable re-use of dired buffers

(require 'dired-x) ;; FIX: change this to an autoload

;; (setq dired-listing-switches "-al") ;; NOTE: default listing switches

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook (lambda ()
			     (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; NOTE: was dired-advertised-find-file
			     (dired-omit-mode 1)
			     (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))) ;; NOTE: was dired-up-directory

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; NOTE: hide un-interesting files in dired
      dired-omit-extensions (append dired-latex-unclean-extensions
				    dired-bibtex-unclean-extensions
				    dired-texinfo-unclean-extensions))

(provide 'dired-config)

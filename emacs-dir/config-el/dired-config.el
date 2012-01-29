;; ====================================
;; ~/.emacs.d/config-el/dired-config.el
;; Matthew Ball (copyleft 2012)
;; ====================================

;;; dired
(put 'dired-find-alternate-file 'disabled nil) ;; enable re-use of dired buffers

(require 'dired-x) ;; FIXME: change this to an autoload

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook (lambda ()
			     (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; was dired-advertised-find-file
			     (dired-omit-mode 1)
			     (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))) ;; was dired-up-directory

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; hide un-interesting files in dired
      dired-omit-extensions (append dired-latex-unclean-extensions
				    dired-bibtex-unclean-extensions
				    dired-texinfo-unclean-extensions))

(provide 'dired-config)

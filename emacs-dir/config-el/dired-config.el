;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/dired-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:05:35 EST

;;; COMMENT: dired
;; SOURCE: `http://emacswiki.org/emacs/DiredMode'
(put 'dired-find-alternate-file 'disabled nil) ;; NOTE: enable re-use of dired buffers

(require 'dired-x) ;; FIX: change this to an autoload

;; (setq dired-listing-switches "-al") ;; NOTE: default listing switches

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook (lambda ()
			     (turn-on-dired-find-alternate-file)
			     (dired-omit-mode 1)))

(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; NOTE: hide un-interesting files in dired
      dired-omit-extensions (append dired-latex-unclean-extensions
				    dired-bibtex-unclean-extensions
				    dired-texinfo-unclean-extensions))

(defun turn-on-dired-find-alternate-file (&rest junk)
  "Enable `dired-find-alternate-file' function and modifies `dired-up-directory'."
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ;; NOTE: was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ;; NOTE: was dired-up-directory

;; TODO: test these things
;; (autoload 'dired-jump "dired-x" "Jump to dired buffer corresponding to current buffer." 'interactive)
;; (autoload 'dired-jump-other-window "dired-x" "Like \\[dired-jump] (`dired-jump') but in other window." 'interactive)

;; (setq dired-bind-jump t)

;; (global-set-key "\C-x\C-j" 'dired-jump)
;; (global-set-key "\C-x4\C-j" 'dired-jump-other-window)

;; (setq dired-x-hands-off-my-keys t)
;; (setq dired-bind-man nil)
;; (setq dired-bind-info nil)
;; (eval-after-load "dired" '(setq dired-mode-map-orig dired-mode-map))
;; (eval-after-load "dired-x" '(setq dired-mode-map dired-mode-map-orig))

(provide 'dired-config)

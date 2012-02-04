;; ~/.emacs.d/config-el/eshell-config.el
;; Matthew Ball (copyleft 2012)

;;; eshell
(require 'ansi-color)
(require 'eshell)
(require 'em-smart)

;; (autoload 'ansi-color "ansi-color" "ANSI colour library for GNU Emacs lisp." t)
;; (autoload 'eshell "eshell" "GNU Emacs Shell." t)
;; (autoload 'em-smart "esh-mode" "..." t)

(setq eshell-prompt-function (lambda () (concat (user-login-name) "@" (system-name) ":" (eshell/pwd) (if (= (user-uid) 0) "# " "$ "))) ;; modify eshell prompt
      eshell-prompt-regexp "^[^#$\n]*[#$] " ;; fix shell auto-complete
      eshell-cmpl-cycle-completions nil ;; avoid cycle-completion
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'" ;; ignore file prefixes
      eshell-save-history-on-exit t ;; save eshell history on exit
      eshell-where-to-jump 'begin ;; jump to beginning of line
      eshell-review-quick-commands nil ;; enable quick review
      eshell-smart-space-goes-to-end t) ;; save buffer history

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start eshell-last-output-end))

(defun eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color) ;; enable colours in eshell sessions

(provide 'eshell-config)

;; FILE: ~/.emacs.d/config-el/key-bindings-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: global key-bindings
;; (global-set-key (kbd "<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "M-<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "<f2>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "M-<f2>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<f2>") 'function) ;; NOTE: ...
(global-set-key (kbd "<f3>") 'switch-to-dot-emacs) ;; NOTE: switch to ~/.emacs.d/init.el file (or evaluate-buffer if already present)
(global-set-key (kbd "M-<f3>") 'show-custom-comment-tag) ;; NOTE: show custom comment tags in an outline structure
(global-set-key (kbd "C-<f3>") 'show-dot-file-structure) ;; NOTE: show the structure of the "same" dot-config files ;; ERROR: this function only works with '.el' extensions
(global-set-key (kbd "<f4>") 'erc-start-or-switch) ;; NOTE: start an erc session (or switch to the most active buffer)
;; (global-set-key (kbd "M-<f4>") 'function) ;; NOTE: ...
(global-set-key (kbd "C-<f4>") 'gnus) ;; NOTE: start a gnus session (or switch to an existing session)
(global-set-key (kbd "<f5>") 'eproject-setup-toggle) ;; NOTE: show/hide the project configuration browser
(global-set-key (kbd "M-<f5>") 'magit-status) ;; NOTE: view the git-status of the current file with magit
;; (global-set-key (kbd "C-<f5>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "<f6>") 'function) ;; NOTE: ...
(global-set-key (kbd "M-<f6>") 'ido-goto-symbol) ;; NOTE: go to a symbol in the current buffer
(global-set-key (kbd "C-<f6>") 'eval-and-replace) ;; NOTE: evaluate a lisp expression and replace with the value
(global-set-key (kbd "<f7>") 'slime) ;; NOTE: start slime session
(global-set-key (kbd "M-<f7>") 'imaxima) ;; NOTE: start interactive maxima session
;; (global-set-key (kbd "C-<f7>") 'function) ;; NOTE: ...
(global-set-key (kbd "<f8>") 'org-agenda) ;; NOTE: call up agenda screen
(global-set-key (kbd "M-<f8>") 'org-refile) ;; NOTE: refile target
;; (global-set-key (kbd "C-<f8>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "<f9>") 'function) ;; NOTE: ...
(global-set-key (kbd "M-<f9>") 'elisp-index-search) ;; NOTE: search for the documentation of an emacs lisp function
(global-set-key (kbd "C-<f9>") 'emacs-index-search) ;; NOTE: search for the documentation of an emacs command
(global-set-key (kbd "<f10>") 'browse-url) ;; NOTE: run web browser
(global-set-key (kbd "M-<f10>") 'run-package-manager) ;; NOTE: run the system package manager
(global-set-key (kbd "C-<f10>") 'package-list-packages) ;; NOTE: list available elpa packages
(global-set-key (kbd "<f11>") 'emms) ;; NOTE: start emacs multimedia system
(global-set-key (kbd "M-<f11>") 'emms-player-mpd-connect) ;; NOTE:  connect emms to mpd
(global-set-key (kbd "C-<f11>") 'proced) ;; NOTE:  start a proced session (process manager)
(global-set-key (kbd "<f12>") 'linum-mode) ;; NOTE:  turn on line numbering
(global-set-key (kbd "M-<f12>") (lambda () (interactive) (switch-to-buffer "*scratch*"))) ;; NOTE: switch to the scratch buffer
(global-set-key (kbd "C-<f12>") 'regexp-builder) ;; NOTE:  start regular-expression builder
;; ---
(global-set-key (kbd "TAB") 'smart-tab) ;; NOTE: use smart tab
(global-set-key (kbd "C-<tab>") 'other-window) ;; NOTE: C-TAB moves to other (next) window
(global-set-key (kbd "M-<tab>") (lambda () (interactive (other-window -1)))) ;; NOTE: M-TAB moves to other (previous) window
(global-set-key (kbd "M-x") 'smex) ;; NOTE: smex improves default ido at the mini buffer
(global-set-key	(kbd "M-X") 'smex-major-mode-commands) ;; NOTE: available major mode commands
(global-set-key	(kbd "C-c C-c M-x") 'execute-extended-command) ;; NOTE: original M-x command
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; NOTE: open ibuffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; NOTE: shows a list of recently opened files
(global-set-key (kbd "C-x f") 'recentf-ido-find-file) ;; NOTE: use ido to navigate recentf files
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window) ;; NOTE: open new window
(global-set-key (kbd "C-z") 'undo) ;; NOTE: undo some previous change
;; ---
(global-set-key (kbd "C-x a r") 'align-regexp) ;; NOTE: align columns within a region
;; ---
(global-set-key (kbd "C-s") 'isearch-forward-regexp) ;; NOTE: use regexp searches by default
(global-set-key (kbd "C-r") 'isearch-backward-regexp) ;; NOTE: ...
(global-set-key (kbd "M-%") 'query-replace-regexp) ;; NOTE: ...
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)
;; ---
(global-set-key (kbd "C-c c c") 'insert-custom-comment-tag) ;; NOTE: insert a custom comment tag
(global-set-key (kbd "C-c c s") 'show-custom-comment-tag) ;; NOTE: show custom comment tags
(global-set-key (kbd "C-c c h") 'insert-custom-header-text) ;; NOTE: insert custom header text for a source code dot file
;; ---
;; (global-set-key (kbd "<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "M-<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "s-<menu>") 'function) ;; NOTE: ...
;; ---
;; (global-unset-key (kbd "C-z")) ;; NOTE: remove binding on C-z (suspend-frame)

(provide 'key-bindings-config)

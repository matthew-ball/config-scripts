;; ===========================================
;; ~/.emacs.d/config-el/key-bindings-config.el
;; Matthew Ball (copyleft 2012)
;; ===========================================

;;; global key-bindings
;; (global-set-key (kbd "<f1>") 'function) ;; ...
;; (global-set-key (kbd "M-<f1>") 'function) ;; ...
;; (global-set-key (kbd "C-<f1>") 'function) ;; ...
;; (global-set-key (kbd "<f2>") 'function) ;; ...
;; (global-set-key (kbd "M-<f2>") 'function) ;; ...
;; (global-set-key (kbd "C-<f2>") 'function) ;; ...
(global-set-key (kbd "<f3>") 'switch-to-dot-emacs) ;; switch to ~/.emacs.d/init.el file (or evaluate-buffer if already present)
(global-set-key (kbd "M-<f3>") 'show-bugs-fixes-todos) ;; show any TODO items in the source code comments of a file
(global-set-key (kbd "C-<f3>") 'show-dot-file-structure) ;; show the structure of the active dot-config file
(global-set-key (kbd "<f4>") 'erc-start-or-switch) ;; start an erc session (or switch to the most active buffer)
;; (global-set-key (kbd "M-<f4>") 'function) ;; ...
(global-set-key (kbd "C-<f4>") 'gnus) ;; start a gnus session (or switch to an existing session)
(global-set-key (kbd "<f5>") 'eproject-setup-toggle) ;; show/hide the project configuration browser
(global-set-key (kbd "M-<f5>") 'magit-status) ;; view the git-status of the current file with magit
;; (global-set-key (kbd "C-<f5>") 'function) ;; ...
;; (global-set-key (kbd "<f6>") 'function) ;; ...
(global-set-key (kbd "M-<f6>") 'ido-goto-symbol) ;; go to a symbol in the current buffer
(global-set-key (kbd "C-<f6>") 'eval-and-replace) ;; evaluate a lisp expression and replace with the value
(global-set-key (kbd "<f7>") 'slime) ;; start slime session
(global-set-key (kbd "M-<f7>") 'imaxima) ;; start interactive maxima session
;; (global-set-key (kbd "C-<f7>") 'function) ;; ...
(global-set-key (kbd "<f8>") 'org-agenda) ;; call up agenda screen
(global-set-key (kbd "M-<f8>") 'org-refile) ;; refile target
;; (global-set-key (kbd "C-<f8>") 'function) ;; ...
;; (global-set-key (kbd "<f9>") 'function) ;; ...
(global-set-key (kbd "M-<f9>") 'elisp-index-search) ;; search for the documentation of an emacs lisp function
(global-set-key (kbd "C-<f9>") 'emacs-index-search) ;; search for the documentation of an emacs command
(global-set-key (kbd "<f10>") 'package-list-packages) ;; list available elpa packages
(global-set-key (kbd "M-<f10>") 'run-package-manager) ;; run the system package manager
;; (global-set-key (kbd "C-<f10>") 'function) ;; ...
(global-set-key (kbd "<f11>") 'emms) ;; start emacs multimedia system
(global-set-key (kbd "M-<f11>") 'emms-player-mpd-connect) ;; connect emms to mpd
;; (global-set-key (kbd "C-<f11>") 'function) ;; ...
(global-set-key (kbd "<f12>") 'linum-mode) ;; turn on line numbering
(global-set-key (kbd "M-<f12>") (lambda () (interactive) (switch-to-buffer "*scratch*"))) ;; switch to the scratch buffer
(global-set-key (kbd "C-<f12>") 'regexp-builder) ;;  start regular-expression builder
;; ---
(global-set-key (kbd "TAB") 'smart-tab) ;; use smart tab
(global-set-key (kbd "M-x") 'smex) ;; smex improves default ido at the mini buffer
(global-set-key	(kbd "M-X") 'smex-major-mode-commands) ;; available major mode commands
(global-set-key	(kbd "C-c C-c M-x") 'execute-extended-command) ;; original M-x command
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; open ibuffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; shows a list of recently opened files
(global-set-key (kbd "C-x f") 'recentf-ido-find-file) ;; use ido to navigate recentf files
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window) ;; open new window
(global-set-key (kbd "C-z") 'undo) ;; undo some previous change
;; ---
(global-set-key (kbd "C-s") 'isearch-forward-regexp) ;; use regexp searches by default
(global-set-key (kbd "C-r") 'isearch-backward-regexp) ;; ...
(global-set-key (kbd "M-%") 'query-replace-regexp) ;; ...
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)
;; ---
(global-set-key (kbd "C-c i") 'insert-custom-tag) ;; insert a custom tag comment
(global-set-key (kbd "C-c h") 'insert-custom-dot-file-header-text) ;; insert custom header text for a source code dot file
;; ---
;; (global-set-key (kbd "<menu>") 'function) ;; ...
;; (global-set-key (kbd "M-<menu>") 'function) ;; ...
;; (global-set-key (kbd "C-<menu>") 'function) ;; ...
;; (global-set-key (kbd "s-<menu>") 'function) ;; ...
;; ---
;; (global-unset-key (kbd "C-z")) ;; remove binding on C-z (suspend-frame)

(provide 'key-bindings-config)

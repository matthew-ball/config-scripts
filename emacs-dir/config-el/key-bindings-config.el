;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/key-bindings-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:06:02 EST

;;; COMMENT: considering changing the `Ctrl' key to `CapsLock'

;;; COMMENT: global key-bindings
;;; COMMENT: for terminal mode
(defun terminal-mode-init (&rest args)
  "Cleans up how GNU Emacs receives/interprets the CONTROL and META characters when run in a terminal session."
  (interactive)
  (define-key input-decode-map "[[A" (kbd "<f1>")) ;; NOTE: bind `<f1>' in (tty) terminal
  (define-key input-decode-map "[[B" (kbd "<f2>")) ;; NOTE: bind `<f2>' in (tty) terminal
  (define-key input-decode-map "[[C" (kbd "<f3>")) ;; NOTE: bind `<f3>' in (tty) terminal 
  (define-key input-decode-map "[[D" (kbd "<f4>")) ;; NOTE: bind `<f4>' in (tty) terminal
  (define-key input-decode-map "[[E" (kbd "<f5>")) ;; NOTE: bind `<f5>' in (tty) terminal 
  (define-key input-decode-map "O1;3Q" (kbd "M-<f2>")) ;; NOTE: bind `M-<f2>' in terminal
  (define-key input-decode-map "O1;5Q" (kbd "C-<f2>")) ;; NOTE: bind `C-<f2>' in terminal
  (define-key input-decode-map "O1;2Q" (kbd "S-<f2>")) ;; NOTE: bind `S-<f2>' in terminal
  (define-key input-decode-map "O1;3R" (kbd "M-<f3>")) ;; NOTE: bind `M-<f3>' in terminal
  (define-key input-decode-map "O1;5R" (kbd "C-<f3>")) ;; NOTE: bind `C-<f3>' in terminal
  (define-key input-decode-map "O1;2R" (kbd "S-<f3>")) ;; NOTE: bind `S-<f3>' in terminal
  (define-key input-decode-map "O1;3S" (kbd "M-<f4>")) ;; NOTE: bind `M-<f4>' in terminal
  (define-key input-decode-map "O1;5S" (kbd "C-<f4>")) ;; NOTE: bind `C-<f4>' in terminal
  (define-key input-decode-map "O1;2S" (kbd "S-<f4>")) ;; NOTE: bind `S-<f4>' in terminal
  (define-key input-decode-map " [15;3~
    ]" (kbd "M-<f5>")) ;; NOTE: bind `M-<f5>'
  (define-key input-decode-map " [15;5~
    ]" (kbd "C-<f5>")) ;; NOTE: bind `C-<f5>'
  )

;; (terminal-mode-init) ;; NOTE: set `input-decode-map' variable ;; ERROR: this does not work
;; ---
;; (global-set-key (kbd "<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "M-<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "<f2>") 'function) ;; NOTE: ...
(global-set-key (kbd "M-<f2>") 'save-desktop-session) ;; NOTE: save the current GNU Emacs session
(global-set-key (kbd "C-<f2>") 'restore-desktop-session) ;; NOTE: load the saved GNU Emacs session
(global-set-key (kbd "<f3>") 'show-custom-comment-tag) ;; NOTE: show custom comments
(global-set-key (kbd "M-<f3>") 'search-string) ;; NOTE: search for a string over buffers
(global-set-key (kbd "C-<f3>") 'search-string-under-point) ;; NOTE: search string under point
(global-set-key (kbd "<f4>") 'erc-start-or-switch) ;; NOTE: start an erc session (or switch to the most active buffer)
(global-set-key (kbd "M-<f4>") 'terminal-start-or-switch) ;; NOTE: switch to an ANSI terminal session
(global-set-key (kbd "C-<f4>") 'gnus) ;; NOTE: start a gnus session (or switch to an existing session)

;; TODO: create `programming-prefix-key' for <f5>
(defconst programming-prefix-key (kbd "<f5>") "Programming prefix key.")
(defvar programming-map (lookup-key global-map programming-prefix-key) "Keymap designed for programming.")

(unless (keymapp programming-map) ;; NOTE: if `programming-map' is not yet defined ...
  (setq programming-map (make-sparse-keymap))) ;; NOTE:  ... set `programming-map' to a sparse key map

(define-key global-map programming-prefix-key programming-map) ;; NOTE: join `programming-map' to `programming-prefix-key'
(define-key programming-map (kbd "c") 'compile)
(define-key programming-map (kbd "n") 'next-error)
(define-key programming-map (kbd "p") 'previous-error)
(define-key programming-map (kbd "g") 'gdb)

(global-set-key (kbd "M-<f5>") 'magit-status) ;; NOTE: view the git-status of the current file with magit
;; (global-set-key (kbd "C-<f5>") 'dictionary-word) ;; NOTE: look up `current-word' from `http://dictionary.reference.com/'
(global-set-key (kbd "<f6>") 'count-words) ;; NOTE: count words in a buffer
(global-set-key (kbd "M-<f6>") 'ido-goto-symbol) ;; NOTE: go to a symbol in the current buffer
(global-set-key (kbd "C-<f6>") 'eval-and-replace) ;; NOTE: evaluate a lisp expression and replace with the value
(global-set-key (kbd "<f7>") 'slime) ;; NOTE: start slime session
(global-set-key (kbd "M-<f7>") 'imaxima) ;; NOTE: start interactive maxima session
(global-set-key (kbd "C-<f7>") 'ielm) ;; NOTE: interactive emacs lisp mode
(global-set-key (kbd "<f8>") 'org-agenda) ;; NOTE: call up agenda screen
(global-set-key (kbd "M-<f8>") 'org-refile) ;; NOTE: refile target
(global-set-key (kbd "C-<f8>") 'org-archive-subtree) ;; NOTE: archive an org-mode subtree entry
(global-set-key (kbd "<f9>") 'proced) ;; NOTE: start a process manager session
(global-set-key (kbd "M-<f9>") 'elisp-index-search) ;; NOTE: search for the documentation of an emacs lisp function
(global-set-key (kbd "C-<f9>") 'emacs-index-search) ;; NOTE: search for the documentation of an emacs command
;; (global-set-key (kbd "<f10>") 'function) ;; NOTE: ...
(global-set-key (kbd "M-<f10>") 'run-package-manager) ;; NOTE: run the system package manager
(global-set-key (kbd "C-<f10>") 'list-packages) ;; NOTE: list available elpa packages
(global-set-key (kbd "<f11>") 'emms) ;; NOTE: start emacs multimedia system
;; (global-set-key (kbd "M-<f11>") 'emms-player-mpd-connect) ;; NOTE:  connect emms to mpd
;; (global-set-key (kbd "C-<f11>") 'function) ;; NOTE:  ...
(global-set-key (kbd "<f12>") 'browse-url) ;; NOTE:  browse a URL session
(global-set-key (kbd "M-<f12>")	(lambda () (interactive) (switch-to-buffer "*scratch*"))) ;; NOTE: switch to the scratch buffer
(global-set-key (kbd "C-<f12>") 'regexp-builder) ;; NOTE:  start regular-expression builder
;; ---
(global-set-key (kbd "TAB") 'smart-tab)	;; NOTE: use smart tab
(global-set-key (kbd "C-<tab>") 'other-window) ;; NOTE: C-TAB moves to other (next) window
(global-set-key (kbd "M-x") 'smex) ;; NOTE: smex improves default ido at the mini buffer
(global-set-key	(kbd "M-X") 'smex-major-mode-commands) ;; NOTE: available major mode commands
(global-set-key	(kbd "C-c C-c M-x") 'execute-extended-command) ;; NOTE: original M-x command
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; NOTE: open ibuffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; NOTE: shows a list of recently opened files
(global-set-key (kbd "C-x f") 'recentf-ido-find-file) ;; NOTE: use ido to navigate recentf files
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window) ;; NOTE: open new window
(global-set-key (kbd "C-z") 'undo-tree-visualize) ;; NOTE: visualise changes in the `undo-tree'
;;(global-set-key (kbd "C-z") 'undo) ;; NOTE: undo some previous change
;; ---
(global-set-key (kbd "C-x a r") 'align-regexp) ;; NOTE: align columns within a region
;; ---
(global-set-key (kbd "C-s") 'isearch-forward-regexp)  ;; NOTE: use regexp searches by default
(global-set-key (kbd "C-r") 'isearch-backward-regexp) ;; NOTE: ...
(global-set-key (kbd "M-%") 'query-replace-regexp)    ;; NOTE: ...
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)
;; ---
(global-set-key (kbd "C-c c c") 'insert-custom-comment-tag) ;; NOTE: insert a custom comment tag
(global-set-key (kbd "C-c c s") 'show-custom-comment-tag)   ;; NOTE: show custom comment tags
(global-set-key (kbd "C-c c h") 'insert-custom-header-text) ;; NOTE: insert custom header text for a source code dot file
;; ---
;; (global-set-key (kbd "<menu>") 'function)   ;; NOTE: ...
;; (global-set-key (kbd "M-<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "s-<menu>") 'function) ;; NOTE: ...
;; ---
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-M-+") 'toggle-selective-display)
(global-set-key (kbd "C-c h") 'toggle-hiding) ;; NOTE: toggle code folding with C-c h
;; ---
;; (global-unset-key (kbd "C-z")) ;; NOTE: remove binding on C-z (suspend-frame)

(provide 'key-bindings-config)

;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/key-bindings-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;; TODO: considering changing the `Ctrl' key to `CapsLock'

;;; COMMENT: when running GNU Emacs inside a terminal mode
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

;; ERROR: this does not work
(unless (window-system)
  (terminal-mode-init)) ;; NOTE: set `input-decode-map' variable

;;; COMMENT: global key-bindings
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Key-Bindings.html'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html'
;; ---
;; (global-set-key (kbd "<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "M-<f1>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<f1>") 'function) ;; NOTE: ...

;; COMMENT: emacs session
(global-set-key (kbd "<f2>") 'server-shutdown) ;; NOTE: kill the current emacs --daemon session
(global-set-key (kbd "M-<f2>") 'save-desktop-session) ;; NOTE: save the current GNU Emacs session
(global-set-key (kbd "C-<f2>") 'restore-desktop-session) ;; NOTE: load the saved GNU Emacs session

;; COMMENT: search string
(global-set-key (kbd "<f3>") 'show-custom-comment-tag) ;; NOTE: show custom comments
(global-set-key (kbd "M-<f3>") 'search-string) ;; NOTE: search for a string over buffers
(global-set-key (kbd "C-<f3>") 'search-string-under-point) ;; NOTE: search string under point

;; COMMENT: emacs interactive functions (NOTE: these are my most commonly used shortcuts)
(global-set-key (kbd "<f4>") 'erc-start-or-switch) ;; NOTE: start an erc session (or switch to the most active buffer)
(global-set-key (kbd "M-<f4>") 'start-bash) ;; NOTE: switch to a bash shell
(global-set-key (kbd "C-<f4>") 'gnus) ;; NOTE: start a gnus session (or switch to an existing session)

;; COMMENT: programming specific tasks
(defconst programming-prefix-key (kbd "<f5>") "Programming prefix key.")
(defvar programming-map (lookup-key global-map programming-prefix-key) "Keymap designed for programming.")

(unless (keymapp programming-map) ;; NOTE: if `programming-map' is not yet defined ...
  (setq programming-map (make-sparse-keymap))) ;; NOTE:  ... set `programming-map' to a sparse key map

(define-key global-map programming-prefix-key programming-map) ;; NOTE: join `programming-map' to `programming-prefix-key'
(define-key programming-map (kbd "c") 'compile)
(define-key programming-map (kbd "n") 'next-error)
(define-key programming-map (kbd "p") 'previous-error)
(define-key programming-map (kbd "d") 'gdb)
(define-key programming-map (kbd "m") 'magit-status) ;; NOTE: view the `git-status' of the current file
(define-key programming-map (kbd "g") 'ido-goto-symbol) ;; NOTE: go to a symbol in the current buffer
(define-key programming-map (kbd "e") 'eval-and-replace) ;; NOTE: evaluate a lisp expression and replace with the value
(define-key programming-map (kbd "i") 'ielm) ;; NOTE: start the interactive emacs lisp mode
(define-key programming-map (kbd "s") 'slime) ;; NOTE: start slime session

;; (global-set-key (kbd "M-<f5>") 'function)
;; (global-set-key (kbd "C-<f5>") 'function)

;; COMMENT: writing specific tasks
(defconst writing-prefix-key (kbd "<f6>") "Writing prefix key.")
(defvar writing-map (lookup-key global-map writing-prefix-key) "Keymap designed for writing.")

(unless (keymapp writing-map)
  (setq writing-map (make-sparse-keymap)))

(define-key global-map writing-prefix-key writing-map)
(define-key writing-map (kbd "c") 'count-words) ;; NOTE: count the words in the current buffer
;;(define-key writing-map (kbd "d") 'dictionary-word) ;; NOTE: look up `current-word' from `http://dictionary.reference.com/'
(define-key writing-map (kbd "t") 'thesaurus-choose-synonym-and-replace) ;; NOTE: ...
(define-key writing-map (kbd "e") 'ebib) ;; NOTE: run the emacs bibliography manager
(define-key writing-map (kbd "d") 'dictem-run-search) ;; NOTE: dictionary search for word
(define-key writing-map (kbd "n") 'deft) ;; NOTE: quick note taking with `deft'

;;(global-set-key (kbd "M-<f6>") 'function)
;;(global-set-key (kbd "C-<f6>") 'function)

;; COMMENT: emacs internals
;; TODO: should have `gnus' and `erc' etc but meh
(defconst internals-prefix-key (kbd "<f7>") "Emacs internals prefix key.")
(defvar internals-map (lookup-key global-map internals-prefix-key) "Keymap designed for emacs internal functions.")

(unless (keymapp internals-map)
  (setq internals-map (make-sparse-keymap)))

(define-key global-map internals-prefix-key internals-map)
(define-key internals-map (kbd "E") 'elisp-index-search) ;; NOTE: search for the documentation of an emacs lisp function
(define-key internals-map (kbd "M") 'emacs-index-search) ;; NOTE: search for the documentation of an emacs command
(define-key internals-map (kbd "S") 'switch-to-scratch) ;; NOTE: switch to `*scratch*' buffer
(define-key internals-map (kbd "b") 'browse-url) ;; NOTE: browse a URL session
(define-key internals-map (kbd "c") 'bbdb-create) ;; NOTE: add an entry to the `bbdb' database
(define-key internals-map (kbd "v") 'battery) ;; NOTE: display battery statistics
(define-key internals-map (kbd "l") 'list-packages)  ;; NOTE: list available elpa packages
(define-key internals-map (kbd "e") 'emms) ;; NOTE: start emacs multimedia system
(define-key internals-map (kbd "p") 'proced) ;; NOTE: start a process manager session
(define-key internals-map (kbd "r") 'regexp-builder) ;; NOTE: start regular-expression builder
(define-key internals-map (kbd "m") 'imaxima) ;; NOTE: start interactive maxima session

;; (global-set-key (kbd "M-<f7>") 'function)
;; (global-set-key (kbd "C-<f7>") 'function)

;; COMMENT: `org-mode' related
(defconst org-prefix-key (kbd "<f8>") "Emacs org-mode prefix key.")
(defvar org-map (lookup-key global-map org-prefix-key) "Keymap designed for Emacs org-mode functions.")

(unless (keymapp org-map)
  (setq org-map (make-sparse-keymap)))

(define-key global-map org-prefix-key org-map)
(define-key org-map (kbd "a") 'org-agenda)
(define-key org-map (kbd "b") 'org-iswitchb)
(define-key org-map (kbd "c") 'org-capture)
(define-key org-map (kbd "d") 'org-deadline)
;;(define-key org-map (kbd "g") 'generate) ;; NOTE: what?
(define-key org-map (kbd "e") 'org-toggle-pretty-entities)
(define-key org-map (kbd "t") 'org-todo)
(define-key org-map (kbd "r") 'org-refile)
(define-key org-map (kbd "s") 'org-schedule)
(define-key org-map (kbd "A") 'org-archive-subtree)
(define-key org-map (kbd "S") 'org-store-link)

;; (global-set-key (kbd "M-<f8>") 'function)
;; (global-set-key (kbd "C-<f8>") 'function)

;; COMMENT:
;; (global-set-key (kbd "<f9>") 'function)
;; (global-set-key (kbd "M-<f9>") 'function)
;; (global-set-key (kbd "C-<f9>") 'function)

;; COMMENT:
;; (global-set-key (kbd "<f10>") 'function)
;; (global-set-key (kbd "M-<f10>") 'function)
;; (global-set-key (kbd "C-<f10>") 'function)

;; COMMENT:
;; (global-set-key (kbd "<f11>") 'function)
;; (global-set-key (kbd "M-<f11>") 'function)
;; (global-set-key (kbd "C-<f11>") 'function)

;; COMMENT:
;; (global-set-key (kbd "<f12>") 'function)
;; (global-set-key (kbd "M-<f12>") 'function)
;; (global-set-key (kbd "C-<f12>") 'function)
;; ---
;; (global-set-key (kbd "<menu>") 'function)   ;; NOTE: ...
;; (global-set-key (kbd "M-<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "C-<menu>") 'function) ;; NOTE: ...
;; (global-set-key (kbd "s-<menu>") 'function) ;; NOTE: ...
;; ---
(global-set-key (kbd "TAB") 'smart-tab)	;; NOTE: use smart tab
;;(global-set-key (kbd "C-<tab>") 'other-window) ;; NOTE: C-TAB moves to other (next) window
;; ---
(global-set-key (kbd "M-x") 'smex) ;; NOTE: smex improves default ido at the mini buffer
(global-set-key	(kbd "M-X") 'smex-major-mode-commands) ;; NOTE: available major mode commands
;; ---
(global-set-key	(kbd "C-c C-c M-x") 'execute-extended-command) ;; NOTE: original M-x command
;; --- `dired-x'
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
;; ---
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; NOTE: open ibuffer
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; NOTE: shows a list of recently opened files
(global-set-key (kbd "C-x f") 'recentf-ido-find-file) ;; NOTE: use ido to navigate recentf files
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window) ;; NOTE: open new window
;; ---
(global-set-key (kbd "C-z") 'undo-tree-visualize) ;; NOTE: visualise changes in the `undo-tree'
;;(global-set-key (kbd "C-z") 'undo) ;; NOTE: undo some previous change
;; ---
(global-set-key (kbd "C-x a r") 'align-regexp) ;; NOTE: align columns within a region
;; ---
(global-set-key (kbd "C-S-n") 'next-logical-line)
(global-set-key (kbd "C-S-p") 'previous-logical-line)
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
(global-set-key (kbd "C-<prior>") 'previous-user-buffer) ;; NOTE: bind C-<PGUP> to oldest buffer
(global-set-key (kbd "C-<next>") 'next-user-buffer) ;; NOTE: bind C-<PGDN> to most recently used
;; ---
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-M-+") 'toggle-selective-display)
(global-set-key (kbd "C-c h") 'toggle-hiding) ;; NOTE: toggle code folding with C-c h
;; ---
;; (global-unset-key (kbd "C-z")) ;; NOTE: remove binding on C-z (suspend-frame)

;;; COMMENT: mode specific key-bindings

(provide 'key-bindings-config)

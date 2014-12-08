;;; key-bindings-config.el --- Configuration for key-bindings

;; Copyright (C) 2008-2014  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: configuration

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration for key-bindings.

;;; Code:

(defgroup user-key-bindings nil "Custom key-binding variables." :group 'user-variables)

;; IMPORTANT: when running inside a terminal mode ...
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

(unless (window-system)
  (terminal-mode-init)) ;; NOTE: set `input-decode-map' variable

;;; IMPORTANT: global key-bindings
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Key-Bindings.html'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html'
;; IMPORTANT: emacs session
(global-set-key (kbd "<f2>") #'server-start) ;; NOTE: start an emacs --daemon service
(global-set-key (kbd "C-<f2>") #'server-shutdown) ;; NOTE: kill the current emacs --daemon session

;; IMPORTANT: emacs interactive functions (NOTE: these are my most commonly used shortcuts)
;; TODO: these should probably be set in `user-config.el'
(global-set-key (kbd "<f4>") #'erc-start-or-switch)
(global-set-key (kbd "S-<f4>") #'custom-erc-switch-buffer)
(global-set-key (kbd "M-<f4>") #'eshell)
(global-set-key (kbd "C-<f4>") #'gnus)

;; IMPORTANT: custom key maps
;; NOTE: this creates a map inside the global map
(defmacro custom-keymap (name prefix)
  (let* ((custom-name (concat "custom-" (symbol-name name)))
	 (custom-keymap (intern (concat custom-name "-map")))
	 (custom-prefix (intern (concat custom-name "-prefix-key"))))
    `(progn
       (defconst ,custom-prefix ,prefix "Custom prefix key.")
       (defvar ,custom-keymap (lookup-key global-map ,custom-prefix) "Custom keymap.")

       (unless (keymapp ,custom-keymap) (setq ,custom-keymap (make-sparse-keymap)))

       (define-key global-map ,custom-prefix ,custom-keymap))))

;; IMPORTANT: programming specific tasks
(custom-keymap programming (kbd "<f5>"))

(define-key custom-programming-map (kbd "c") #'compile)
(define-key custom-programming-map (kbd "n") #'next-error)
(define-key custom-programming-map (kbd "p") #'previous-error)
(define-key custom-programming-map (kbd "d") #'gdb)
(define-key custom-programming-map (kbd "G") #'ido-goto-symbol)
(define-key custom-programming-map (kbd "i") #'ielm)
;; TODO: `custom-slime-map' ??
(define-key custom-programming-map (kbd "D") #'slime-documentation-lookup) ;; TODO: `user-config.el'
(define-key custom-programming-map (kbd "s") #'slime-connect) ;; TODO: `user-config.el'
(define-key custom-programming-map (kbd "S") #'slime-disconnect) ;; TODO: `user-config.el'
(define-key custom-programming-map (kbd "SPC") #'slime-selector) ;; TODO: `user-config.el'


;; IMPORTANT: writing specific keys
(custom-keymap writing (kbd "<f6>"))

(define-key custom-writing-map (kbd "c") #'count-words)
(define-key custom-writing-map (kbd "u") #'upcase-word)
(define-key custom-writing-map (kbd "l") #'downcase-word)
(define-key custom-writing-map (kbd "C") #'capitalize-word)
(define-key custom-writing-map (kbd "i") #'ispell-word)
(define-key custom-writing-map (kbd "I") #'ispell-buffer)

;; IMPORTANT: emacs internals
(custom-keymap internals (kbd "<f7>"))

(define-key custom-internals-map (kbd "E") #'elisp-index-search)
(define-key custom-internals-map (kbd "M") #'emacs-index-search)
(define-key custom-internals-map (kbd "S") #'switch-to-scratch)
(define-key custom-internals-map (kbd "l") #'list-packages)
(define-key custom-internals-map (kbd "p") #'proced)
(define-key custom-internals-map (kbd "r") #'regexp-builder)
(define-key custom-internals-map (kbd "u") #'browse-url)
(define-key custom-internals-map (kbd "v") #'battery)

;; IMPORTANT: `org-mode' related
(custom-keymap org-mode (kbd "<f8>"))

(define-key custom-org-mode-map (kbd "a") #'org-agenda)
(define-key custom-org-mode-map (kbd "b") #'org-iswitchb)
(define-key custom-org-mode-map (kbd "c") #'org-capture)
(define-key custom-org-mode-map (kbd "d") #'org-deadline)
(define-key custom-org-mode-map (kbd "e") #'org-toggle-pretty-entities)
(define-key custom-org-mode-map (kbd "f") #'org-agenda-file-to-front)
(define-key custom-org-mode-map (kbd "t") #'org-todo)
(define-key custom-org-mode-map (kbd "r") #'org-refile)
(define-key custom-org-mode-map (kbd "s") #'org-schedule)
(define-key custom-org-mode-map (kbd "A") #'org-archive-subtree)
(define-key custom-org-mode-map (kbd "S") #'org-store-link)

;; IMPORTANT: global key-bindings
(global-set-key (kbd "TAB") #'smart-tab)
(global-set-key	(kbd "C-c C-c M-x") #'execute-extended-command)
(global-set-key (kbd "C-x B") #'ido-switch-buffer-other-window)
(global-set-key (kbd "C-x C-j") #'dired-jump)
(global-set-key (kbd "C-x 4 C-j") #'dired-jump-other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-r") #'recentf-open-files)
(global-set-key (kbd "C-x f") #'recentf-ido-find-file)
(global-set-key (kbd "C-x M-f") #'ido-find-file-other-window)
(global-set-key (kbd "C-x i") #'iedit-mode)
(global-set-key (kbd "C-x a r") #'align-regexp)
(global-set-key (kbd "C-S-n") #'next-logical-line)
(global-set-key (kbd "C-S-p") #'previous-logical-line)
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)
(global-set-key (kbd "C-M-%") #'query-replace)
(global-set-key (kbd "C-c c c") #'insert-custom-comment-tag)
(global-set-key (kbd "C-c c s") #'show-custom-comment-tag)
(global-set-key (kbd "C-c c h") #'insert-custom-header-text)
(global-set-key (kbd "C-+") #'toggle-hiding)
(global-set-key (kbd "C-M-+") #'toggle-selective-display)
(global-unset-key (kbd "<insert>"))

;; IMPORTANT: mode specific key-bindings

(provide 'key-bindings-config)
;;;key-bindings-config.el ends here

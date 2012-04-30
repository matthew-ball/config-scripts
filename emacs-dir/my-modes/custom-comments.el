;; FILE: /home/chu/.conf-scripts/emacs-dir/my-modes/custom-comments.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;; COMMENT:
;; Welcome to custom-comments - A minor mode extension for GNU Emacs managing highlighting of custom comments.
;; This mode is designed to require as little user interaction as possible:
;; The user just needs to enter what words they would like highlighted into the variable `custom-comment-tag-list'
;; The major modes for which this extension works are available in the variable `custom-comment-tag-mode-list'

;; COMMENT:
;; This is still a work in progress, eventual ideas are to split the list of comments into a `COMMENT' class and a `WARNING' class, with different colours of highlighting in each instance.

;;; COMMENT: highlight custom comment tags
(defvar font-lock-custom-comment-tag-face 'font-lock-custom-comment-tag-face "Face name to use for custom comment tags.")
(defface font-lock-custom-comment-tag-face '((t (:foreground "SpringGreen"))) "Font Lock mode face used to highlight custom comment tags." :group 'font-lock-faces)
(defvar custom-comment-tag-list '("AUTHOR" "BUG" "COMMENT" "DEBUG" "ERROR" "FILE" "FIX" "IMPORTANT" "NOTE" "TEST" "TODO" "WARNING") "Available custom comment tags.")
(defvar custom-comment-tag-mode-hooks
  '(emacs-lisp-mode-hook lisp-mode-hook shell-script-mode-hook sh-mode-hook haskell-mode-hook scheme-mode-hook)
  "Major modes which enable highlighting of custom comment tags.")

(defvar custom-comment-suppress-init-message nil "Suppress the printing of the initial activation message.")

;; TODO: Differentiate between `comment' and `warning' tags
;; (defvar custom-comment-tag-alist-comment '("AUTHOR" "COMMENT" "FILE" "IMPORTANT" "NOTE" "TODO"))
;; (defvar custom-comment-tag-alist-warning '("BUG" "DEBUG" "ERROR" "FIX" "WARNING" "TEST"))

;; (defvar font-lock-custom-comment-tag-face-comment 'font-lock-custom-comment-tag-face-comment)
;; (defface font-lock-custom-comment-tag-face-comment '((t (:foreground "SpringGreen"))))

;; (defvar font-lock-custom-todo-tag-face-warning 'font-lock-custom-comment-tag-face-warning)
;; (defface font-lock-custom-todo-tag-face-warning '((t (:foreground "OrangeRed"))))

(defun custom-comment-tag-regexp (&rest junk)
  "The \"optimised\" regular expresssion of the `custom-comment-tag-list' list variable."
  (concat (regexp-opt custom-comment-tag-list 'words) ":"))

(defun insert-custom-comment-tag (&rest junk) ;; TODO: there should be a check to make sure we have `ido-completing-read' available (???)
  "Insert a custom comment tag (see: `custom-comment-tag-list') in a source code file."
  (interactive)
  (insert (concat "" (make-string 2 (aref comment-start 0)) " " (ido-completing-read "Insert comment tag: " custom-comment-tag-list) ": ")))

(defun show-custom-comment-tag (&rest junk)
  "Show the custom comment tags (defined in the variable `custom-comment-tag-list') in an outline-mode structure.
This function depends on the multi-occur function `show-custom-structure'."
  (interactive)
  (show-custom-structure (custom-comment-tag-regexp))) ;; NOTE: this requires the function `custom-comment-tag-regexp'

(defun activate-highlight-custom-comment-tags (&rest junk)
  "Highlight custom comment tags in designated modes.
The custom comment \"tags\" are defined in the variable `custom-comment-tag-list'.
The \"designated\" modes are defined in the variable `custom-comment-tag-mode-hooks'."
  (setq temp-custom-comments-alist (concat "\\<" (regexp-opt custom-comment-tag-list) ":")) ;; TODO: there may be a better way of doing this
  (mapc
   (lambda (mode-hook)
     (add-hook mode-hook
	       (lambda ()
		 (font-lock-add-keywords nil
					 `((,temp-custom-comments-alist 0 font-lock-custom-comment-tag-face t))))))
   custom-comment-tag-mode-hooks)
  (when (eq custom-comment-suppress-init-message nil)
    (message "Custom highlight tags activated.")))

(provide 'custom-comments)

;; FILE: /home/chu/.conf-scripts/emacs-dir/my-modes/custom-comments.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: highlight custom comment tags
(defvar font-lock-custom-comment-tag-face 'font-lock-custom-comment-tag-face "Face name to use for custom comment tags.")
(defface font-lock-custom-comment-tag-face '((t (:foreground "SpringGreen"))) "Font Lock mode face used to highlight custom comment tags." :group 'font-lock-faces)
(defvar custom-comment-tag-list '("AUTHOR" "BUG" "COMMENT" "DEBUG" "ERROR" "FILE" "FIX" "IMPORTANT" "NOTE" "TEST" "TODO" "WARNING") "Available custom comment tags.")
(defvar custom-comment-tag-mode-hooks
  '(emacs-lisp-mode-hook lisp-mode-hook shell-script-mode-hook sh-mode-hook haskell-mode-hook scheme-mode-hook)
  "Major modes which enable highlighting of custom comment tags.")

(defvar custom-comment-suppress-init-message nil "Suppress the printing of the initial activation message.")

;; TODO: Differentiate between `comment' and `warning' tags
;; (defvar font-lock-custom-comment-tag-face)
;; (defface font-lock-custom-comment-tag-face '((t (:foreground "SpringGreen"))))

;; (defvar font-lock-custom-todo-tag-face)
;; (defface font-lock-custom-todo-tag-face '((t (:foreground "OrangeRed"))))

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
  (show-custom-structure (custom-comment-tag-regexp)))

(defun activate-highlight-custom-comment-tags (&rest junk) ;; ERROR: the regxp produces a string with only single backslahes, but font-lock-keywords wants double back-slashes
  "Highlight custom comment tags in designated modes.
The custom comment \"tags\" are defined in the variable `custom-comment-tag-list'.
The \"designated\" modes are defined in the variable `custom-comment-tag-mode-hooks'."
  (mapc
   (lambda (mode-hook)
     (add-hook mode-hook
	       (lambda ()
		 (font-lock-add-keywords nil
					 ;; '(((custom-comment-tag-regexp) 0 font-lock-custom-comment-tag-face t)))))) ;; ERROR: doesn't work
					 '(("\\<\\(AUTHOR\\|BUG\\|COMMENT\\|DEBUG\\|ERROR\\|FI\\(?:LE\\|X\\)\\|IMPORTANT\\|NOTE\\|T\\(?:EST\\|ODO\\)\\|WARNING\\):"
					    1 font-lock-custom-comment-tag-face t))))));; FIX: this string should not be hardcoded
   custom-comment-tag-mode-hooks)
  (when (eq custom-comment-suppress-init-message nil)
    (message "Custom highlight tags activated.")))

(provide 'custom-comments)

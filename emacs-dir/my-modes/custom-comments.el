;; FILE: /home/chu/.conf-scripts/emacs-dir/my-modes/custom-comments.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;; COMMENT:
;; Welcome to custom-comments - A minor mode extension for GNU Emacs managing highlighting of custom comments.
;; The mode also allows users to examine the structure of their configuration files
;; The user just needs to enter what words they would like highlighted into the variables:
;; - `custom-comment-tag-alist-comment' : for 'comment' related comments.
;; - `custom-comment-tag-alist-warning' : for 'warning' related comments.
;; The major modes for which this extension works are available in the variable `custom-comment-tag-mode-hooks'.

;; An example configuration would be:
;; (require 'custom-comments)
;; (setq custom-comment-suppress-init-message t)
;; (add-to-list custom-comment-tag-alist-comment "COMMENT")
;; (add-to-list custom-comment-tag-alist-warning "WARNING")
;; (activate-highlight-custom-comment-tags)

;; TODO:
;; Make the two variables `custom-comment-tag-alist-comment' and `custom-comment-tag-alist-warning' both empty to start off with, and let the user populate them as they see fit.
;; Make the variable `custom-comment-tag-mode-hooks' empty to start off with, and let the user populate it as they see fit.
;; Create new variables `custom-comment-tag-colour-comment' and `custom-comment-tag-colour-warning' which set the font lock face colour for their respective tags (so this "mode" might work as intended a non-Zenburn theme).
;; In the function `insert-custom-comment-tag', make sure we have available to use the `ido-completing-read' functionality.

;;; COMMENT: highlight custom comment tags
(defvar custom-comment-tag-alist-comment '("AUTHOR" "COMMENT" "FILE" "IMPORTANT" "NOTE" "TODO") "Available custom comment tags.")
(defvar custom-comment-tag-alist-warning '("BUG" "DEBUG" "ERROR" "FIX" "WARNING" "TEST") "Available custom warning tags.")
(defvar custom-comment-tag-alist (append custom-comment-tag-alist-comment custom-comment-tag-alist-warning) "Available custom tags.")

;; NOTE: there's some obvious commonality between these two "sets"
(defvar font-lock-custom-comment-tag-face-comment 'font-lock-custom-comment-tag-face-comment "Face name to use for `custom-comment-tag-alist-comment' tags.")

(defvar font-lock-custom-comment-tag-face-warning 'font-lock-custom-comment-tag-face-warning "Face name to use for `custom-comment-tag-alist-warning' tags.")

(defface font-lock-custom-comment-tag-face-comment '((t (:foreground "SpringGreen"))) "Font lock face to highlight custom `custom-comment-tag-alist-comment' tags." :group 'font-lock-faces)

(defface font-lock-custom-comment-tag-face-warning '((t (:foreground "OrangeRed"))) "Font lock face to highlight custom `custom-comment-tag-alist-warning' tags." :group 'font-lock-faces)

(defvar custom-comment-tag-mode-hooks
  '(emacs-lisp-mode-hook lisp-mode-hook shell-script-mode-hook sh-mode-hook haskell-mode-hook scheme-mode-hook cc-mode-hook python-mode-hook)
  "Major modes which enable highlighting of custom comment tags.")

(defvar custom-comment-suppress-init-message nil "Suppress the printing of the initial activation message.")

(defun activate-highlight-custom-comment-tags (&rest junk)
  "Highlight custom comment tags in designated modes.
The custom comment \"tags\" are defined in the variable `custom-comment-tag-list'.
The \"designated\" modes are defined in the variable `custom-comment-tag-mode-hooks'."
  (setq temp-custom-comment-tag-alist-comment (concat "\\<" (regexp-opt custom-comment-tag-alist-comment) ":"))
  (setq temp-custom-comment-tag-alist-warning (concat "\\<" (regexp-opt custom-comment-tag-alist-warning) ":"))
  (mapc
   (lambda (mode-hook)
     (add-hook mode-hook
	       (lambda ()
		 (font-lock-add-keywords nil `((,temp-custom-comment-tag-alist-comment 0 font-lock-custom-comment-tag-face-comment t)))
		 (font-lock-add-keywords nil `((,temp-custom-comment-tag-alist-warning 0 font-lock-custom-comment-tag-face-warning t))))))
   custom-comment-tag-mode-hooks)
  (when (eq custom-comment-suppress-init-message nil)
    (message "Custom highlight tags activated.")))

(defun custom-comment-tag-regexp (&rest junk)
  "The \"optimised\" regular expresssion of the `custom-comment-tag-alist' list variable."
  (concat (regexp-opt custom-comment-tag-alist 'words) ":"))

(defun insert-custom-comment-tag (&rest junk)
  "Insert a custom comment tag (see: `custom-comment-tag-alist') in a source code file."
  (interactive)
  ;; (if (fboundp 'ido-completing-read) ;; NOTE: this is a check to make sure we have `ido-completing-read' available
  ;;     (message "ido-completing-read is available.")
  ;;   (message "ido-completing-read is not available."))
  (insert (concat "" (make-string 2 (aref comment-start 0)) " " (ido-completing-read "Insert comment tag: " custom-comment-tag-alist) ": ")))

(defun show-custom-comment-tag (&rest junk)
  "Show the custom comment tags (defined in the variable `custom-comment-tag-alist') in an outline-mode structure.
NOTE: This function depends on the multi-occur function `show-custom-structure'."
  (interactive)
  (show-custom-structure (custom-comment-tag-regexp)))

(provide 'custom-comments)

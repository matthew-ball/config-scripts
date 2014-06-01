;;; custom-comments.el --- Highlight user-defined comment strings in source code buffers

;; Copyright (C) 2013  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; URL:
;; Version: 1.0.0
;; Package-Requires:
;; Keyowrds: font-lock, comments

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

;; A (minor mode [not quite]) extension for GNU Emacs managing the highlighting
;; of custom comments in source code buffers.

;; NOTE: An example configuration would be:

;; (require 'custom-comments)

;; (custom-comment-create-new-tag "heading" '((t (:foreground "Blue" :weight bold))))
;; (custom-comment-create-new-tag "comment" '((t (:foreground "Green" :weight bold))))
;; (custom-comment-create-new-tag "warning" '((t (:foreground "Red" :weight bold))))

;; (add-tag-to-category "heading" "HEADING")
;; (add-tag-to-category "comment" "IMPORTANT")
;; (add-tag-to-category "warning" "WARNING")

;; (custom-comment-mode)

;;; Code:

;;; IMPORTANT: custom comment group
(defgroup custom-comment-group nil "Custom comments for `prog-mode' buffers."
  :group 'font-lock :group 'programming)

;;; IMPORTANT: highlight custom comment tags
(defvar custom-comment-tags-list '() "All custom tags.")

(defmacro custom-comment-create-new-tag (name face)
  "Create a new tag category NAME which uses FACE to discern it from the remaining buffer text."
  (let ((tag-name (intern (format "custom-comment-tags-%s" name)))
        (tag-face (intern (format "custom-comment-tags-%s-face" name))))
    `(progn
        (defvar ,tag-name nil "Category for custom tags.")
        (defvar ,tag-face (quote ,tag-face) "Face name to use for custom tags category.")
        (defface ,tag-face ,face "Font lock face to highlight custom tags category." :group (quote font-lock-faces))
        (add-to-list (quote custom-comment-tags-list) (quote ,tag-name)))))

;; TODO: this is unnecessarily a macro
(defmacro add-tag-to-category (category tag)
  "Add comment TAG to CATEGORY."
  (let ((tag-name (intern (format "custom-comment-tags-%s" category))))
    `(add-to-list (quote ,tag-name) (quote ,tag))))

;; TODO: find a better way to do this
(defun highlight-custom-comment-tags (&rest junk)
  "Highlight custom comment tags in for all major-modes which inherit from `prog-mode'."
  (mapc
   (lambda (entry)
     (let* ((face-str (concat "\\<" (regexp-opt (eval entry)) ":"))
            (face-var (intern (concat (symbol-name entry) "-face")))
            (face (list `(,face-str 0 ,face-var t))))
       (eval `(add-hook 'prog-mode-hook
			(lambda () (font-lock-add-keywords nil (quote ,face)))))))
   custom-comment-tags-list))

;; FIX: this doesn't work
;; NOTE: when turning the mode off remove all instances of `custom-comment-tags-*-face' from `font-lock-keywords' variable
;;;###autoload
;; (define-minor-mode custom-comment-highlight-mode
;;   "A minor-mode which toggles highlighting of custom comment strings in `prog-mode' buffers."
;;   :group 'custom-comment-group
;;   ;; the initial value
;;   :init-value nil
;;   ;; the indicator for the mode-line
;;   :lighter " cc"
;;   ;; the minor mode bindings
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c i") 'insert-custom-comment-tag)
;;             map)
;;   ;;(make-local-variable 'custom-comment-tags-list)
;;   (highlight-custom-comment-tags))

;;;###autoload
(defun insert-custom-comment-tag (&rest junk)
  "Insert a custom comment tag (see: `custom-comment-tags-list') in a source code file."
  (interactive)
  (insert
   (concat "" (make-string 2 (aref comment-start 0)) " "
	   (ido-completing-read "Insert comment tag: " (custom-comment-collect-tags) ": "))))

;;;###autoload
(defun search-string-under-point (&rest junk)
  "Search for the string under point across all buffers with the same extension."
  (interactive)
  (show-custom-structure (thing-at-point 'word)))

;;;###autoload
(defun search-string (string &rest junk) ;; TODO: accept regexps
  "Show the outline of all file containings the text STRING in a single directory."
  (interactive "sEnter search string: ")
  (show-custom-structure string))

;;;###autoload
(defun show-comment-structure (&rest junk)
  "Show the outline comment structure."
  (interactive)
  (show-custom-structure (custom-comment-tag-regexp)))

;; NOTE: utility functions
(defun custom-comment-collect-tags (&rest junk)
  "Collect all custom comment tags together in a single list."
  (apply 'append (mapcar (lambda (entry) (eval entry)) custom-comment-tags-list)))

(defun custom-comment-tag-regexp (&rest junk)
  "The \"optimised\" regular expresssion of the `custom-comment-tags-list'."
  (concat (regexp-opt (custom-comment-collect-tags) 'words) ":"))

(defun show-custom-structure (string &rest junk)
  "Show the outline structure of all files matching the same extension in a directory."
  (multi-occur-in-matching-buffers (file-name-extension (buffer-file-name)) string)
  ;; (occur-mode-clean-buffer) ;; NOTE: clean up the occur-mode buffer
  (other-window 1))

;; SOURCE: unknown
(defun occur-mode-clean-buffer ()
  "Removes all commentary from the `*Occur*' buffer, leaving the unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
	(set-buffer (get-buffer "*Occur*"))
	(goto-char (point-min))
	(toggle-read-only 0)
	(if (looking-at "^[0-9]+ lines matching \"")
	    (kill-line 1))
	(while (re-search-forward "^[ \t]*[0-9]+:" (point-max) t)
	  (replace-match "")
	  (forward-line 1)))
    (message "There is no buffer named \"*Occur*\".")))

(provide 'custom-comments)
;;; custom-comments.el ends here

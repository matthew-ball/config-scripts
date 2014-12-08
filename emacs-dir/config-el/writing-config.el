;;; writing-config.el --- Configuration for writing-related settings/options

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

;; Configuration for writing-related settings and options.

;;; Code:

(defgroup user-writing nil "Custom writing variables." :group 'user-variables)

;;; IMPORTANT: bibtex
;; SOURCE: `http://www.emacswiki.org/emacs/BibTeX'

;;; IMPORTANT: reftex
;; SOURCE:
;; (autoload 'reftex-mode "reftex" "RefTeX minor mode for GNU Emacs." t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX minor mode for GNU Emacs." t)
;; (autoload 'reftex-citation "reftex-cite" "RefTeX inert citation." nil)
;; (autoload 'reftex-index-phrase-mode "reftex-index" "RefTeX phrase mode." t)

;; (after "reftex"
;;   (setq reftex-enable-partial-scans t ;; NOTE: make reftex faster
;; 	reftex-save-parse-info t ;; NOTE: save the information gathered while reading a file
;; 	reftex-use-multiple-selection-buffers t ;; NOTE: use a separate buffer for each selection type
;; 	reftex-default-bibliography `("default.bib" ,(expand-file-name (concat user-documents-directory "Papers/papers.bib")))
;; 	reftex-cite-prompt-optional-args nil
;; 	reftex-cite-cleanup-optional-args t
;; 	reftex-extra-bindings t))

;; (defun org-mode-reftex-setup ()
;;   "Set up `reftex' integration with `org-mode'."
;;   (unless (fboundp 'reftex-mode)
;;     (load-library "reftex"))
;;   (and (buffer-file-name) (file-exists-p (buffer-file-name)) (reftex-parse-all))
;;   (define-key org-mode-map (kbd "C-c )") #'reftex-citation))

;;(add-hook 'org-mode-hook #'org-mode-reftex-setup)

;;; IMPORTANT: the emacs bibliography manager
;; SOURCE: `http://ebib.sourceforge.net/'
(autoload 'ebib "ebib" "A BibTeX database manager for GNU Emacs." t)

(after "ebib"
  ;; TODO: investigate @string clauses and abbreviations for common journals
  ;; TODO: create `philosophy.bib' `mathematics.bib' `linguistics.bib' `computer-science.bib' etc
  (setq ebib-preload-bib-files (list (format "%su4537508.bib" user-university-directory) ;; NOTE: university courses
				     (format "%sPapers/papers.bib" user-documents-directory) ;; NOTE: general papers
				     ;; "/home/chu/Documents/Papers/papers.bib"
				     ;; "/home/chu/Documents/ANU/u4537508.bib"
				     ;; "/home/chu/Documents/Papers/philosophy.bib"
				     ;; "/home/chu/Documents/Papers/mathematics.bib"
				     ;; "/home/chu/Documents/Papers/linguistics.bib"
				     ;; "/home/chu/Documents/Papers/computer-science.bib"
				     )
	ebib-keywords-list (list "philosophy"
				 "mathematics"
				 "logic"
				 "computer science"
				 "linguistics"
				 "miscellaneous")
	ebib-autogenerate-keys t ;; NOTE: generate unique keys automatically
	ebib-file-search-dirs (list (format "%s" user-home-directory)
				    (format "%sPapers/" user-documents-directory))) ;; NOTE: directories to search when viewing external files

  (setcdr (assoc "pdf" ebib-file-associations) "epdfview"))

;;; IMPORTANT: custom inserts
(defun surrounded-by-p (char)
  "Returns t if word is surrounded by given char."
  (save-excursion
    (and (forward-word -1)
         (equal char (char-before))
         (forward-word 1)
         (equal char (char-after)))))

(defun surround-word (char &optional force)
  "Surrounds word with given character.  If force is nil and word is already surrounded by given character removes them."
  (save-excursion
    (if (not (surrounded-by-p char))
        (progn
          (forward-word 1)
          (insert char)
          (forward-word -1)
          (insert char)
          t)
      (forward-word 1)
      (delete-char 1)
      (forward-word -1)
      (delete-char -1)
      nil)))

(defmacro propertize-word (property character)
  "Define functions for propertizing words with PROPERTY using CHARACTER."
  `(defun ,(intern (format "%s-word" property)) (&optional force)
     ,(format "Insert a %s character (%c) before (and after) an input string." property character)
     (interactive "p")
     (surround-word ,character force)))

(propertize-word bold ?*) ;; => (bold-word)
(propertize-word italic ?/) ;; => (italic-word)
(propertize-word underline ?_) ;; => (underline-word)
(propertize-word verbatim ?~) ;; => (verbatim-word)
(propertize-word teletype ?=) ;; => (teletype-word)

;;; IMPORTANT: customisations
(defun turn-on-custom-org-key-bindings ()
  "Activate custom `org-mode' key-bindings."
  (define-key org-mode-map (kbd "C-M-j") #'org-insert-heading) ;; NOTE: M-RET inserts a new heading
  (define-key org-mode-map (kbd "C-c b") #'bold-word)
  (define-key org-mode-map (kbd "C-c i") #'italic-word)
  (define-key org-mode-map (kbd "C-c u") #'underline-word)
  (define-key org-mode-map (kbd "C-c v") #'verbatim-word)
  (define-key org-mode-map (kbd "C-c t") #'teletype-word))

(defun turn-on-custom-org ()
  "Activate custom `org-mode' functionality."
  (org-toggle-pretty-entities) ;; NOTE: toggle UTF-8 unicode symbols
  (imenu-add-to-menubar "Imenu")
  (turn-on-custom-org-key-bindings)) ;; NOTE: enable custom org-mode bindings

(defun turn-on-hl-mode ()
  ""
  (hl-line-mode t))

(add-hook 'org-mode-hook #'turn-on-custom-org)
(add-hook 'org-agenda-mode-hook #'turn-on-hl-mode #'append)

(provide 'writing-config)
;;; writing-config.el ends here

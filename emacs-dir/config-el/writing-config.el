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

;;; IMPORTANT: diary and calendar mode
;; SOURCE: `http://www.emacswiki.org/emacs/DiaryMode'
;; SOURCE: `http://www.emacswiki.org/emacs/CalendarMode'
;; TODO: move this to `general-config.el'
;;(autoload 'calendar "calendar" "Keep a personal diary with GNU Emacs." t)
;; (require 'calendar)

;; (after "calendar"
;;   (setq ;;calendar-view-diary-initially-flag t
;; 	;;calendar-view-holidays-initially-flag t
;; 	;;calendar-mark-diary-entries-flag t
;; 	calendar-mark-holidays-flag t
;; 	;;diary-file (expand-file-name (concat user-organisation-directory "journal.org"))
;; 	number-of-diary-entries 14)

;;   ;; (add-hook 'diary-display-hook #'fancy-diary-display)
;;   (add-hook 'today-visible-calendar-hook #'calendar-mark-today))

;;; IMPORTANT: org-mode configuration
;; TODO: move to `user-config.el'
;; SOURCE: `http://emacswiki.org/emacs/OrgMode'
;; SOURCE: `http://lists.gnu.org/archive/html/emacs-orgmode/2011-04/msg00761.html'
(autoload 'org-install "org-exp" "Organise tasks with org-mode." t)
(autoload 'org-special-blocks "org-special-blocks" "Render blocks of code with org-mode." t)
(autoload 'org-bbdb-open "org-bbdb" "The big-brother database and org-mode." t)

(after "org"
  ;; (require 'ox-odt)
  ;; (require 'ox-latex)
  ;; (require 'org-agenda)
  ;; (require 'org-capture)
  ;; (require 'org-indent)
  
  ;; TODO: split these up - move them earlier in the config (if possible)
  (setq org-return-follows-link t ;; NOTE: use RETURN to follow links
	org-completion-use-ido t ;; NOTE: enable `ido-mode' for target (buffer) completion
	org-outline-path-complete-in-steps t ;; NOTE: targets complete in steps - 1. filename 2. <tab> next level of targets
	org-footnote-auto-adjust t ;; NOTE: automatically handle footnotes
	org-hide-emphasis-markers t ;; NOTE: hide emphasis markers in org-mode buffers
	;; org-fontify-done-headline t
	;; org-read-date-display-live nil ;; NOTE: disable the live date-display
	;; org-insert-mode-line-in-empty-file t
	;; appearance
	;; org-odd-levels-only t ;; NOTE: use only odd levels for an outline
	;; org-hide-leading-stars t ;; NOTE: hide leading stars in a headline
	;; org-treat-S-cursor-todo-selection-as-state-change nil ;; NOTE: ignore processing
	;; org-use-property-inheritance t ;; NOTE: children tasks inherit properties from their parent
	org-support-shift-select 1 ;; NOTE: enable using SHIFT + ARROW keys to highlight text
	;; log
	org-log-done 'time ;; NOTE: capture a timestamp for when a task changes state
	org-log-into-drawer 'LOGBOOK ;; NOTE: log changes in the LOGBOOK drawer
	;; custom tags
	org-tags-column -90
	org-tag-alist '(("COMPUTER_SCIENCE" . ?c)
			("GENERAL"          . ?g)
			;; ("ASSIGNMENT"       . ?a)
			;; ("WEBSITE"          . ?w)
			;; ("PROJECT"          . ?p)
			("JOURNAL"          . ?j)
			("NOTES"            . ?n)
			("LINGUISTICS"      . ?l)
			("MATHEMATICS"      . ?m)
			("PROGRAMMING"      . ?P)
			("READING"          . ?r)
			("PHILOSOPHY"       . ?p)
			("TRAVEL"           . ?t)
			("WRITING"          . ?w)
			("UNIVERSITY"       . ?u))
	;; scheduling
	org-deadline-warning-days 7
	org-timeline-show-empty-dates t
	org-use-tag-inheritance nil ;; NOTE: disable tag inheritance
	org-use-fast-todo-selection t ;; NOTE: enable fast task state switching
	;; notes
	org-directory (expand-file-name user-organisation-directory) ;; NOTE: default directory for org mode
	org-default-notes-file (expand-file-name user-org-notes-file) ;; NOTE: file for quick notes
	;; `org-archive'
	org-archive-location (concat (expand-file-name user-org-archive-file) "::* Archives") ;; NOTE: archiving items
	;; `org-refile'
	org-refile-target '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)) ;; NOTE: any file contributing (agenda); up to 5 levels deep
	org-refile-use-outline-path 'file ;; NOTE: targets start with the file name - allows creating level 1 tasks
	org-refile-allow-creating-parent-nodes 'confirm))  ;; NOTE: allow refile to create parent tasks with confirmation

;;; IMPORTANT: `org-link'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Handling-links.html'
(after "org-link"
  (org-add-link-type "ebib" 'ebib)

  ;; TODO: add more citation types to ebib
  (org-add-link-type "cite" 'ebib
		     (lambda (path desc format)
		       (cond ((eq format 'latex)
			      (format "\\cite{%s}" path)))))

  ;; TODO: create an ebib entry which links to ERC logs
  ;; NOTE: this would require `erc-log-mode' from MELPA

  ;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Link-abbreviations.html'
  (setq org-link-abbrev-alist '(("google" . "http://www.google.com/search?q=")
				("wikipedia" . "http://www.en.wikipedia.org/wiki/Special:Search/"))))

;;; IMPORTANT: org-agenda
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html'
(autoload 'org-agenda "org-agenda" "View an agenda of tasks in `org-mode'." t)

(after "org-agenda"
  (setq org-agenda-include-diary t ;; NOTE: include entries from the emacs diary
	org-agenda-skip-scheduled-if-done t ;; NOTE: ...
	org-agenda-inhibit-startup t
	org-agenda-skip-deadline-if-done t ;; NOTE: ...
	org-agenda-skip-additional-timestamps-same-entry nil ;; NOTE: don't skip multiple entries per day
	org-agenda-dim-blocked-tasks nil ;; NOTE: do not dim blocked tasks
	org-agenda-span 'month ;; NOTE: show a month of agendas
	org-agenda-files `(,(expand-file-name user-org-journal-file)
			   ,(expand-file-name user-org-notes-file)
			   ;; ,(expand-file-name user-org-projects-file)
			   ,(expand-file-name user-org-university-file)))

  ;; TODO: create something similar to the 'q' version (i.e. include a section on Tasks by Context),
  (setq org-agenda-custom-commands ;; NOTE: custom commands for `org-agenda'
	'(("A" "All" ((agenda "Weekly Agenda" ((org-agenda-ndays 7) ;; NOTE: overview of tasks
					       (org-agenda-start-on-weekday nil) ;; NOTE: calendar begins today
					       (org-agenda-repeating-timestamp-show-all t)
					       (org-agenda-entry-types '(:timestamp :sexp))))
		      (agenda "Daily Agenda" ((org-agenda-ndays 1) ;; NOTE: daily agenda
					      (org-deadline-warning-days 7) ;; NOTE: seven day warning for deadlines
					      (org-agenda-todo-keyword-format "[ ]")
					      (org-agenda-scheduled-leaders '("" ""))
					      (org-agenda-prefix-format "%t%s")))
		      (todo "TODO" ;; NOTE: todos searched by context
			    ((org-agenda-prefix-format "- ")
			     (org-agenda-sorting-strategy '(tag-up priority-down))
			     (org-agenda-todo-keyword-format "")
			     (org-agenda-overriding-header "All Tasks"))))
	   "ALL" ((org-agenda-compact-blocks t) (org-agenda-remove-tags t))) ;; NOTE: `ALL' TODOs
	  ("u" "University" ((org-agenda-list nil nil 1) (tags "UNIVERSITY") (tags-todo "ASSIGNMENT")) "UNIVERSITY") ;; NOTE: `UNIVERSITY' tasks
	  ("p" "Project" ((tags-todo "PROJECT") (tags-todo "TRAVEL") (tags-todo "GENERAL") (tags-todo "WRITING") (tags-todo "UNIVERSITY") (tags-todo "NOTES")) "PROJECTS") ;; NOTE: `PROJECT' tasks
	  ("j" "Journal" ((tags "JOURNAL")) "JOURNAL")
	  ("w" "Writing" ((tags "WRITING")) "WRITING")
	  ("r" "Reading" ((tags "READING") (tags "WEBSITE")) "READING"))))

;;; IMPORTANT: org-capture
;; SOURCE: `http://orgmode.org/manual/Capture.html'
(autoload 'org-capture "org-capture" "Quickly capture tasks and notes with `org-mode'." t)

;;; IMPORTANT: capture templates (WARNING: do not use 'C' or 'q' characters for binding)
;; TODO: custom `org-capture' templates
;;       - school
;;       - notes
;;       - journal
(after "org-capture"
  (setq org-capture-templates
	'(;; ("L" "Library" entry (file+headline (expand-file-name user-org-university-file) "Library")
	  ;;  "** %^{Title} %?%^g\n - Borrowed %^t\n - Due: %^t\n\n" :empty-lines 1 :immediate-finish 1)
	  ;; ("U" "University Course" entry (file+headline (expand-file-name user-org-university-file) "Courses")
	  ;;  "%(add-course)" :empty-lines 1 :immediate-finish 1)
	  ;; ("A" "Assignment" plain (file+function (expand-file-name user-org-university-file) course-code)
	  ;;  "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	  ;; ("c" "Contacts" plain (file+headline (expand-file-name user-org-contacts-file) "Contacts")
	  ;;  "[[bbdb:%^{Name}][%^{Name}]] %?%^g" :empty-lines 1 :immediate-finish 1)
	  ;; ("j" "Journal" entry (file+datetree (expand-file-name user-org-journal-file))
	  ;;  "* %U\n%?\n%i\n")
	  ("n" "Notes" entry (file+headline (expand-file-name user-org-notes-file) "Notes")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("g" "General" entry (file+headline (expand-file-name user-org-notes-file) "General")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("p" "Philosophy" entry (file+headline (expand-file-name user-org-notes-file) "Philosophy")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("l" "Linguistics" entry (file+headline (expand-file-name user-org-notes-file) "Linguistics")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("m" "Mathematics" entry (file+headline (expand-file-name user-org-notes-file) "Mathematics")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("c" "Computer Science" entry (file+headline (expand-file-name user-org-notes-file) "Computer Science")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("P" "Programming" entry (file+headline (expand-file-name user-org-notes-file) "Programming")
	   "** TODO ~%^{Name}~ - %^{Description} [0%] %^g
- [ ] Research [0%]
- [ ] Design [0%]
- [ ] Implementation [0%]" :empty-lines 1 :immediate-finish 1)
	  ("r" "Reading" entry (file+headline (expand-file-name user-org-notes-file) "Reading")
	   "** %^{Title}%?%^g\n" :empty-lines 1 :immediate-finish 1)
	  ("w" "Writing" entry (file+headline (expand-file-name user-org-notes-file) "Writing")
	   "** %^{Title}%?%^g\n" :empty-lines 1 :immediate-finish 1))))

;;; IMPORTANT: org-babel
;; SOURCE: `http://orgmode.org/worg/org-contrib/babel/intro.html'
(autoload 'org-babel-load-file "ob-tangle" "Interact with programming languages in `org-mode'." t)

(after "ob-tangle"
  ;; (require 'ob-lisp)

  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							   (lisp . t)
							   (maxima . t)
							   (scheme . t)
							   (haskell . t)
							   (latex . t)
							   (R . nil)
							   (gnuplot . nil)
							   (perl . nil)
							   (python . nil)
							   (ruby . nil)
							   (screen . t)
							   (sh . t)))

  (setq org-confirm-babel-evaluate nil ;; NOTE: no confirmation before evaluating code
	org-src-fontify-natively t ;; NOTE: enable fontify in source code blocks
	org-src-tab-acts-natively t)) ;; NOTE: tab works properly

;;; IMPORTANT: `org-entities'
;; SOURCE: `http://orgmode.org/manual/Special-symbols.html'
(autoload 'org-entities "org-entities" "Enable unicode support for `org-mode'." t)

(after "org-entities"
  (require 'org-entities-user+)

  (defun org-insert-user-entity ()
    "Insert symbol from `org-entities-user' list."
    (interactive)
    (let ((entity (ido-completing-read "Insert entity: " (mapcar #'(lambda (element) (car element)) org-entities-user))))
      (insert (format "\\%s" entity))))

  (defun org-insert-entity ()
    "Insert symbol from `org-entities' list."
    (interactive)
    (let ((entity
	   (ido-completing-read "Insert entity: "
				(remove-if #'null (mapcar #'(lambda (element)
							      (unless (stringp element)
								(format "%s" (car element))))
							  org-entities)))))
      (insert (format "\\%s" entity)))))

;;; IMPORTANT: org-export
;; SOURCE: `http://orgmode.org/manual/Exporting.html'
;; (autoload 'org-export-dispatch "ox" "Export files in `org-mode'." t)

;; (after "ox"
;;   (setq org-export-latex-default-class "article"
;; 	org-export-with-toc nil ;; NOTE: turn off `org-mode' exporting a table of contents
;; 	org-export-run-in-background t ;; NOTE: run `org-export' tasks in the background
;; 	org-export-with-tasks nil ;; NOTE: turn off `org-mode' exporting tasks
;; 	org-export-with-todo-keywords nil)) ;; NOTE: turn off `org-mode' exporting of TODO keywords

;; IMPORTANT: `org-indent'
;; SOURCE: ...
;; (after "org-indent"
;;   (setq org-indent-indentation-per-level 1 ;; NOTE: two indents per level
;; 	org-startup-indented t)) ;; NOTE: indent text in org documents (WARNING: can crash emacs)))

;;; IMPORTANT: blogging from emacs (org publishing)
;; SOURCE: `http://bzg.fr/blogging-from-emacs.html'
;; (after "ox-publish"
;;   (setq org-publish-project-alist
;; 	'(("blog"
;; 	   :base-directory "~/Documents/blog/"
;; 	   :html-extension "html"
;; 	   :base-extension "org"
;; 	   :publishing-directory "~/Public/html/"
;; 	   :publishing-function (org-html-publish-to-html)
;; 	   :html-preamble nil
;; 	   :html-postamble nil))))

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

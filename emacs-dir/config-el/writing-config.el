;;; writing-config.el --- Configuration for writing-related settings/options

;; Copyright (C) 2013  Matthew Ball

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

;;; IMPORTANT: insert date and time
;; SOURCE: `http://www.emacswiki.org/emacs/InsertDate'
(defun insert-date (format)
  "Wrapper around `format-time-string'."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%c")))

;;; IMPORTANT: deft
;; SOURCE: `http://jblevins.org/projects/deft/'
(autoload 'deft "deft" "Note taking with deft." t)

(after "deft"
  (setq deft-extension "org"
	deft-text-mode 'org-mode
	deft-directory (format "%s.deft/" user-organisation-directory)))

;;; IMPORTANT: diary and calendar mode
;; SOURCE: `http://www.emacswiki.org/emacs/DiaryMode'
;; SOURCE: `http://www.emacswiki.org/emacs/CalendarMode'
;; (autoload 'calendar "calendar" "Keep a personal diary with GNU Emacs." t)

;; (setq view-diary-entries-initially t
;;       mark-diary-entries-in-calendar t
;;       diary-file "/home/chu/Documents/Organisation/diary"
;;       number-of-diary-entries 7)

;; (eval-after-load "calendar"
;;   (add-hook 'diary-display-hook 'fancy-diary-display)
;;   (add-hook 'today-visible-calendar-hook 'calendar-mark-today))

;;; IMPORTANT: flyspell
;; SOURCE: `http://www.emacswiki.org/emacs/FlySpell'
(autoload 'flyspell-mode "flyspell" "On-the-fly spell checking" t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
;;(autoload 'flyspell-prog-mode "flyspell" "On-the-fly spell checking." t)
;;(autoload 'tex-mode-flyspell-verify "flyspell" "..." t)

(after "flyspell"
  (add-hook 'text-mode-hook 'turn-on-flyspell)) ;; NOTE: turn on automatic spell check if in a `text-mode'

;;; IMPORTANT: ispell
(setq ispell-program-name "aspell" ;; NOTE: use aspell for automatic spelling
      ispell-parser 'tex
      ispell-extra-args '("--sug-mode=ultra"))

;;; IMPORTANT: dictem
;; SOURCE: ...
(autoload 'dictem-run-search "dictem" "" t)

(after "dictem"
  (setq dictem-server "dict.org"
	dictem-port "2628"
	dictem-exclude-databases '("ger-" "-ger" "fra-" "-fra")
	dictem-default-database "*"
	dictem-default-strategy ".")

  ;; TODO: move to key-bindings!!!
  (defconst dictem-prefix-key (kbd "C-c d") "Key map for `dictem'.")
  (defvar dictem-map (lookup-key global-map dictem-prefix-key) "...")

  (unless (keymapp dictem-map)
    (setq dictem-map (make-sparse-keymap)))

  (define-key global-map dictem-prefix-key dictem-map)
  (define-key dictem-map (kbd "s") 'dictem-run-search) ;; NOTE: SEARCH = MATCH + DEFINE : ask for word, database and search strategy and show definitions found
  (define-key dictem-map (kbd "m") 'dictem-run-match) ;; NOTE: MATCH : ask for word, database and search strategy and show matches found
  (define-key dictem-map (kbd "d") 'dictem-run-define);; NOTE: DEFINE : ask for word and database name and show definitions found
  (define-key dictem-map (kbd "C-c M-r") 'dictem-run-show-server) ;; NOTE: SHOW SERVER : show information about DICT server
  (define-key dictem-map (kbd "C-c M-i") 'dictem-run-show-info) ;; NOTE: SHOW INFO : show information about the database
  (define-key dictem-map (kbd "C-c M-b") 'dictem-run-show-databases) ;; NOTE: SHOW DB : show a list of databases provided by DICT server

  ;; TODO: ...
  ;; (add-hook 'c-mode-common-hook
  ;; 	  '(lambda ()
  ;; 	     (interactive)
  ;; 	     (make-local-variable 'dictem-default-database)
  ;; 	     (setq dictem-default-database "man")))

  ;; the code above sets default database to "man" in C buffers

  (dictem-initialize)

  ;; NOTE: for creating hyperlinks on database names and found matches (click on them with mouse-2)
  (add-hook 'dictem-postprocess-match-hook 'dictem-postprocess-match)

  ;; NOTE: for highlighting the separator between the definitions found, this also creates hyperlink on database names
  (add-hook 'dictem-postprocess-definition-hook 'dictem-postprocess-definition-separator)

  ;; NOTE: for creating hyperlinks in `dictem' buffer that contains definitions
  (add-hook 'dictem-postprocess-definition-hook 'dictem-postprocess-definition-hyperlinks)

  ;; NOTE: for creating hyperlinks in dictem buffer that contains information about a database
  (add-hook 'dictem-postprocess-show-info-hook 'dictem-postprocess-definition-hyperlinks)

  ;; NOTE: "virtual" dictionary
  (setq dictem-user-databases-alist '(("_en-en"  . ("foldoc" "gcide" "wn"))
				      ("en-en" . ("dict://dict.org:2628/english")) 
				      ("_unidoc" . ("susv3" "man" "info" "howto" "rfc"))))

  ;; NOTE: ...
  (setq dictem-use-user-databases-only t)

  ;; NOTE: all functions from dictem-postprocess-each-definition-hook will be run for each definition which in turn will be narrowed
  ;; NOTE: current database name is kept in dictem-current-dbname variable
  ;; NOTE: the following code demonstrates how to highlight SUSV3 and ROFF definitions
  (add-hook 'dictem-postprocess-definition-hook 'dictem-postprocess-each-definition)

  ;; NOTE: function for highlighting definition from the database "susv3"
  ;; (defun dictem-highlight-susv3-definition ()
  ;;   (cond ((string= "susv3" dictem-current-dbname)
  ;; 	 (goto-char (point-min))
  ;; 	 (while (search-forward-regexp
  ;; 		 "^ *[QWERTYUIOPASDFGHJKLZXCVBNM ]+$" nil t)
  ;; 	   (put-text-property
  ;; 	    (match-beginning 0) (match-end 0) 'face 'bold)))))

  ;; ;; NOTE: function to show roff-formatted text from the database "man"
  ;; (require 'woman)

  ;; (defun dictem-highlight-man-definition ()
  ;;   (cond ((string= "man" dictem-current-dbname)
  ;; 	 (goto-char (point-min))
  ;; 	 (while (search-forward-regexp "^  " nil t)
  ;; 	   (replace-match ""))
  ;; 	 (goto-char (point-min))
  ;; 	 (forward-line 2)
  ;; 	 (woman-decode-region (point) (point-max)))))

  ;; (add-hook 'dictem-postprocess-each-definition-hook 'dictem-highlight-susv3-definition)
  ;; (add-hook 'dictem-postprocess-each-definition-hook 'dictem-highlight-man-definition)
  )

;;; IMPORTANT: thesaurus
;; SOURCE: `http://emacswiki.org/emacs/thesaurus.el'
(autoload 'thesaurus-choose-synonym-and-replace "thesaurus" "Choose and replace a word with it's synonym." t)

(after "thesaurus"
  (setq thesaurus-bhl-api-key "8c5a079b300d16a5bb89246322b1bea6"))  ;; NOTE: from registration

;;; IMPORTANT: bibtex
;; SOURCE: `http://www.emacswiki.org/emacs/BibTeX'

;;; IMPORTANT: reftex
;; SOURCE:
(autoload 'reftex-mode "reftex" "RefTeX minor mode for GNU Emacs." t)
(autoload 'turn-on-reftex "reftex" "RefTeX minor mode for GNU Emacs." t)
(autoload 'reftex-citation "reftex-cite" "RefTeX inert citation." nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "RefTeX phrase mode." t)

(after "reftex"
  ;; IMPORTANT: reftex formats (for biblatex)
  ;; (setq reftex-cite-format
  ;;       '((?c . "\\cite[]{%l}")
  ;;         (?t . "\\textcite{%l}")
  ;;         (?a . "\\autocite[]{%l}")
  ;;         (?p . "\\parencite{%l}")
  ;;         (?f . "\\footcite[][]{%l}")
  ;;         (?F . "\\fullcite[]{%l}")
  ;;         (?x . "[]{%l}")
  ;;         (?X . "{%l}")))

  ;; (setq font-latex-match-reference-keywords
  ;;       '(("cite" "[{")
  ;;         ("cites" "[{}]")
  ;;         ("footcite" "[{")
  ;;         ("footcites" "[{")
  ;;         ("parencite" "[{")
  ;;         ("textcite" "[{")
  ;;         ("fullcite" "[{")
  ;;         ("citetitle" "[{")
  ;;         ("citetitles" "[{")
  ;;         ("headlessfullcite" "[{")))

  (setq reftex-enable-partial-scans t ;; make reftex faster
	reftex-save-parse-info t ;; save the information gathered while reading a file
	reftex-use-multiple-selection-buffers t ;; use a separate buffer for each selection type
	reftex-default-bibliography '("default.bib"
				      "/home/chu/Documents/Papers/papers.bib"
				      ) ;; default bibliography file(s)
	reftex-cite-prompt-optional-args nil
	reftex-cite-cleanup-optional-args t
	reftex-extra-bindings t ;; enable extra reftex bindings
	))

(defun org-mode-reftex-setup ()
  "Set up `reftex' integration with `org-mode'."
  (unless (fboundp 'reftex-mode)
    (load-library "reftex"))
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

;;(add-hook 'org-mode-hook 'org-mode-reftex-setup)

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

  (setcdr (assoc "pdf" ebib-file-associations) "evince"))

;; Ebib Entry: [[ebib:horwich1996][Horwich (1996)]]
;; Citation Entry: [[cite:horwich1996][Horwich (1996)]]

;; IMPORTANT: some personal ebib stuff
(defun ebib-export-directory (extension directory &rest junk)
  "Generates a BibTeX entry for all files with file-extension EXTENSION in directory DIRECTORY.

NOTE: This requires that each file in DIRECTORY be named according to \"<title>.EXTENSION\"."
  (mapc #'(lambda (file)
            ;; TODO: this needs to have a check whether `file' is already known, and if so, skip
	    (let ((title (replace-regexp-in-string "-" " " (file-name-nondirectory (file-name-sans-extension file))))
		  (author "")
		  (year "")
		  (tags nil))
	      (when (and (file-readable-p file) (not (file-directory-p file)))
		(insert
		 (format "@article{%s,\n	author = {%s},\n	title  = {%s},\n	year   = {%s},\n	file   = {%s}\n}\n\n"
			 (file-name-nondirectory (file-name-sans-extension file))
			 author
			 title
			 year
			 file)))))
	(directory-files directory t (concat "\." extension "$") t)))

(defun ebib-print-directory ()
  "Print the BibTeX entries from the TARGET-DIRECTORY variable, according to FILE-EXTENSION."
  (interactive)
  (let ((buffer-name "ebib-")
	(file-extension "pdf")
	(target-directory (format "%sPapers/PDFs/" user-documents-directory)))
    (switch-to-buffer "ebib-directory")
    (ebib-export-directory file-extension target-directory)))

;; TODO: can we do an `ido-completing-read' over a list of keys?
(defun org-insert-citation (key name)
  "Insert a BibTeX citation in an `org-mode' buffer, matching the `org-link' format."
  (interactive "sEnter key: \nsEnter name: ")
  (insert (format "[[%s][%s]]" key name)))

;;; IMPORTANT: org-mode configuration
;; SOURCE: `http://emacswiki.org/emacs/OrgMode'
;; SOURCE: `http://lists.gnu.org/archive/html/emacs-orgmode/2011-04/msg00761.html'
(autoload 'org-install "org-exp" "Organise tasks with org-mode." t)
(autoload 'org-special-blocks "org-special-blocks" "Render blocks of code with org-mode." t)
(autoload 'org-bbdb-open "org-bbdb" "The big-brother database and org-mode." t)

(after "org"
  (setq org-return-follows-link t ;; NOTE: use RETURN to follow links
	org-completion-use-ido t ;; NOTE: enable `ido-mode' for target (buffer) completion
	org-outline-path-complete-in-steps t ;; NOTE: targets complete in steps - 1. filename 2. <tab> next level of targets
	org-footnote-auto-adjust t ;; NOTE: automatically handle footnotes
	;; org-read-date-display-live nil ;; NOTE: disable the live date-display
	;; org-insert-mode-line-in-empty-file t
	;; --- appearance ---
	;; org-indent-mode t ;; NOTE: enable org indent mode
	;; org-indent-indentation-per-level 2 ;; NOTE: two indents per level
	;; org-startup-indented t ;; NOTE: indent text in org documents (WARNING: can crash emacs)
	;; org-odd-levels-only t ;; NOTE: use only odd levels for an outline
	;; org-hide-leading-stars t ;; NOTE: hide leading stars in a headline
	;; org-treat-S-cursor-todo-selection-as-state-change nil ;; NOTE: ignore processing
	;; org-use-property-inheritance t ;; NOTE: children tasks inherit properties from their parent
	org-support-shift-select 1 ;; NOTE: enable using SHIFT + ARROW keys to highlight text
	;; --- `org-log' ---
	org-log-done 'time ;; NOTE: capture a timestamp for when a task changes state
	org-log-into-drawer 'LOGBOOK ;; NOTE: log changes in the LOGBOOK drawer
	;; --- scheduling ---
	org-deadline-warning-days 7
	org-timeline-show-empty-dates t
	org-use-tag-inheritance nil ;; NOTE: disable tag inheritance
	org-use-fast-todo-selection t ;; NOTE: enable fast task state switching
	;; --- tags ---
	org-tags-column -80
	;; --- notes ---
	org-directory (expand-file-name user-organisation-directory) ;; NOTE: default directory for org mode
	org-default-notes-file (expand-file-name user-org-notes-file) ;; NOTE: file for quick notes
	;; --- `org-archive' ---
	org-archive-location (concat (expand-file-name user-org-archive-file) "::* Archives") ;; NOTE: archiving items
	;; --- `org-refile' ---
	org-refile-target '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)) ;; NOTE: any file contributing (agenda); up to 5 levels deep
	org-refile-use-outline-path 'file ;; NOTE: targets start with the file name - allows creating level 1 tasks
	org-refile-allow-creating-parent-nodes 'confirm) ;; NOTE: allow refile to create parent tasks with confirmation

  ;; TODO: re-order AND add new tags
  (setq org-tag-alist ;; NOTE: list of tags allowed in `org-mode' files
	'(("ASSIGNMENT"       . ?a)
	  ;; ("BOOKMARK"         . ?b)
	  ("COMPUTER SCIENCE" . ?c)
	  ("GENERAL"          . ?g)
	  ("HOLIDAY"          . ?h)
	  ("LIBRARY"          . ?l)
	  ;; ("IDEAS"            . ?i)
	  ("JOURNAL"          . ?j)
	  ("NOTES"            . ?n)
	  ("MATHEMATICS"      . ?m)
	  ("PROJECT"          . ?p)
	  ("PROGRAMMING"      . ?P)
	  ("READING"          . ?r)
	  ("PHILOSOPHY"       . ?s)
	  ("TRAVEL"           . ?t)
	  ("WRITING"          . ?T)
	  ("UNIVERSITY"       . ?u)
	  ("WEBSITE"          . ?w))))

;;; IMPORTANT: `org-link'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Handling-links.html'
(after "org"
  (org-add-link-type "ebib" 'ebib)

  ;; TODO: add more citation types to ebib
  (org-add-link-type "cite" 'ebib
		     (lambda (path desc format)
		       (cond ((eq format 'latex)
			      (format "\\cite{%s}" path)))))

  ;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Link-abbreviations.html'
  ;; (setq org-link-abbrev-alist '(("google"   . "http://www.google.com/search?q=")))
  )

;;; IMPORTANT: org-agenda
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html'
(after "org-agenda"
  (setq org-agenda-include-diary t ;; NOTE: include entries from the emacs diary
	org-agenda-skip-scheduled-if-done t ;; NOTE: ...
	org-agenda-skip-deadline-if-done t ;; NOTE: ...
	org-agenda-skip-additional-timestamps-same-entry nil ;; NOTE: don't skip multiple entries per day
	org-agenda-dim-blocked-tasks nil ;; NOTE: do not dim blocked tasks
	org-agenda-span 'month) ;; NOTE: show a month of agendas

  (setq org-agenda-files `(,(expand-file-name user-org-university-file)
			   ,(expand-file-name user-org-notes-file) ;; IMPORTANT: this is "journal.org" 
			   ,(expand-file-name user-org-projects-file)
			   ,(concat (expand-file-name user-organisation-directory) "home.org")
			   ,(concat (expand-file-name user-organisation-directory) "contacts.org")
			   ,(concat (expand-file-name user-organisation-directory) "birthday.org")
			   ,(concat (expand-file-name user-organisation-directory) "bookmarks.org")
			   ;; ,(concat (expand-file-name user-reading-directory) "readings.org")
			   ;; ,(concat (expand-file-name user-writing-directory) "writings.org")
			   ))

  ;; TODO: create something similar to the 'q' version (i.e. include a section on Tasks by Context),
  (setq org-agenda-custom-commands ;; NOTE: custom commands for `org-agenda'
	'(("A" "All" ((agenda "" ((org-agenda-ndays 7) ;; NOTE: overview of tasks
				  (org-agenda-start-on-weekday nil) ;; NOTE: calendar begins today
				  (org-agenda-repeating-timestamp-show-all t)
				  (org-agenda-entry-types '(:timestamp :sexp))))
		      (agenda "" ((org-agenda-ndays 1) ;; NOTE: daily agenda
				  (org-deadline-warning-days 7) ;; NOTE: seven day warning for deadlines
				  (org-agenda-todo-keyword-format "[ ]")
				  (org-agenda-scheduled-leaders '("" ""))
				  (org-agenda-prefix-format "%t%s")))
		      (todo "TODO" ;; NOTE: todos searched by context
			    ((org-agenda-prefix-format "[ ] %T: ")
			     (org-agenda-sorting-strategy '(tag-up priority-down))
			     (org-agenda-todo-keyword-format "")
			     (org-agenda-overriding-header "\n All Tasks \n"))))
	   "ALL"
	   ((org-agenda-compact-blocks t)
	    (org-agenda-remove-tags t))
	   ) ;; NOTE: `ALL' tasks
	  ("u" "University"
	   ((org-agenda-list nil nil 1)
	    (tags "UNIVERSITY")
	    (tags-todo "ASSIGNMENT"))
	   "UNIVERSITY") ;; NOTE: `UNIVERSITY' tasks
	  ("p" "Project"
	   ((tags-todo "PROJECT")
	    (tags-todo "TRAVEL")
	    (tags-todo "GENERAL")
	    (tags-todo "WRITING")
	    (tags-todo "UNIVERSITY")
	    (tags-todo "NOTES"))
	   "PROJECTS") ;; NOTE: `PROJECT' tasks
	  ;; ("j" "Journal"
	  ;;  ((tags "JOURNAL"))
	  ;;  "JOURNAL")
	  ("r" "Reading"
	   ((tags "READING")
	    (tags "WEBSITE"))
	   "READING") ;; NOTE: `READING' tasks
	  )))

;;; IMPORTANT: org-export
;; SOURCE: `http://orgmode.org/manual/Exporting.html'
(setq org-export-latex-default-class "paper"
      org-export-with-toc nil ;; NOTE: turn off `org-mode' exporting a table of contents
      org-export-run-in-background t ;; NOTE: run `org-export' tasks in the background
      org-export-with-tasks nil ;; NOTE: turn off `org-mode' exporting tasks
      org-export-with-todo-keywords nil) ;; NOTE: turn off `org-mode' exporting of TODO keywords

;;; IMPORTANT: org-capture
;; SOURCE: `http://orgmode.org/manual/Capture.html'
;; SOURCE: `http://orgmode.org/worg/org-contrib/org-protocol.html'
(autoload 'org-capture "org-capture" "..." t)
(autoload 'org-protocol "org-protocol" "Use `org-mode' with `emacsclient'." t)

;;; IMPORTANT: capture templates (WARNING: do not use 'C' or 'q' characters for binding)
(after "org-capture"
  (setq org-capture-templates
	'(("L" "Library" entry (file+headline (expand-file-name user-org-university-file) "Library")
	   "** %^{Title} %?%^g\n - Borrowed %^t\n - Due: %^t\n\n" :empty-lines 1 :immediate-finish 1)
	  ("U" "University Course" entry (file+headline (expand-file-name user-org-university-file) "Courses")
	   "%(add-course)" :empty-lines 1 :immediate-finish 1)
	  ("A" "Assignment" plain (file+function (expand-file-name user-org-university-file) course-code)
	   "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	  ("c" "Contacts" plain (file+headline (expand-file-name user-org-contacts-file) "Contacts")
	   "[[bbdb:%^{Name}][%^{Name}]] %?%^g" :empty-lines 1 :immediate-finish 1)
	  ;; ("J" "Journal" entry (file+datetree (expand-file-name user-org-notes-file))
	  ;;  "* %?\n Entered on %U\n  %i\n  %a")
	  ("N" "Note" entry (file+headline (expand-file-name user-org-notes-file) "Notes")
	   "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	  ("P" "Projects" plain (file+function (expand-file-name user-org-projects-file) project-capture)
	   "*** TODO %^{Description} %?%^g\n" :empty-lines 1 :immediate-finish 1)
	  ("R" "Reading" entry (file+headline (expand-file-name user-org-projects-file) "Reading"))
	  ("W" "Writing" entry (file+headline (expand-file-name user-org-projects-file) "Writing"))
	  ;; TODO: need to write `reading-capture' and `writing-capture' functions
	  )))

;;; IMPORTANT: school organisation
;; TODO: integrate with `ido-mode' somehow
;; WARNING: this is *terrible* - seriously consider a re-write !!!
;; TODO: can do some cool stuff here - consider writing a `org-school.el'
(defun add-course (&rest junk)
  "Capture a course via org-mode's `org-capture'."
  (let ((course-details ""))
    ;; NOTE: need to create link to course.org file
    (setq course-details (concat course-details "** " (read-from-minibuffer "Course Code: ") "%?%^g\n"
				 " TITLE: " (read-from-minibuffer "Course Title: ") "\n"
				 " LECTURER: " (read-from-minibuffer "Course Lecturer: ") "\n"
				 " LECTURES: \n + %^T :: " (read-from-minibuffer "Room Location: ") "\n"))
    ;; NOTE: this technically lies, y goes into the loop, anything else jumps to tutorial/seminar
    (while (string= (read-from-minibuffer "Add Lecture? (y/n): ") "y")
      (setq course-details (concat course-details " + %^T  :: " (read-from-minibuffer "Room Location: ") "\n")))
    ;; NOTE: this technically lies, t for "tutorial", any other input means "seminar"
    (concat course-details " " (if (string= (read-from-minibuffer "Tutorial or Seminar? (t/s): ") "t")
				   "TUTORIAL: "
				 "SEMINAR: ")
	    "\n + %^T :: " (read-from-minibuffer "Room Location: ") "\n")))

(defun file-path (&rest junk)
  "Return the path of a file."
  (buffer-file-name (get-buffer (car buffer-name-history))))

(defun dir-path (&rest junk)
  "Return the path of a directory."
  (car (rassq (get-buffer (car buffer-name-history)) dired-buffers)))

;; TODO: this needs to be looked at again
(defun course-code (&rest junk)
  "Search for a COURSE-CODE appearing in 'school.org' and if found move the point to that location."
  (interactive)
  (switch-to-buffer "school.org")
  (goto-char (point-min))
  (let ((str (read-from-minibuffer "Enter course code: ")))
    (when (search-forward (concat "** " str "\t") nil nil)
      (forward-line 9))))

(defun insert-lecture-template ()
  "..."
  (interactive)
  (insert (format "** Lecture %d: %s" 1 (format-time-string "%d/%m/%y"))))

;;; IMPORTANT: org-babel
;; SOURCE: `http://orgmode.org/worg/org-contrib/babel/intro.html'
(autoload 'org-babel-load-file "ob-tangle" "Interact with programming languages in `org-mode'." t)

(after "ob-tangle"
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 ;;(common-lisp . t)
				 (maxima . t)
				 (scheme . t)
				 (haskell . t)
				 (latex . t)
				 (R . nil)
				 (gnuplot . nil)
				 (perl . nil)
				 (python . nil)
				 (ruby . nil)
				 (screen . nil)
				 (sh . nil)))

  (setq org-confirm-babel-evaluate nil ;; NOTE: no confirmation before evaluating code
	org-src-fontify-natively t ;; NOTE: enable fontify in source code blocks
	org-src-tab-acts-natively t ;; NOTE: tab works properly
	))

;;; IMPORTANT: org-latex-export
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-latex-export.html'
(autoload 'org-latex "org-latex" "Render LaTeX with `org-mode'." t)
(autoload 'org-bibtex "org-bibtex" "Bibliographies with `org-mode'." t)

(after "org-bibtex"
  (require 'org-exp-bibtex))

(after "org-exp"
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))

  (add-to-list 'org-export-latex-classes
	       '("paper"
		 "\\documentclass[12pt,a4paper,oneside]{paper}
\\usepackage{amsfonts}
\\usepackage{amsthm}
\\setcounter{secnumdepth}{0}
[NO-DEFAULT-PACKAGES]
[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-export-latex-classes
	     '("book"
	       "\\documentclass[12pt,a4paper,oneside]{book}
\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{mathtools}
\\usepackage{makeidx}
\\usepackage{bussproofs}
[NO-DEFAULT-PACKAGES]
[EXTRA]"
	       ("\\part{%s}" . "\\part*{%s}")
	       ("\\chapter{%s}" . "\\chapter*{%s}")
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-export-latex-classes
	     '("assignment"
	       "\\documentclass[10pt,a4paper]{article}
\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage[cm]{fullpage}
\\usepackage{multicol}
\\usepackage{mdwlist}
\\usepackage{geometry}
\\usepackage{pgf}
\\usepackage{tikz}
\\usetikzlibrary{positioning,automata,arrows,shapes}
[NO-DEFAULT-PACKAGES]
[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
	     '("article"
	       "\\documentclass[12pt,a4paper]{article}
\\setcounter{secnumdepth}{0}
[NO-DEFAULT-PACKAGES]
[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
	     '("beamer"
	       "\\documentclass[10pt]{beamer}
[NO-DEFAULT-PACKAGES]
[EXTRA]"
	       org-beamer-sectioning))

;; IMPORTANT: enable latex source code highlighting
(setq org-export-latex-listings t) ;; NOTE: enable listings features
)

;; TODO: modify `org-export-latex-packages-alist' (i.e. include some LaTeX packages)
(after "org"
  (add-to-list 'org-export-latex-packages-alist '("" "listings")) ;; NOTE: listings package
  (add-to-list 'org-export-latex-packages-alist '("" "color")) ;; NOTE: colored source code
  (add-to-list 'org-export-latex-packages-alist '("" "tipa")) ;; NOTE: support for phonetic alphabet
  (add-to-list 'org-export-latex-packages-alist '("" "tipx")) ;; NOTE: support for phonetic alphabet
  ;;(add-to-list 'org-export-latex-packages-alist '("" "bussproofs")) ;; NOTE: for sequent style proofs
  (add-to-list 'org-export-latex-packages-alist '("" "amssymb")) ;; NOTE: mathematics symbols
  (add-to-list 'org-export-latex-packages-alist '("" "amsmath")) ;; NOTE: mathematics symbols
  (add-to-list 'org-export-latex-packages-alist '("" "hyperref"))) ;; NOTE: hyper-references

;;; IMPORTANT: `org-entities'
;; SOURCE: `http://orgmode.org/manual/Special-symbols.html'
(autoload 'org-entities "org-entities" "Enable unicode support for `org-mode'." t)

;; TODO: the fact these are all automatically put inside a math environment is a bit problematic
;; (add-to-list 'org-entities-user '("neg"            "\\neg" t "&not;" "[negation]" nil "¬"))
;; ;;(add-to-list 'org-entities-user '("iff"            "\\iff" t "&iff;" "[if and only if]" nil "↔"))
;; (add-to-list 'org-entities-user '("iff"            "\\iff" t "&iff;" "[if and only if]" nil "\leftrightarrow"))
;; (add-to-list 'org-entities-user '("top"            "\\top" t "&top;" "[top (true)]" nil "⊤"))
;; (add-to-list 'org-entities-user '("bot"            "\\bot" t "&bot;" "[bot (false)]" nil "⊥"))
;; (add-to-list 'org-entities-user '("therefore"      "\\therefore" t "&therefore;" "[therefore]" nil "∴"))
;; (add-to-list 'org-entities-user '("because"        "\\because" t "&because;" "[because]" nil "∵"))
;; (add-to-list 'org-entities-user '("derives"        "\\vdash" t "&vdash;" "[derives]" nil "⊢"))
;; (add-to-list 'org-entities-user '("notderives"     "\\not\\vdash" t "&notvdash" "[notderives]" nil "⊬"))
;; (add-to-list 'org-entities-user '("models"         "\\models" t "&mod;" "[models]" nil "⊨"))
;; (add-to-list 'org-entities-user '("notmodels"      "\\not\\models" t "&notmodels" "[notmodels]" nil "⊭"))
;; (add-to-list 'org-entities-user '("forces"         "\\Vdash" t "&forces" "[forces]" nil "⊩"))
;; (add-to-list 'org-entities-user '("notforces"      "\\not\\Vdash" t "&notforces" "[notforces]" nil "⊮"))
;; (add-to-list 'org-entities-user '("boxconditional" "\\boxconditional" t "&boxcond;" "[boxcond]" nil "□→"))
;; (add-to-list 'org-entities-user '("box"            "\\Box" t "&box;" "[box]" nil "□"))
;; (add-to-list 'org-entities-user '("diamond"        "\\Diamond" t "&diamond;" "[diamond]" nil "◇"))
;; (add-to-list 'org-entities-user '("cdots"          "\\cdots" t "&cdots;" "[center dots]" nil "⋯"))
;; (add-to-list 'org-entities-user '("ldots"          "\\ldots" t "&ldots;" "[line dots]" nil "…"))
;; (add-to-list 'org-entities-user '("reals"          "\\mathbb{R}" t "&reals;" "[real numbers]" nil "ℝ"))
;; (add-to-list 'org-entities-user '("integers"       "\\mathbb{Z}" t "&integers;" "[integers]" nil "ℤ"))
;; (add-to-list 'org-entities-user '("primes"         "\\mathbb{P}" t "&primes;" "[prime numbers]" nil "ℙ"))
;; (add-to-list 'org-entities-user '("naturals"       "\\mathbb{N}" t "&naturals;" "[natural numbers]" nil "ℕ"))
;; (add-to-list 'org-entities-user '("irrationals"    "\\mathbb{I}" t "&irrationals;" "[irrational numbers]" nil "𝕀"))
;; (add-to-list 'org-entities-user '("rationals"      "\\mathbb{Q}" t "&rationals;" "[rational numbers]" nil "ℚ"))
;; (add-to-list 'org-entities-user '("complex"        "\\mathbb{C}" t "&complex;" "[complex numbers]" nil "ℂ"))
;; TODO: ...
;; (add-to-list 'org-entities-user '("box" "\\Box" t "&box;" "[box]" nil "□"))
;; (add-to-list `org-entities-user '("diamond" "\\Diamond" t "&diamond;" "[diamond]" nil "⋄"))
;; (add-to-list 'org-entities-user '("langle" "\\langle" t "&langle;" "[left angle]" nil ""))
;; (add-to-list 'org-entities-user '("rangle" "\\rangle" t "&rangle;" "[right angle]" nil ""))

;; IMPORTANT: logic symbols
(after "org-entities"
  (add-to-list 'org-entities-user '("neg" "\\neg" nil nil nil nil "¬"))
  ;;(add-to-list 'org-entities-user '("iff" "\\iff" nil nil nil nil "↔"))
  (add-to-list 'org-entities-user '("iff" "\\iff" nil nil nil nil "\leftrightarrow"))
  (add-to-list 'org-entities-user '("top" "\\top" nil nil nil nil "⊤"))
  (add-to-list 'org-entities-user '("bot" "\\bot" nil nil nil nil "⊥"))
  (add-to-list 'org-entities-user '("therefore" "\\therefore" nil nil nil nil "∴"))
  (add-to-list 'org-entities-user '("because" "\\because" nil nil nil nil "∵"))
  (add-to-list 'org-entities-user '("derives" "\\vdash" nil nil nil nil "⊢"))
  (add-to-list 'org-entities-user '("notderives" "\\not\\vdash" nil nil nil nil "⊬"))
  (add-to-list 'org-entities-user '("models" "\\models" nil nil nil nil "⊨"))
  (add-to-list 'org-entities-user '("notmodels" "\\not\\models" nil nil nil nil "⊭"))
  (add-to-list 'org-entities-user '("forces" "\\Vdash" nil nil nil nil "⊩"))
  (add-to-list 'org-entities-user '("notforces" "\\not\\Vdash" nil nil nil nil "⊮"))
  (add-to-list 'org-entities-user '("boxconditional" "\\boxconditional" nil nil nil nil "□→"))
  (add-to-list 'org-entities-user '("box" "\\Box" nil nil nil nil "□"))
  (add-to-list 'org-entities-user '("diamond" "\\Diamond" nil nil nil nil "◇"))
  (add-to-list 'org-entities-user '("cdots" "\\cdots" nil nil nil nil "⋯"))
  (add-to-list 'org-entities-user '("ldots" "\\ldots" nil nil nil nil "…"))
  ;; IMPORTANT: mathematics symbols
  (add-to-list 'org-entities-user '("reals" "\\mathbb{R}" nil nil nil nil "ℝ"))
  (add-to-list 'org-entities-user '("integers" "\\mathbb{Z}" nil nil nil nil "ℤ"))
  (add-to-list 'org-entities-user '("primes" "\\mathbb{P}" nil nil nil nil "ℙ"))
  (add-to-list 'org-entities-user '("naturals" "\\mathbb{N}" nil nil nil nil "ℕ"))
  (add-to-list 'org-entities-user '("irrationals" "\\mathbb{I}" nil nil nil nil "𝕀"))
  (add-to-list 'org-entities-user '("rationals" "\\mathbb{Q}" nil nil nil nil "ℚ"))
  (add-to-list 'org-entities-user '("complex" "\\mathbb{C}" nil nil nil nil "ℂ"))
  ;; IMPORTANT: misc
  (add-to-list 'org-entities-user '("mid" "\\mid" t nil nil nil "|"))
  ;; IMPORTANT: phonetic symbols
  ;; TODO: investigate the \textipa{} environments as possible LaTeX exports (as done with \eng and \esh)
  ;; SOURCE: `http://www.phon.ucl.ac.uk/home/wells/ipa-unicode.htm'
  ;; SOURCE: `ftp://ftp.tex.ac.uk/ctan/ctan/tex-archive/bibliography/biber/documentation/utf8-macro-map.html'
  ;; SOURCE: `http://en.wikibooks.org/wiki/LaTeX/Linguistics#IPA_characters'
  (add-to-list 'org-entities-user '("eng" "\\textipa{N}" nil nil nil nil "ŋ"))
  (add-to-list 'org-entities-user '("esh" "\\textipa{S}" nil nil nil nil "ʃ"))
  (add-to-list 'org-entities-user '("thy" "\\eth" nil nil nil nil "ð"))
  (add-to-list 'org-entities-user '("thi" "\\theta" nil nil nil nil "θ"))
  (add-to-list 'org-entities-user '("darkl" "\\textltilde" nil nil nil nil "ɫ"))
  (add-to-list 'org-entities-user '("schwa" "\\textipa{@}" nil nil nil nil "ə"))
  (add-to-list 'org-entities-user '("dotlessj" "\\textbardotlessj" nil nil nil nil "ɟ"))
  (add-to-list 'org-entities-user '("curvedt" "\\textsubarch{t}" nil nil nil nil "ʈ"))
  (add-to-list 'org-entities-user '("retracteddiacritic" "\\b{n}" nil nil nil nil "n̠"))
  ;;(add-to-list 'org-entities-user '("alveolarapproximate" "\\textipa{\*r}" nil nil nil nil "ɹ"))
  (add-to-list 'org-entities-user '("alveolarapproximate" "\\textturnr" nil nil nil nil "ɹ"))
  (add-to-list 'org-entities-user '("fishhook" "\\textfishhookr" nil nil nil nil "ɾ"))
  (add-to-list 'org-entities-user '("palatalfricative" "\\textipa{C}" nil nil nil nil "ç"))
  (add-to-list 'org-entities-user '("bilabialclick" "\\textbullseye" nil nil nil nil "ʘ"))
  (add-to-list 'org-entities-user '("glottalstop" "" nil nil nil nil "ʔ"))
  (add-to-list 'org-entities-user '("alveolarstop" "\\textyogh" nil nil nil nil "ʒ"))
  (add-to-list 'org-entities-user '("pharyngealfricative" "" nil nil nil nil "ʕ"))
  ;;(add-to-list 'org-entities-user '("Eng" "\\textipa{N}" nil nil nil nil "Ŋ"))
  ;;(add-to-list 'org-entities-user '("Esh" "\\textipa{S}" nil nil nil nil "Ʃ"))
  )

;; ⟨ʋ⟩ ⟨ɑ⟩ ⟨ɣ⟩ ⟨ɛ⟩ ⟨ɸ⟩ ⟨ʋ⟩ ⟨β⟩ ⟨θ⟩ ⟨χ⟩

;; \mathbb{R}		\mathbf{R}		\mathcal{R}		\mathfrak{R}
;; \mathbb{Z}		\mathbf{Z}		\mathcal{Z}		\mathfrak{Z}
;; \mathbb{Q}		\mathbf{Q}		\mathcal{Q}		\mathfrak{Q}

;; TODO: add customizations for \mathcal{}'s

(defun org-insert-user-entity ()
  "Insert symbol from `org-entities-user' list."
  (interactive)
  (let ((entity (ido-completing-read "Insert entity: " (mapcar #'(lambda (element) (car element)) org-entities-user))))
    (insert (format "\\%s" entity))))

;; IMPORTANT: ...
(define-skeleton insert-org-latex-package
  "Inserts a LaTeX use-package clause into a document."
  "Insert package name: "
  "#+LATEX_HEADER: \\usepackage{" str "}")

;; IMPORTANT: `org-mode' custom file templates
(defvar org-template-list (list "beamer" "paper" "assignment") "List of custom template types.")

;;; IMPORTANT: org-beamer
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-beamer/tutorial.html'
;; SOURCE: `http://orgmode.org/manual/Beamer-class-export.html'
(autoload 'org-beamer "org-beamer" "Presentations with org-beamer." t)

(defvar org-beamer-themes-list (list "Atnibes"
				     "Bergen"
				     "Berkeley"
				     "Berlin"
				     "Copenhagen"
				     "Darmstadt"
				     "Dresden"
				     "Frankfurt"
				     "Goettingen"
				     "Hannover"
				     "Ilmenau"
				     "JuanLesPins"
				     "Luebeck"
				     "Madrid"
				     "Malmoe"
				     "Marburg"
				     "Montpellier"
				     "PaloAlto"
				     "Pittsburgh"
				     "Rochester"`
				     "Singapore"
				     "Szeged"
				     "Warsaw"
				     "boxes"
				     "default") "List of beamer available themes.")

;; NOTE: inserting a `reading-notes' template is not part of this function
(defun insert-org-template (&rest junk)
  "..."
  (interactive)
  (let ((type (ido-completing-read "Select template: " org-template-list))
	(title (read-string "Enter title: "))
	(options (list "toc:nil" "tasks:nil"))
	(author "Matthew Ball"))
    (insert (format "#+LATEX_CLASS: %s\n" type))
    (when (string= type "beamer")
      (insert (format "#+LATEX_HEADER: \\usetheme{%s}\n" (ido-completing-read "Select theme: " org-beamer-themes-list))))
    (mapc #'(lambda (option) (insert (format "#+OPTIONS: %s\n" option))) options)
    (insert "\n")
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+AUTHOR: %s\n\n" author))
    (insert (format "* %s\n" title))
    (insert "* Footnotes\n")))

(defun goto-or-create-reading-notes-file (title &rest junk)
  "Open the corresponding reading notes file for a document, or create a new file if one doesn't exist."
  (interactive "sEnter title: ")
  (let ((file-name (format "~/Documents/Reading/notes/%s.org" (replace-regexp-in-string " " "-" (downcase title))))
	;;(file (format user-reading-directory "notes/%s.org") (replace-regexp-in-string " " "-" (downcase title)))
	)
    (if (file-exists-p file-name)
	(find-file file-name)
      (progn
	(find-file file-name)
	(let ((author (read-string "Enter author's name: ")))
	  (insert-org-reading-notes-template title author))))))

(defun insert-org-reading-notes-template (title name &rest junk)
  "Insert a template for a reading notes file into an `org-mode' document."
  (interactive "sEnter title: \nsEnter author's name: ")
  (let ((type "paper")
	(options (list "toc:nil" "tasks:nil"))
	(author "Matthew Ball"))
    (insert (format "#+LATEX_CLASS: %s\n" type))
    (mapc #'(lambda (option) (insert (format "#+OPTIONS: %s\n" option))) options)
    (insert "\n")
    (insert (format "#+TITLE: %s by %s: Reading Notes\n" title name))
    (insert (format "#+AUTHOR: %s\n" author))
    (insert "\n")
    ;; (insert "* TODO Paper: \n") ;; TODO: link to PDF file (if it exists)
    (insert (format "* %s\n" title))
    (insert "* Footnotes\n")))

;; IMPORTANT: the following `define-skeleton' entries are old and redundant
(define-skeleton insert-org-paper
  "Inserts an `org-mode' paper template."
  "Insert paper title: "
  "#+LATEX_CLASS: paper\n#+OPTIONS: toc:nil\n#+OPTIONS: tasks:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball\n\n* " str "\n* Footnotes\n")

(define-skeleton insert-org-assignment
  "Inserts an `org-mode' assignment template."
  "Insert assignment title: "
  "#+LATEX_CLASS: assignment\n#+OPTIONS: toc:nil\n#+OPTIONS: tasks:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball, u4537508\n\n* " str "\n* Footnotes\n")

(define-skeleton insert-org-beamer
  "Inserts an `org-mode' beamer presentation template."
  "Insert presentation title: "
  "#+LATEX_CLASS: beamer\n#+LATEX_HEADER: \\usetheme{Warsaw}\n#+OPTIONS: toc:nil\n#+OPTIONS: tasks:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball\n\n* " str "\n* Footnotes\n")

;;; IMPORTANT: Insert a custom file template
(defvar org-custom-file-alist (list "paper" "beamer" "assignment") "List of custom file types for use with `org-mode' documents.")

(defun org-insert-custom-file (&rest junk)
  "Insert custom `org-mode' file template."
  (interactive)
  (let ((custom-file-type (ido-completing-read "Select file type: " org-custom-file-alist)))
    (funcall (intern (concat "insert-org-" custom-file-type)))))

;;; IMPORTANT: custom footnotes
;; TODO: get user input from the keyboard
(defvar org-custom-footnote-types (list "book" "paper" "article" "default") "The list of availables types for footnotes.")

(defun org-custom-insert-footnote-book ()
  "Insert a book footnote template."
  ;; - author
  ;; - title
  ;; - publisher
  ;; - year
  ;; - page number(s)
  )

(defun org-custom-insert-footnote-paper ()
  "Insert a paper footnote template."
  ;; - author
  ;; - title
  ;; - journal
  ;; - volume number
  ;; - year
  ;; - page number(s)
  )

(defun org-custom-insert-footnote-article ()
  "Insert a article footnote template."
  )

(defun org-custom-insert-footnote-default ()
  "Insert a default footnote template."
  ;; - text
  )

(defun org-custom-insert-footnote (footnote-name)
  "Insert a footnote in an `org-mode' document."
  (interactive "sEnter footname name: ")
  (save-excursion
    (let* ((footnote-type (ido-completing-read "Select footnote type: " org-custom-footnote-types))
	   (footnote-text (funcall (intern (concat "org-custom-insert-footnote-" footnote-type)))))
      (insert (concat "[fn:" footnote-name "]"))
      (end-of-buffer)
      (insert (concat "\n[fn:" footnote-name "] " footnote-text)))))

;;; IMPORTANT: this is a set of custom inserts for common clauses in an `org-mode' document
(defun custom-org-insert-footnote (name text) ;; TODO: this could be made so much better
  "Insert a footnote in an `org-mode' document."
  (interactive "sEnter footnote name: \nsEnter text: ")
  (save-excursion
    (insert (concat "[fn:" name "]"))
    (end-of-buffer)
    (insert (concat "\n[fn:" name "] " text))))

;;; IMPORTANT: custom inserts
(defun surrounded-by-p (char)
  "Returns t if word is surrounded by given char."
  (save-excursion
    (and (forward-word -1)
         (equal char (char-before))
         (forward-word 1)
         (equal char (char-after)))))

(defun surround-word (char &optional force)
  "Surrounds word with given character.  If force is nil and word is already surrounded by given character remoevs them."
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

;; (defmacro propertize-word (prop char)
;;   "..."
;;   `(defun (intern (concat ,prop "-word")) (&optional force)
;;      "Insert a PROPERTY character before (and after) an input string."
;;      (interactive "p")
;;      (surround-word ?,char ,force)
;;      ))
;; (propertize-word 'bold ?*) => (bold-word)
;; (propertize-word 'italic ?/) => (italic-word)
;; etc

(defun my-bold-word (&optional force) ;; NOTE: C-c b should be `org-insert-text-bolded'
  "Insert a bold character (*) before (and after) an input string."
  (interactive "p")
  (surround-word ?* force))

(defun my-italic-word (&optional force) ;; NOTE: C-c i should be `org-insert-text-italicised'
  "Insert an italicise character (/) before (and after) an input string."
  (interactive "p")
  (surround-word ?/ force))

(defun my-underline-word (&optional force) ;; NOTE: C-c u should be `org-insert-text-underlined'
  "Insert an underline character (_) before (and after) an input string."
  (interactive "p")
  (surround-word ?_ force))

(defun my-verbatim-word (&optional force) ;; NOTE: C-c v should be ...
  "Insert a verbatim character (~) before (and after) an input string."
  (interactive "p")
  (surround-word ?~ force))

(defun my-teletyped-word (&optional force) ;; NOTE: C-c t should be ...
  "Insert a teletype character (=) before (and after) an input string."
  (interactive "p")
  (surround-word ?= force))

;;; IMPORTANT: customisations
(defun turn-on-custom-org-bindings ()
  "Activate custom `org-mode' bindings."
  ;; TODO: add binding for `org-insert-custom-file' command
  (define-key org-mode-map (kbd "C-M-j") 'org-insert-heading) ;; NOTE: M-RET inserts a new heading
  ;;(define-key org-mode-map (kbd "C-c p") 'insert-org-paper) ;; NOTE: insert paper template with C-c p
  ;;(define-key org-mode-map (kbd "C-c b") 'insert-org-beamer) ;; NOTE: insert beamer template with C-c b
  ;;(define-key org-mode-map (kbd "C-c c") 'org-insert-citation) ;; NOTE: insert a citation clause
  (define-key org-mode-map (kbd "C-c i") 'org-insert-latex-clause) ;; NOTE: insert a LaTeX clause with C-c i
  (define-key org-mode-map (kbd "C-c f") 'custom-org-insert-footnote) ;; NOTE: insert a footnote with C-c f
  (define-key org-mode-map (kbd "C-c b") 'my-bold-word)
  (define-key org-mode-map (kbd "C-c i") 'my-italic-word)
  (define-key org-mode-map (kbd "C-c u") 'my-underline-word)
  (define-key org-mode-map (kbd "C-c v") 'my-verbatim-word)
  (define-key org-mode-map (kbd "C-c t") 'my-teletyped-word))

(defun turn-on-custom-org ()
  "Activate custom `org-mode' functionality."
  (org-toggle-pretty-entities) ;; NOTE: toggle UTF-8 unicode symbols
  ;;(org-indent-mode) ;; NOTE: indent with headings
  ;;(setq org-startup-indented t) ;; NOTE: indent with headings
  (turn-on-custom-org-bindings)) ;; NOTE: enable custom org-mode bindings

(add-hook 'org-mode-hook (lambda () (turn-on-custom-org)))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;;; IMPORTANT: journal entries with `org-mode'
;; SOURCE: `http://www.emacswiki.org/emacs/OrgJournal'
(require 'org-journal)

(after "org-journal"
  (setq org-journal-dir
	(expand-file-name (concat user-organisation-directory "/journal/"))))

;;; IMPORTANT: word count
;; SOURCE: `http://orgmode.org/worg/org-hacks.html'
;; (defun org-word-count (beg end
;;                            &optional count-latex-macro-args?
;;                            count-footnotes?)
;;   "Report the number of words in the Org mode buffer or selected region.
;; Ignores:
;; - comments
;; - tables
;; - source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
;; - hyperlinks (but does count words in hyperlink descriptions)
;; - tags, priorities, and TODO keywords in headers
;; - sections tagged as 'not for export'.

;; The text of footnote definitions is ignored, unless the optional argument
;; COUNT-FOOTNOTES? is non-nil.

;; If the optional argument COUNT-LATEX-MACRO-ARGS? is non-nil, the word count
;; includes LaTeX macro arguments (the material between {curly braces}).
;; Otherwise, and by default, every LaTeX macro counts as 1 word regardless
;; of its arguments."
;;   (interactive "r")
;;   (unless mark-active
;;     (setf beg (point-min)
;;           end (point-max)))
;;   (let ((wc 0)
;;         (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
;;     (save-excursion
;;       (goto-char beg)
;;       (while (< (point) end)
;;         (cond
;;          ;; Ignore comments.
;;          ((or (org-in-commented-line) (org-at-table-p))
;;           nil)
;;          ;; Ignore hyperlinks. But if link has a description, count
;;          ;; the words within the description.
;;          ((looking-at org-bracket-link-analytic-regexp)
;;           (when (match-string-no-properties 5)
;;             (let ((desc (match-string-no-properties 5)))
;;               (save-match-data
;;                 (incf wc (length (remove "" (org-split-string
;;                                              desc "\\W")))))))
;;           (goto-char (match-end 0)))
;;          ((looking-at org-any-link-re)
;;           (goto-char (match-end 0)))
;;          ;; Ignore source code blocks.
;;          ((org-in-regexps-block-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
;;           nil)
;;          ;; Ignore inline source blocks, counting them as 1 word.
;;          ((save-excursion
;;             (backward-char)
;;             (looking-at org-babel-inline-src-block-regexp))
;;           (goto-char (match-end 0))
;;           (setf wc (+ 2 wc)))
;;          ;; Count latex macros as 1 word, ignoring their arguments.
;;          ((save-excursion
;;             (backward-char)
;;             (looking-at latex-macro-regexp))
;;           (goto-char (if count-latex-macro-args?
;;                          (match-beginning 2)
;;                        (match-end 0)))
;;           (setf wc (+ 2 wc)))
;;          ;; Ignore footnotes.
;;          ((and (not count-footnotes?)
;;                (or (org-footnote-at-definition-p)
;;                    (org-footnote-at-reference-p)))
;;           nil)
;;          (t
;;           (let ((contexts (org-context)))
;;             (cond
;;              ;; Ignore tags and TODO keywords, etc.
;;              ((or (assoc :todo-keyword contexts)
;;                   (assoc :priority contexts)
;;                   (assoc :keyword contexts)
;;                   (assoc :checkbox contexts))
;;               nil)
;;              ;; Ignore sections marked with tags that are
;;              ;; excluded from export.
;;              ((assoc :tags contexts)
;;               (if (intersection (org-get-tags-at) org-export-exclude-tags
;;                                 :test 'equal)
;;                   (org-forward-same-level 1)
;;                 nil))
;;              (t
;;               (incf wc))))))
;;         (re-search-forward "\\w+\\W*")))
;;     (message (format "%d words in %s." wc
;;                      (if mark-active "region" "buffer")))))

(provide 'writing-config)
;;; writing-config.el ends here

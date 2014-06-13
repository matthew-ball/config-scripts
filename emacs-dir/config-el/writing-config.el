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
;; TODO: move this to `user-config.el'
;; SOURCE: `http://jblevins.org/projects/deft/'
(autoload 'deft "deft" "Note taking with deft." t)

(after "deft"
  (setq deft-extension "org"
	deft-text-mode 'org-mode
	deft-directory (format "%s.deft/" user-organisation-directory)))

;;; IMPORTANT: diary and calendar mode
;; SOURCE: `http://www.emacswiki.org/emacs/DiaryMode'
;; SOURCE: `http://www.emacswiki.org/emacs/CalendarMode'
;; TODO: move this to `general-config.el'
;; (autoload 'calendar "calendar" "Keep a personal diary with GNU Emacs." t)

;; (setq calendar-view-diary-initially-flag t
;;       calendar-view-holidays-initially-flag t
;;       ;;calendar-mark-diary-entries-flag t
;;       ;;calendar-mark-holidays-flag t
;;       ;;diary-file "/home/chu/Documents/Organisation/diary"
;;       number-of-diary-entries 7)

;; (eval-after-load "calendar"
;;   ;; (add-hook 'diary-display-hook 'fancy-diary-display)
;;   ;; (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;;   )

;;; IMPORTANT: flyspell
;; TODO: move this to `general-config.el'
;; SOURCE: `http://www.emacswiki.org/emacs/FlySpell'
;;(require 'flyspell)
(autoload 'flyspell-mode "flyspell" "..." t)

(after "flyspell"
  (setq flyspell-issue-welcome-flag nil)
  (add-hook 'text-mode-hook 'turn-on-flyspell)) ;; NOTE: turn on automatic spell check if in a `text-mode'

;;; IMPORTANT: ispell
;; TODO: move this to `general-config.el'
(require 'ispell)

(after "ispell"
  (setq ispell-program-name "aspell" ;; NOTE: use aspell for automatic spelling
	ispell-parser 'tex
	ispell-dictionary "british"
	;; ispell-alternate-dictionary "/usr/share/dict/american-english" ;; FIX: ...
	ispell-extra-args '("--sug-mode=ultra")))

;;; IMPORTANT: thesaurus
;; SOURCE: `http://emacswiki.org/emacs/thesaurus.el'
;; (autoload 'thesaurus-choose-synonym-and-replace "thesaurus" "Choose and replace a word with it's synonym." t)

;; (after "thesaurus"
;;   (setq thesaurus-bhl-api-key "8c5a079b300d16a5bb89246322b1bea6"))  ;; NOTE: from registration

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
	reftex-extra-bindings t))

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

  (setcdr (assoc "pdf" ebib-file-associations) "epdfview"))

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
  ;; TODO: split these up - move them earlier in the config (if possible)
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
	  ;;("WEBSITE"          . ?w)
	  )))

;;; IMPORTANT: `org-link'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Handling-links.html'
(after "org"
  (org-add-link-type "ebib" 'ebib)

  ;; TODO: add more citation types to ebib
  (org-add-link-type "cite" 'ebib
		     (lambda (path desc format)
		       (cond ((eq format 'latex)
			      (format "\\cite{%s}" path)))))

  ;; TODO: create an ebib entry which links to ERC logs
  ;; NOTE: this would require `erc-log-mode' from MELPA

  ;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Link-abbreviations.html'
  ;; (setq org-link-abbrev-alist '(("google"   . "http://www.google.com/search?q=")))
  )

;;; IMPORTANT: blogging from emacs
;; SOURCE: `http://bzg.fr/blogging-from-emacs.html'
;; (after "ox-publish"
;;   (setq org-publish-project-alist
;; 	'(("blog"
;; 	   :base-directory "~/"
;; 	   :html-extension "html"
;; 	   :base-extension "org"
;; 	   :publishing-directory "~/Public/html/"
;; 	   :publishing-function (org-html-publish-to-html)
;; 	   :html-preamble nil
;; 	   :html-postamble nil))))

;;; IMPORTANT: org-agenda
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html'
(after "org-agenda"
  (setq org-agenda-include-diary nil ;; NOTE: include entries from the emacs diary
	org-agenda-skip-scheduled-if-done t ;; NOTE: ...
	org-agenda-inhibit-startup t
	org-agenda-skip-deadline-if-done t ;; NOTE: ...
	org-agenda-skip-additional-timestamps-same-entry nil ;; NOTE: don't skip multiple entries per day
	org-agenda-dim-blocked-tasks nil ;; NOTE: do not dim blocked tasks
	org-agenda-span 'month) ;; NOTE: show a month of agendas

  (setq org-agenda-files `(,(expand-file-name user-org-university-file)
			   ,(expand-file-name user-org-notes-file) ;; IMPORTANT: this is "journal.org" 
			   ,(expand-file-name user-org-projects-file)
			   ,(concat (expand-file-name user-organisation-directory) "home.org")
			   ;;,(concat (expand-file-name user-organisation-directory) "contacts.org")
			   ,(concat (expand-file-name user-organisation-directory) "birthday.org")
			   ;;,(concat (expand-file-name user-organisation-directory) "bookmarks.org")
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
(setq org-export-latex-default-class "article"
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
	'(;; ("L" "Library" entry (file+headline (expand-file-name user-org-university-file) "Library")
	  ;;  "** %^{Title} %?%^g\n - Borrowed %^t\n - Due: %^t\n\n" :empty-lines 1 :immediate-finish 1)
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
  (require 'ob-lisp)

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
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
				 (screen . nil)
				 (sh . nil)))

  (setq org-confirm-babel-evaluate nil ;; NOTE: no confirmation before evaluating code
	org-src-fontify-natively t ;; NOTE: enable fontify in source code blocks
	org-src-tab-acts-natively t ;; NOTE: tab works properly
	))

;;; IMPORTANT: org-latex-export
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-latex-export.html'
(autoload 'org-latex "org-latex" "Render LaTeX with `org-mode'." t)
;; (autoload 'org-bibtex "org-bibtex" "Bibliographies with `org-mode'." t)

;; (after "org-bibtex"
;;   (require 'org-exp-bibtex))

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
;; (after "org"
;;   (add-to-list 'org-export-latex-packages-alist '("" "listings")) ;; NOTE: listings package
;;   (add-to-list 'org-export-latex-packages-alist '("" "color")) ;; NOTE: colored source code
;;   (add-to-list 'org-export-latex-packages-alist '("" "tipa")) ;; NOTE: support for phonetic alphabet
;;   (add-to-list 'org-export-latex-packages-alist '("" "tipx")) ;; NOTE: support for phonetic alphabet
;;   ;;(add-to-list 'org-export-latex-packages-alist '("" "bussproofs")) ;; NOTE: for sequent style proofs
;;   (add-to-list 'org-export-latex-packages-alist '("" "amssymb")) ;; NOTE: mathematics symbols
;;   (add-to-list 'org-export-latex-packages-alist '("" "amsmath")) ;; NOTE: mathematics symbols
;;   (add-to-list 'org-export-latex-packages-alist '("" "hyperref"))) ;; NOTE: hyper-references

;;; IMPORTANT: `org-entities'
;; SOURCE: `http://orgmode.org/manual/Special-symbols.html'
(autoload 'org-entities "org-entities" "Enable unicode support for `org-mode'." t)

(after "org-entities"
  (require 'org-entities-user+))

(defun org-insert-user-entity ()
  "Insert symbol from `org-entities-user' list."
  (interactive)
  (let ((entity (ido-completing-read "Insert entity: " (mapcar #'(lambda (element) (car element)) org-entities-user))))
    (insert (format "\\%s" entity))))

(defun org-insert-entity ()
  "Insert symbol from `org-entities' list."
  (interactive)
  (let ((entity	(ido-completing-read "Insert entity: "
				     (remove-if #'null (mapcar #'(lambda (element)
								   (unless (stringp element)
								     (format "%s" (car element))))
							       org-entities)))))
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

;;(define-key org-mode-map (kbd "C-c b") '(propertize-word bold ?*))

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
  (define-key org-mode-map (kbd "C-c b") 'bold-word)
  (define-key org-mode-map (kbd "C-c i") 'italic-word)
  (define-key org-mode-map (kbd "C-c u") 'underline-word)
  (define-key org-mode-map (kbd "C-c v") 'verbatim-word)
  (define-key org-mode-map (kbd "C-c t") 'teletype-word))

(defun turn-on-custom-org ()
  "Activate custom `org-mode' functionality."
  (org-toggle-pretty-entities) ;; NOTE: toggle UTF-8 unicode symbols
  ;;(org-indent-mode) ;; NOTE: indent with headings
  ;;(setq org-startup-indented t) ;; NOTE: indent with headings
  (auto-complete-mode t)
  (imenu-add-to-menubar "Imenu")
  (turn-on-custom-org-bindings)) ;; NOTE: enable custom org-mode bindings

(defun turn-on-hl-mode ()
  ""
  (hl-line-mode t))

(add-hook 'org-mode-hook 'turn-on-custom-org)
(add-hook 'org-agenda-mode-hook 'turn-on-hl-mode 'append)

(provide 'writing-config)
;;; writing-config.el ends here

;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/org-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: org-mode
;; SOURCE: `http://emacswiki.org/emacs/OrgMode'
;; SOURCE: `http://lists.gnu.org/archive/html/emacs-orgmode/2011-04/msg00761.html'
(autoload 'org-install "org-exp" "Organise tasks with org-mode." t)
(autoload 'org-special-blocks "org-special-blocks" "Render blocks of code with org-mode." t)
(autoload 'org-bbdb-open "org-bbdb" "The big-brother database and org-mode." t)

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
      ;; --- notes ---
      org-directory (expand-file-name user-organisation-directory) ;; NOTE: default directory for org mode
      org-default-notes-file (expand-file-name user-org-notes-file) ;; NOTE: file for quick notes
      ;; --- `org-archive' ---
      org-archive-location (concat (expand-file-name user-org-archive-file) "::* Archives") ;; NOTE: archiving items
      ;; --- `org-refile' ---
      org-refile-target '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)) ;; NOTE: any file contributing (agenda); up to 5 levels deep
      org-refile-use-outline-path 'file ;; NOTE: targets start with the file name - allows creating level 1 tasks
      org-refile-allow-creating-parent-nodes 'confirm) ;; NOTE: allow refile to create parent tasks with confirmation

;;; COMMENT: org-agenda
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html'
(setq org-agenda-include-diary t ;; NOTE: include entries from the emacs diary
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
	("j" "Journal"
	 ((tags "JOURNAL"))
	 "JOURNAL")
	("r" "Reading"
	 ((tags "READING")
	  (tags "WEBSITE"))
	 "READING") ;; NOTE: `READING' tasks
	))

;;; COMMENT: org-export
;; SOURCE: `http://orgmode.org/manual/Exporting.html'
(setq org-export-latex-default-class "paper"
      org-export-with-toc nil ;; NOTE: turn off `org-mode' exporting a table of contents
      org-export-run-in-background t ;; NOTE: run `org-export' tasks in the background
      org-export-with-tasks nil ;; NOTE: turn off `org-mode' exporting tasks
      org-export-with-todo-keywords nil) ;; NOTE: turn off `org-mode' exporting of TODO keywords

;;; COMMENT: org-capture
;; SOURCE: `http://orgmode.org/manual/Capture.html'
;; SOURCE: `http://orgmode.org/worg/org-contrib/org-protocol.html'
(autoload 'org-capture "org-capture" "..." t)
(autoload 'org-protocol "org-protocol" "Use `org-mode' with `emacsclient'." t)

;; TODO: re-order AND add new tags
(setq org-tag-alist ;; NOTE: list of tags allowed in `org-mode' files
      '(("ASSIGNMENT"       . ?a)
	("BOOKMARK"         . ?b)
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
	("WEBSITE"          . ?w)))

;;; COMMENT: capture templates (WARNING: do not use 'C' or 'q' characters for binding)
(setq org-capture-templates
      '(("L" "Library" entry (file+headline (expand-file-name user-org-university-file) "Library")
         "** %^{Title} %?%^g\n - Borrowed %^t\n - Due: %^t\n\n" :empty-lines 1 :immediate-finish 1)
        ("U" "University Course" entry (file+headline (expand-file-name user-org-university-file) "Courses")
	 "%(add-course)" :empty-lines 1 :immediate-finish 1)
        ("A" "Assignment" plain (file+function (expand-file-name user-org-university-file) course-code)
         "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
        ("c" "Contacts" plain (file+headline (expand-file-name user-org-contacts-file) "Contacts")
         "[[bbdb:%^{Name}][%^{Name}]] %?%^g" :empty-lines 1 :immediate-finish 1)
        ("J" "Journal" entry (file+datetree (expand-file-name user-org-notes-file))
         "* %?\n Entered on %U\n  %i\n  %a")
        ("N" "Note" entry (file+headline (expand-file-name user-org-notes-file) "Notes")
	 "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
        ("P" "Projects" plain (file+function (expand-file-name user-org-projects-file) project-capture)
         "*** TODO %^{Description} %?%^g\n" :empty-lines 1 :immediate-finish 1)
        ("R" "Reading" entry (file+headline (expand-file-name user-org-projects-file) "Reading"))
        ("W" "Writing" entry (file+headline (expand-file-name user-org-projects-file) "Writing"))
        ;; TODO: need to write `reading-capture' and `writing-capture' functions
        ))

;;; COMMENT: custom capture functions
(defun search-for-heading (file heading prefix &rest junk)
  "Search for an `org-heading' HEADING in an FILE."
  (switch-to-buffer file)
  (goto-char (point-min))
  (when (search-forward (concat prefix heading "\t") nil nil)
    (progn
      ;;(beginning-of-line)
      (end-of-line)
      ;;(insert "\n")
      )))

;;; COMMENT: reading capture
(defvar reading-material-types (list "Paper" "Book" "Web-site") "List of reading material.")

(defun reading-capture (&rest junk)
  "Capture a reading task, through one of its variants."
  (let ((reading-type (ido-completing-read "Material Type: " reading-material-types))
  	;;(project-str "")
	)
    (search-for-heading "projects.org" reading-type "** ")))

;;; COMMENT: writing capture
(defvar writing-types (list "Paper" "Book") "List of writing material.")

(defun writing-capture (&rest junk)
  "...")

;;; COMMENT: project capture
;; NOTE: this is probably not the best way of doing this
(defvar project-options-alist nil "List of available project types.")

(add-to-list 'project-options-alist "Travel")
;;(add-to-list 'project-options-alist "Ideas")
(add-to-list 'project-options-alist "Programming")
(add-to-list 'project-options-alist "Writing")
(add-to-list 'project-options-alist "General")

;; TODO: write some way of "counting" the stars (something like `depth')
(defun project-capture (&rest junk)
  "Capture a `project' through one of its variants."
  (let ((project-type (ido-completing-read "Project Type: " project-options-alist))
  	;;(project-str "")
	)
    (search-for-heading "projects.org" project-type "** ")))

;;; COMMENT: school organisation
;; TODO: integrate with `ido-mode' somehow
;; WARNING: this is *terrible* - seriously consider a re-write !!!
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

;;; COMMENT: org-babel
;; SOURCE: `http://orgmode.org/worg/org-contrib/babel/intro.html'
(autoload 'org-babel-load-file "org-babel" "Interact with programming languages in `org-mode'." t)
;;(require 'ob-haskell) ;; NOTE: require `org-babel-haskell'

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
      )

;;; COMMENT: org-latex-export
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-latex-export.html'
(autoload 'org-latex "org-latex" "Render LaTeX with `org-mode'." t)
(autoload 'org-bibtex "org-bibtex" "Bibliographies with `org-mode'." t)
(require 'org-exp-bibtex) ;; TODO: change to an autoload

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

;; COMMENT: enable latex source code highlighting
(setq org-export-latex-listings t) ;; NOTE: enable listings features

;; TODO: modify `org-export-latex-packages-alist' (i.e. include some LaTeX packages)
(add-to-list 'org-export-latex-packages-alist '("" "listings")) ;; NOTE: listings package
(add-to-list 'org-export-latex-packages-alist '("" "color")) ;; NOTE: colored source code
(add-to-list 'org-export-latex-packages-alist '("" "tipa")) ;; NOTE: support for phonetic alphabet
(add-to-list 'org-export-latex-packages-alist '("" "tipx")) ;; NOTE: support for phonetic alphabet
;;(add-to-list 'org-export-latex-packages-alist '("" "bussproofs")) ;; NOTE: for sequent style proofs
(add-to-list 'org-export-latex-packages-alist '("" "amssymb")) ;; NOTE: mathematics symbols
(add-to-list 'org-export-latex-packages-alist '("" "amsmath")) ;; NOTE: mathematics symbols
(add-to-list 'org-export-latex-packages-alist '("" "hyperref")) ;; NOTE: hyper-references

;;; COMMENT: `org-entities'
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

;; COMMENT: logic symbols
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
;; COMMENT: mathematics symbols
(add-to-list 'org-entities-user '("reals" "\\mathbb{R}" nil nil nil nil "ℝ"))
(add-to-list 'org-entities-user '("integers" "\\mathbb{Z}" nil nil nil nil "ℤ"))
(add-to-list 'org-entities-user '("primes" "\\mathbb{P}" nil nil nil nil "ℙ"))
(add-to-list 'org-entities-user '("naturals" "\\mathbb{N}" nil nil nil nil "ℕ"))
(add-to-list 'org-entities-user '("irrationals" "\\mathbb{I}" nil nil nil nil "𝕀"))
(add-to-list 'org-entities-user '("rationals" "\\mathbb{Q}" nil nil nil nil "ℚ"))
(add-to-list 'org-entities-user '("complex" "\\mathbb{C}" nil nil nil nil "ℂ"))
;; COMMENT: misc
(add-to-list 'org-entities-user '("mid" "\\mid" t nil nil nil "|"))
;; COMMENT: phonetic symbols
(add-to-list 'org-entities-user '("eng" "\\textipa{N}" nil nil nil nil "ŋ"))
(add-to-list 'org-entities-user '("esh" "\\textipa{S}" nil nil nil nil "ʃ"))
(add-to-list 'org-entities-user '("thy" "\\eth" nil nil nil nil "ð"))
(add-to-list 'org-entities-user '("thi" "\\theta" nil nil nil nil "θ"))
(add-to-list 'org-entities-user '("alveolarapproximate" "\\textipa{\*r}" nil nil nil nil "ɹ"))
(add-to-list 'org-entities-user '("palatalfricative" "\\textipa{C}" nil nil nil nil "ç"))
(add-to-list 'org-entities-user '("bilabialclick" "\\textbullseye" nil nil nil nil "ʘ"))
(add-to-list 'org-entities-user '("glottalstop" "" nil nil nil nil "ʔ"))
(add-to-list 'org-entities-user '("alveolarstop" "\\textyogh" nil nil nil nil "ʒ"))
(add-to-list 'org-entities-user '("pharyngealfricative" "" nil nil nil nil "ʕ"))
;;(add-to-list 'org-entities-user '("Eng" "\\textipa{N}" nil nil nil nil "Ŋ"))
;;(add-to-list 'org-entities-user '("Esh" "\\textipa{S}" nil nil nil nil "Ʃ"))

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

;; COMMENT: ...
(define-skeleton insert-org-latex-package
  "Inserts a LaTeX use-package clause into a document."
  "Insert package name: "
  "#+LATEX_HEADER: \\usepackage{" str "}")

;; COMMENT: `org-mode' custom file templates
(defvar org-template-list (list "beamer" "paper" "assignment") "List of custom template types.")

;;; COMMENT: org-beamer
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

;; COMMENT: the following `define-skeleton' entries are old and redundant
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

;;; COMMENT: Insert a custom file template
(defvar org-custom-file-alist (list "paper" "beamer" "assignment") "List of custom file types for use with `org-mode' documents.")

(defun org-insert-custom-file (&rest junk)
  "Insert custom `org-mode' file template."
  (interactive)
  (let ((custom-file-type (ido-completing-read "Select file type: " org-custom-file-alist)))
    (funcall (intern (concat "insert-org-" custom-file-type)))))

;;; COMMENT: custom footnotes
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

;;; COMMENT: this is a set of custom inserts for common clauses in an `org-mode' document
(defun custom-org-insert-footnote (name text) ;; TODO: this could be made so much better
  "Insert a footnote in an `org-mode' document."
  (interactive "sEnter footnote name: \nsEnter text: ")
  (save-excursion
    (insert (concat "[fn:" name "]"))
    (end-of-buffer)
    (insert (concat "\n[fn:" name "] " text))))

;;; COMMENT: this is a sort of weird style automatic LaTeX clause monitor for `org-mode' documents
;; TODO: write functions for the following `alist' variables
(defvar org-latex-math-operator-alist (list "frac" "sqrt" "times") "List of mathematical operators in LaTeX.")

(defvar org-latex-environment-alist (list "align" "align*" "equation" "equation*" "matrix" "matrix*" "proof")
  "List of mathematical environments in LaTeX")

(defvar org-latex-math-font-alist (list "mathbb" "mathbf" "mathcal" "mathfrak") "List of mathematical fonts in LaTeX.")

;; (defvar org-latex-math-font-alist (list "mathnormal" "mathrm" "mathit" "mathbf" "mathsf" "mathtt" "mathcal" "mathfrak" "mathbb" "mathscr")
;;   "List of mathematical fonts in LaTeX.")

(defvar org-latex-clause-alist (list "font" "environment" "operator") "List of LaTeX clauses.")

(defun org-insert-latex-math-font ()
  "Insert a LaTeX math font clause into an `org-mode' document."
  (interactive)
  (let ((font (ido-completing-read "Select LaTeX math font: " org-latex-math-font-alist)))
    (insert (concat "$\\" font "{}$"))
    (backward-char 2)))

(defun org-insert-latex-math-environment ()
  "Inserts a LaTeX math environment into an `org-mode' document."
  (interactive)
  (let ((environment (ido-completing-read "Select LaTeX math environment: " org-latex-environment-alist)))
    (indent-relative-maybe)
    (insert (concat "\\begin{" environment "}\n\n"))
    (indent-relative-maybe)
    (insert (concat "\\end{" environment "}"))
    (previous-line)
    (indent-relative-maybe)))

(defun org-insert-latex-math-operator ()
  "Inserts a LaTeX math operator into an `org-mode' document."
  (message "Hello, world"))

(defun org-insert-latex-clause ()
  "Insert a LaTeX clause into an `org-mode' document."
  (interactive)
  (let ((clause (ido-completing-read "Insert LaTeX clause: " org-latex-clause-alist)))
    (funcall (intern (concat "org-insert-latex-math-" clause)))))

;;; COMMENT: latex symbol
;; TODO: ...
(defvar org-custom-symbol-types nil "...")

(setq org-custom-symbol-types (list "greek alphabet"
				    "mathematics"
				    "logic"
				    ;; TODO: anything else?
				    ))

(defun org-custom-insert-symbol ()
  "..."
  )

;;; COMMENT: custom `org-mode' combinations
(define-skeleton org-insert-text-underlined ;; NOTE: C-c u should be `org-insert-text-underlined'
  "Insert an underline character (_) before (and after) an input string."
  "Enter a string: "
  "_" str "_")

(define-skeleton org-insert-text-italicised ;; NOTE: C-c i should be `org-insert-text-italicised'
  "Insert an italicise character (/) before (and after) an input string."
  "Enter a string: "
  "/" str "/")

(define-skeleton org-insert-text-bolded ;; NOTE: C-c b should be `org-insert-text-bolded'
  "Insert a bold character (*) before (and after) an input string."
  "Enter a string: "
  "*" str "*")

(define-skeleton org-insert-text-teletyped ;; NOTE: C-c t should be `org-insert-text-teletyped'
  "Insert a teletype character (=) before (and after) an input string."
  "Enter a string: "
  "=" str "=")

;;; COMMENT: `org-ref-man'
;; NOTE: this is the beginning of a sort of "reference manager" extension which utilises org-mode functionality
;; TODO:
;; - Learn `org-bibtex'.
;; - Integrate `org-ref-man' and `org-bibtex'.
(defun generate-paper-entry (file-name) ;; TODO: update this to reflect spreadsheet format
  "Generate an `org-mode' style file link."
  (insert "[[file:" file-name "][" (file-name-sans-extension (file-relative-name file-name)) "]]\n" ))

;; SOURCE: `org-bibtex.el'
(defun generate-paper-list (dir-name) ;; TODO: make the .pdf extension a variable (NOTE: perhaps modifiable as an argument)
  "Generate a list of PDF documents in a directory supplied by the `DIR-NAME' argument."
  (if (file-exists-p dir-name)
      (let (files result)
	(setq files (directory-files dir-name t (concat "\.pdf$") t))
	(dolist (file files)
	  (when (and (file-readable-p file) (not (file-directory-p file)))
	    (setq result (cons file result))
	    (generate-paper-entry file)))
	result)))

(defun generate-paper-list-current-buffer (&rest junk)
  "Generate a list of documents from the directory of the current buffer."
  (interactive)
  (generate-paper-list (file-name-directory (buffer-file-name))))

;;; COMMENT: ...
(defun get-page-title (url) ;; FIX: does this actually work?
  "Get title of web page, whose url can be found in the current line."
  (interactive "sURL: ")
  ;; Get title of web page, with the help of functions in url.el
  (with-current-buffer (url-retrieve-synchronously url) ;; find title by grep the html code
    (goto-char 0)
    (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
    (setq web_title_str (match-string 1))
    (goto-char 0)
    (if (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1) ;; find charset by grep the html code
        (setq coding_charset (downcase (match-string 1)))
      (setq coding_charset "utf-8") ;; find the charset, assume utf-8 otherwise
      (setq web_title_str (decode-coding-string web_title_str (intern coding_charset)))) ;; decode the string of title.
    (insert-string "\n\n\n")
    (insert-string web_title_str)
    (insert-string (concat "[[" url "][" web_title_str "]]"))
    (insert-string (format "%s - %s" url web_title_str))
    (insert "\n\n\n")
    (insert web_title_str)
    (insert (concat "[[" url "][" web_title_str "]]"))
    (insert (format "%s - %s" url web_title_str))
    (message (concat "title is: " web_title_str))))

;;; COMMENT: customisations
(defun turn-on-custom-org-bindings ()
  "Activate custom `org-mode' bindings."
  ;; TODO: add binding for `org-insert-custom-file' command
  (define-key org-mode-map (kbd "C-M-j") 'org-insert-heading) ;; NOTE: M-RET inserts a new heading
  ;;(define-key org-mode-map (kbd "C-c p") 'insert-org-paper) ;; NOTE: insert paper template with C-c p
  ;;(define-key org-mode-map (kbd "C-c b") 'insert-org-beamer) ;; NOTE: insert beamer template with C-c b
  ;;(define-key org-mode-map (kbd "C-c c") 'org-insert-citation) ;; NOTE: insert a citation clause
  (define-key org-mode-map (kbd "C-c i") 'org-insert-latex-clause) ;; NOTE: insert a LaTeX clause with C-c i
  (define-key org-mode-map (kbd "C-c f") 'custom-org-insert-footnote) ;; NOTE: insert a footnote with C-c f
  )

(defun turn-on-custom-org ()
  "Activate custom `org-mode' functionality."
  (org-toggle-pretty-entities) ;; NOTE: toggle UTF-8 unicode symbols
  ;;(org-indent-mode) ;; NOTE: indent with headings
  ;;(setq org-startup-indented t) ;; NOTE: indent with headings
  (turn-on-custom-org-bindings)) ;; NOTE: enable custom org-mode bindings

(add-hook 'org-mode-hook (lambda () (turn-on-custom-org)))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;;; COMMENT: images
;; SOURCE: `http://orgmode.org/worg/org-configs/org-config-examples.html'
;;(add-to-list 'iimage-mode-image-regex-alist (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))

;; (defun org-toggle-iimage-in-org ()
;;   "Display images in an `org-mode' buffer."
;;   (interactive)
;;   (if (face-underline-p 'org-link)
;;       (set-face-underline-p 'org-link nil)
;;       (set-face-underline-p 'org-link t))
;;   (iimage-mode))

;; TODO: add `org-toggle-iimage-in-org' to an `org-hook' function

;;; COMMENT: `org-link'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Handling-links.html'
(org-add-link-type "ebib" 'ebib)

;; TODO: add more citation types to ebib
(org-add-link-type "cite" 'ebib
		   (lambda (path desc format)
		     (cond ((eq format 'latex)
			    (format "\\cite{%s}" path)))))

;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Link-abbreviations.html'
;; (setq org-link-abbrev-alist '(("google"   . "http://www.google.com/search?q=")))

;;; COMMENT: word count
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

(provide 'org-config)

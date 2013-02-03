;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/org-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;;; COMMENT: org-mode
;; SOURCE: `http://emacswiki.org/emacs/OrgMode'
;; SOURCE: `http://lists.gnu.org/archive/html/emacs-orgmode/2011-04/msg00761.html'
(autoload 'org-install "org-exp" "Organise tasks with `org-mode'." t)
(autoload 'org-special-blocks "org-special-blocks" "Render blocks of code with `org-mode'." t)
(require 'org-bbdb) ;; TODO: change to `autoload'

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

;;; COMMENT: `org-agenda'
;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html'
(setq ;; org-agenda-include-diary t ;; NOTE: include entries from the emacs diary
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

;;; COMMENT: `org-export'
;; SOURCE: `http://orgmode.org/manual/Exporting.html'
(setq org-export-latex-default-class "paper"
      org-export-with-toc nil ;; NOTE: turn off `org-mode' exporting a table of contents
      org-export-run-in-background t ;; NOTE: run `org-export' tasks in the background
      org-export-with-tasks nil ;; NOTE: turn off `org-mode' exporting tasks
      org-export-with-todo-keywords nil) ;; NOTE: turn off `org-mode' exporting of TODO keywords

;;; COMMENT: `org-capture'
;; SOURCE: `http://orgmode.org/manual/Capture.html'
;; SOURCE: `http://orgmode.org/worg/org-contrib/org-protocol.html'
(autoload 'org-protocol "org-protocol" "Use `org-mode' with `emacsclient'." t)
;; TODO: re-order AND add new tags
(setq org-tag-alist ;; NOTE: list of tags allowed in `org-mode' files
      '(("ASSIGNMENT"       . ?a)
	("BOOKMARK"         . ?b)
	("COMPUTER SCIENCE" . ?c)
	("GENERAL"          . ?g)
	("HOLIDAY"          . ?h)
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
	("WEBSITE"          . ?w)
      )) ;; NOTE: tags for `org-set-tags'

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

;;; COMMENT: capture templates
;; WARNING: do not use 'C' or 'q' characters for binding
(setq org-capture-templates  ;; NOTE: templates for `org-capture'
      '(("a" "Assignment"    ;; NOTE: `assignment' capture
	 plain (file+function (expand-file-name user-org-university-file) course-code)
	 "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	("A" "Anniversary"   ;; NOTE: `anniversary' capture
	 entry (file+headline (concat (expand-file-name user-organisation-directory) "birthday.org") "Birthdays")
	 "** %^{Name} %?%^G\n%^{Birthday}t\n" :empty-lines 1 :immediate-finish 1)
	("b" "Purchase"      ;; NOTE: `purchase' capture
	 table-line (file+headline (concat (expand-file-name user-reading-directory) "readings.org") "Purchase")
	 "| %c | %i | %^{Price} |" :immediate-finish 1) ;; NOTE: capture from `Conkeror'
	("f" "File"          ;; NOTE: `file-bookmark' capture
	 table-line (file+headline (concat (expand-file-name user-organisation-directory) "bookmarks.org") "File Bookmarks")
	 "| [[file:%(if (not (buffer-file-name (get-buffer (car buffer-name-history)))) (dir-path) (file-path))][%(car buffer-name-history)]] |" :immediate-finish 1)
	("g" "General"       ;; NOTE: `general' capture (is a `project')
	 entry (file+headline (expand-file-name user-org-notes-file) "General")
	 "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	("j" "Journal"       ;; NOTE: `journal' capture
	 entry (file+datetree (expand-file-name user-org-notes-file))
	 "* %?\nEntered on %U\n  %i\n  %a")
	("k" "Internet"      ;; NOTE: `internet-bookmark' capture
	 table-line (file+headline (concat (expand-file-name user-organisation-directory) "bookmarks.org") "Internet Bookmarks")
	 "| %c |" :immediate-finish 1) ;; TODO: this *is* `website' capture, right?
	("n" "Note"          ;; NOTE: `note' capture
	 entry (file+headline (expand-file-name user-org-notes-file) "Notes")
	 "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
	("p" "Paper to Read" ;; NOTE: `paper-read' capture
	 table-line (file+headline (concat (expand-file-name user-reading-directory) "readings.org") "Reading")
	 "| %^{Title} | %^{Author} | %^{Year} | %^{Journal} |" :immediate-finish 1)
	;; ("P" "Project"       ;; NOTE: `project' capture (is a `project')
	;;  entry (file+headline (expand-file-name user-org-projects-file) "Projects")
	;;  "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	("P" "Project"
	 plain (file+function (expand-file-name user-org-projects-file) project-capture)
	 "*** TODO %^{Description} %?%^g\n" :empty-lines 1 :immediate-finish 1)  ;; FIX: update the `Project' capture
	("r" "Book to Read"  ;; NOTE: `book-read' capture
	 table-line (file+headline (concat (expand-file-name user-reading-directory) "readings.org") "Reading")
	 "| %^{Title} | %^{Author} | %^{Year} | N/A |" :immediate-finish 1)
	("t" "Travel"        ;; NOTE: `travel' capture (is a `project') ;; COMMENT: finish
	 entry (file+headline (expand-file-name user-org-projects-file) "Travel")
	 "*** TODO %^{Title} %?%^g\n - FROM: %^T\n - UNTIL: %^T\n" :empty-lines 1 :immediate-finish 1)
	("u" "University"    ;; NOTE: `university' capture
	 entry (file+headline (expand-file-name user-org-university-file) "University")
	 "%(add-course)" :empty-lines 1 :immediate-finish 1)
	("w" "Writing"       ;; NOTE: `writing' capture (is a `project') ;; COMMENT: finish
	 entry (file+headline (concat (expand-file-name user-writing-directory) "writings.org") "Writing")
	 "** TODO %^{Title} %?%^g" :empty-lines 1 :immediate-finish 1)))

;; COMMENT: older capture ideas
;; ("T" "test"
;;  plain (file+function (expand-file-name user-org-projects-file) project-capture)
;;  "*** TODO %^{Description} %?%^g\n" :empty-lines 1 :immediate-finish 1) ;; NOTE: No more.
;; ("w" "Website"    ;; NOTE: `website' capture
;;  table-line (file+headline (concat (expand-file-name user-reading-directory) "readings.org") "Websites")
;;  "| [[%^{Link}][%^{Title}]] |" :immediate-finish 1) ;; NOTE: this is redundant

;;; COMMENT: project capture
;; NOTE: this is probably not the best way of doing this
(defvar project-options-alist nil "List of available project types.")

(add-to-list 'project-options-alist "Travel")
;;(add-to-list 'project-options-alist "Ideas")
(add-to-list 'project-options-alist "Programming")
(add-to-list 'project-options-alist "Writing")
(add-to-list 'project-options-alist "General")

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

;;; COMMENT: `org-babel'
;; SOURCE: `http://orgmode.org/worg/org-contrib/babel/intro.html'
(autoload 'org-babel-load-file "org-babel" "Interact with programming languages in `org-mode'." t)
(require 'ob-haskell) ;; NOTE: require `org-babel-haskell'

(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       ;;(common-lisp . t)
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

;;; COMMENT: `org-latex-export'
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-latex-export.html'
(autoload 'org-latex "org-latex" "Render LaTeX with `org-mode'." t)
(autoload 'org-bibtex "org-bibtex" "Bibliographies with `org-mode'." t)
(require 'org-exp-bibtex) ;; TODO: change to `autoload'

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
;;(add-to-list 'org-export-latex-packages-alist '("" "bussproofs")) ;; NOTE: for sequent style proofs
(add-to-list 'org-export-latex-packages-alist '("" "amssymb")) ;; NOTE: mathematics symbols
(add-to-list 'org-export-latex-packages-alist '("" "amsmath")) ;; NOTE: mathematics symbols
(add-to-list 'org-export-latex-packages-alist '("" "hyperref")) ;; NOTE: hyper-references

;;; COMMENT: `org-entities'
;; SOURCE: `http://orgmode.org/manual/Special-symbols.html'
(autoload 'org-entities "org-entities" "Enable unicode support for `org-mode'." t)

(add-to-list 'org-entities-user '("neg"            "\\neg" t "&not;" "[negation]" nil "¬"))
;;(add-to-list 'org-entities-user '("iff"            "\\iff" t "&iff;" "[if and only if]" nil "↔"))
(add-to-list 'org-entities-user '("iff"            "\\iff" t "&iff;" "[if and only if]" nil "\leftrightarrow"))
(add-to-list 'org-entities-user '("top"            "\\top" t "&top;" "[top (true)]" nil "⊤"))
(add-to-list 'org-entities-user '("bot"            "\\bot" t "&bot;" "[bot (false)]" nil "⊥"))
(add-to-list 'org-entities-user '("therefore"      "\\therefore" t "&therefore;" "[therefore]" nil "∴"))
(add-to-list 'org-entities-user '("because"        "\\because" t "&because;" "[because]" nil "∵"))
(add-to-list 'org-entities-user '("derives"        "\\vdash" t "&vdash;" "[derives]" nil "⊢"))
(add-to-list 'org-entities-user '("notderives"     "\\not\\vdash" t "&notvdash" "[notderives]" nil "⊬"))
(add-to-list 'org-entities-user '("models"         "\\models" t "&mod;" "[models]" nil "⊨"))
(add-to-list 'org-entities-user '("notmodels"      "\\not\\models" t "&notmodels" "[notmodels]" nil "⊭"))
(add-to-list 'org-entities-user '("forces"         "\\Vdash" t "&forces" "[forces]" nil "⊩"))
(add-to-list 'org-entities-user '("notforces"      "\\not\\Vdash" t "&notforces" "[notforces]" nil "⊮"))
(add-to-list 'org-entities-user '("boxconditional" "\\boxconditional" t "&boxcond;" "[boxcond]" nil "□→"))
(add-to-list 'org-entities-user '("box"            "\\Box" t "&box;" "[box]" nil "□"))
(add-to-list `org-entities-user '("diamond"        "\\Diamond" t "&diamond;" "[diamond]" nil "◇"))
(add-to-list 'org-entities-user '("cdots"          "\\cdots" t "&cdots;" "[center dots]" nil "⋯"))
(add-to-list 'org-entities-user '("ldots"          "\\ldots" t "&ldots;" "[line dots]" nil "…"))
(add-to-list 'org-entities-user '("reals"          "\\mathbb{R}" t "&reals;" "[real numbers]" nil "ℝ"))
(add-to-list 'org-entities-user '("integers"       "\\mathbb{Z}" t "&integers;" "[integers]" nil "ℤ"))
(add-to-list 'org-entities-user '("primes"         "\\mathbb{P}" t "&primes;" "[prime numbers]" nil "ℙ"))
(add-to-list 'org-entities-user '("naturals"       "\\mathbb{N}" t "&naturals;" "[natural numbers]" nil "ℕ"))
(add-to-list 'org-entities-user '("irrationals"    "\\mathbb{I}" t "&irrationals;" "[irrational numbers]" nil "𝕀"))
(add-to-list 'org-entities-user '("rationals"      "\\mathbb{Q}" t "&rationals;" "[rational numbers]" nil "ℚ"))
(add-to-list 'org-entities-user '("complex"        "\\mathbb{C}" t "&complex;" "[complex numbers]" nil "ℂ"))
;; TODO: ...
;; (add-to-list 'org-entities-user '("box" "\\Box" t "&box;" "[box]" nil "□"))
;; (add-to-list `org-entities-user '("diamond" "\\Diamond" t "&diamond;" "[diamond]" nil "⋄"))
;; (add-to-list 'org-entities-user '("langle" "\\langle" t "&langle;" "[left angle]" nil ""))
;; (add-to-list 'org-entities-user '("rangle" "\\rangle" t "&rangle;" "[right angle]" nil ""))

;; \mathbb{R}		\mathbf{R}		\mathcal{R}		\mathfrak{R}
;; \mathbb{Z}		\mathbf{Z}		\mathcal{Z}		\mathfrak{Z}
;; \mathbb{Q}		\mathbf{Q}		\mathcal{Q}		\mathfrak{Q}

;; TODO: add customizations for \mathcal{}'s

;; COMMENT: ...
(define-skeleton insert-org-latex-package
  "Inserts a LaTeX use-package clause into a document."
  "Insert package name: "
  "#+LATEX_HEADER: \\usepackage{" str "}")

;; COMMENT: `org-mode' custom file templates
(defvar org-template-list (list "beamer" "paper" "assignment") "List of custom template types.")

;;; COMMENT: `org-beamer'
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-beamer/tutorial.html'
;; SOURCE: `http://orgmode.org/manual/Beamer-class-export.html'
(require 'org-beamer) ;; TODO: change to `autoload'

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
;; SOURCE: http://www.gnu.org/software/emacs/manual/html_node/org/Handling-links.html
(org-add-link-type "ebib" 'ebib)

;; TODO: add more citation types to ebib
(org-add-link-type "cite" 'ebib
		   (lambda (path desc format)
		     (cond ((eq format 'latex)
			    (format "\\cite{%s}" path)))))

;; SOURCE: `http://www.gnu.org/software/emacs/manual/html_node/org/Link-abbreviations.html'
(setq org-link-abbrev-alist
       '(("google"   . "http://www.google.com/search?q=")))

(provide 'org-config)

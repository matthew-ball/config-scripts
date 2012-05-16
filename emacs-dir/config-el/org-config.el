;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/org-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 17:12:27 EST

;;; COMMENT: org-mode :: "A GNU Emacs Major Mode for notes, project planning, and authoring."
;; SOURCE: `http://emacswiki.org/emacs/OrgMode'
;; SOURCE: `http://orgmode.org/manual/Special-symbols.html'
;; SOURCE: `http://orgmode.org/worg/org-contrib/org-protocol.html'
;; SOURCE: `http://orgmode.org/worg/org-contrib/babel/'
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-latex-export.html'
;; SOURCE: `http://lists.gnu.org/archive/html/emacs-orgmode/2011-04/msg00761.html'
(autoload 'org-install "org-exp" "Organise tasks with `org-mode'." t)
(autoload 'org-entities "org-entities" "Enable unicode support for `org-mode'." t)
(autoload 'org-protocol "org-protocol" "Use `org-mode' with `emacsclient'." t)
(autoload 'org-babel "org-babel" "Interact with programming languages in `org-mode'." t)
(autoload 'org-special-blocks "org-special-blocks" "Render blocks of code with `org-mode'." t)
(autoload 'org-latex "org-latex" "Render LaTeX with `org-mode'." t)
(autoload 'org-bibtex "org-bibtex" "Bibliographies with `org-mode'." t)

(setq org-support-shift-select 1 ;; NOTE: enable using SHIFT + ARROW keys to highlight text
      org-return-follows-link t ;; NOTE: use RETURN to follow links
      org-log-done 'time ;; NOTE: capture a timestamp for when a task changes state
      org-src-fontify-natively t ;; NOTE: enable fontify in source code blocks
      ;; org-read-date-display-live nil ;; NOTE: disable the live date-display
      ;; org-insert-mode-line-in-empty-file t
      ;; org-indent-mode t ;; NOTE: enable org indent mode
      ;; org-indent-indentation-per-level 2 ;; NOTE: two indents per level
      ;; org-startup-indented t ;; NOTE: indent text in org documents (WARNING: can crash emacs)
      ;; org-odd-levels-only t ;; NOTE: use only odd levels for an outline
      ;; org-hide-leading-stars t ;; NOTE: hide leading stars in a headline
      ;; org-treat-S-cursor-todo-selection-as-state-change nil ;; NOTE: ignore processing
      ;; org-use-property-inheritance t ;; NOTE: children tasks inherit properties from their parent
      ;; org-agenda-include-diary t ;; NOTE: include entries from the emacs diary
      org-deadline-warning-days 7
      org-timeline-show-empty-dates t
      org-completion-use-ido t ;; NOTE: enable `ido-mode' for target (buffer) completion
      org-log-into-drawer 'LOGBOOK ;; NOTE: log changes in the LOGBOOK drawer
      org-refile-target '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)) ;; NOTE: targets include this file and any file contributing to the agenda - up to 5 levels deep
      org-refile-use-outline-path 'file ;; NOTE: targets start with the file name - allows creating level 1 tasks
      org-outline-path-complete-in-steps t ;; NOTE: targets complete in steps - 1. filename 2. <tab> next level of targets
      org-refile-allow-creating-parent-nodes 'confirm ;; NOTE: allow refile to create parent tasks with confirmation
      org-footnote-auto-adjust t ;; NOTE: automatically handle footnotes
      org-agenda-skip-additional-timestamps-same-entry nil ;; NOTE: don't skip multiple entries per day
      org-archive-location (concat (expand-file-name user-org-archive-file) "::* Archives") ;; NOTE: archiving items
      org-use-fast-todo-selection t ;; NOTE: enable fast task state switching
      org-use-tag-inheritance nil ;; NOTE: disable tag inheritance
      org-agenda-dim-blocked-tasks nil ;; NOTE: do not dim blocked tasks
      org-directory (expand-file-name user-organisation-directory) ;; NOTE: default directory for org mode
      org-default-notes-file (expand-file-name user-org-notes-file) ;; NOTE: file for quick notes
      org-modules '(org-modules '(org-bbdb
				  org-bibtex
				  org-crypt
				  org-docview
				  org-gnus
				  org-info
				  org-jsinfo
				  org-irc
				  org-mew
				  org-mhe
				  org-rmail
				  org-vm
				  org-wl
				  org-w3m))
      org-agenda-span 'month ;; NOTE: show a month of agendas
      org-agenda-files `(,(expand-file-name user-org-notes-file)
			 ,(expand-file-name user-org-university-file)
			 ,(expand-file-name user-org-projects-file)
			 ,(concat user-organisation-directory "home.org")
			 ,(concat user-organisation-directory "bookmarks.org")
			 ,(concat user-reading-directory "readings.org")
			 ,(concat user-writing-directory "writings.org")))

(setq org-tag-alist ;; TODO: create customisations
      '(("HOME" . ?h) ("UNIVERSITY" . ?u) ("ASSIGNMENT" . ?a) ("READING" . ?r) ("GENERAL" . ?g) ("PROJECT" . ?p) ("NOTES" . ?n)
	("WEBSITE" . ?w) ("BOOKMARK" . ?b) ("PHILOSOPHY" . ?s) ("COMPUTER SCIENCE" . ?c) ("MATHEMATICS" . ?m) ("WRITING" . ?t)) ;; tags for `org-set-tags'

      org-agenda-custom-commands '(("q" "Show All Tasks" ((agenda "" ((org-agenda-ndays 7) ;; NOTE: overview of tasks
								      (org-agenda-start-on-weekday nil) ;; NOTE: calendar begins today
								      (org-agenda-repeating-timestamp-show-all t)
								      (org-agenda-entry-types '(:timestamp :sexp))))
							  (agenda "" ((org-agenda-ndays 1) ;; NOTE: daily agenda
								      (org-deadline-warning-days 7) ;; NOTE: seven day advanced warning for deadlines
								      (org-agenda-todo-keyword-format "[ ]")
								      (org-agenda-scheduled-leaders '("" ""))
								      (org-agenda-prefix-format "%t%s")))
							  (todo "TODO" ;; NOTE: todos searched by context
								((org-agenda-prefix-format "[ ] %T: ")
								 (org-agenda-sorting-strategy '(tag-up priority-down))
								 (org-agenda-todo-keyword-format "")
								 (org-agenda-overriding-header "\n===========\n All Tasks\n==========="))))
				    ((org-agenda-compact-blocks t)
				     (org-agenda-remove-tags t)))
				   ("h" "Home" ((org-agenda-list nil nil 1) (tags-todo "HOME") (tags-todo "GENERAL")) "HOME"
				    (org-agenda-files '("home.org" "notes.org"))) ;; NOTE: tasks for HOME
				   ("u" "University" ((org-agenda-list nil nil 1) (tags "UNIVERSITY") (tags-todo "ASSIGNMENT")) "UNIVERSITY") ;; tasks for UNIVERSITY
				   ;; TODO: create something similar to the 'q' version (i.e. include a section on Tasks by Context)
				   ("p" "Projects" ((org-agenda-list nil nil 1) (tags-todo "GENERAL") (tags-todo "HOME") (tags-todo "PROJECTS")) "PROJECTS" (org-agenda-files '("home.org" "notes.org" "projects.org"))) ;; NOTE: PROJECT tasks
				   ("g" "General" ((org-agenda-list nil nil 1) (tags-todo "GENERAL") (tags "NOTES")) "GENERAL") ;; NOTE: tasks for GENERAL
				   ("r" "Reading" ((org-agenda-list nil nil 1) (tags "READING") (tags "WEBSITE")) "READING")) ;; NOTE: tasks for READING

      org-capture-templates ;; TODO: replace with custom variables
      '(("h" "Home" entry (file+headline "home.org" "Home") ;; (concat (expand-file-name user-organisation-directory) "home.org")
	 "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	;; ("u" "University" entry (file+headline "school.org" "University")
	;;  "** %^{Course Code} %?%^g\n TITLE: %^{Course Title}\n LECTURER: %^{Lecturer}\n" :empty-lines 1 :immediate-finish 1)
	("u" "University" entry (file+headline "school.org" "University") ;; (expand-file-name user-org-university-file)
	 "%(add-course)" :empty-lines 1 :immediate-finish 1)
	("a" "Assignment" plain (file+function "school.org" course-code) ;; (expand-file-name user-org-university-file)
	 "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	;; ("b" "Book to Purchase" table-line (file+headline "books/books.org" "Books to Purchase") ;; WARNING: out-dated books.org
	;;  "| [[%^{Link}][%^{Title}]] | %^{Author} | %^{Price} |" :immediate-finish 1) ;; manual version
	("b" "Purchase" table-line (file+headline "readings/readings.org" "Purchase") ;; (concat (expand-file-name user-reading-directory) "readings.org")
	 "| %c | %i | %^{Price} |" :immediate-finish 1) ;; conkeror version
	("r" "Book to Read" table-line (file+headline "readings/readings.org" "Reading") ;; (concat (expand-file-name user-reading-directory) "readings.org")
	 "| %^{Title} | %^{Author} | %^{Year} | N/A |" :immediate-finish 1)
	("p" "Paper to Read" table-line (file+headline "readings/readings.org" "Reading") ;; (concat (expand-file-name user-reading-directory) "readings.org")
	 "| %^{Title} | %^{Author} | %^{Year} | %^{Journal} |" :immediate-finish 1)
	("k" "Internet Bookmark" table-line (file+headline "bookmarks.org" "Internet Bookmarks") ;; (concat (expand-file-name user-organisation-directory) "bookmarks.org")
	 "| %c |" :immediate-finish 1)
	("f" "File Bookmark" table-line (file+headline "bookmarks.org" "File Bookmarks") ;; (concat (expand-file-name user-organisation-directory) "bookmarks.org")
	 "| [[file:%(if (not (buffer-file-name (get-buffer (car buffer-name-history)))) (dir-path) (file-path))][%(car buffer-name-history)]] |" :immediate-finish 1)
	("w" "Website" table-line (file+headline "readings/readings.org" "Websites") ;; (concat (expand-file-name user-reading-directory) "readings.org")
	 "| [[%^{Link}][%^{Title}]] |" :immediate-finish 1)
	("j" "Project" entry (file+headline "projects.org" "Projects") ;; (expand-file-name user-org-projects-file)
	 "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	("g" "General" entry (file+headline "notes.org" "General") ;; (expand-file-name user-org-notes-file)
	 "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
	("n" "Note" entry (file+headline "notes.org" "Notes") ;; (expand-file-name user-org-notes-file)
	 "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)))

;;; COMMENT: school organisation
(defun add-course (&rest junk)
  "Capture a course via org-mode's `org-capture'."
  (let ((course-details ""))
    (setq course-details (concat course-details "** " (read-from-minibuffer "Course Code: ") " \t%?%^g\n"
				 " TITLE: " (read-from-minibuffer "Course Title: ") "\n"
				 " LECTURER: " (read-from-minibuffer "Course Lecturer: ") "\n"
				 " LECTURES: \n + %^T : " (read-from-minibuffer "Room Location: ") "\n"))
    (while (string= (read-from-minibuffer "Add Lecture? (y/n): ") "y") ;; this technically lies, y goes into the loop, anything else jumps to tutorial/seminar
      (setq course-details (concat course-details " + %^T  : " (read-from-minibuffer "Room Location: ") "\n")))
    (concat course-details " " (if (string= (read-from-minibuffer "Tutorial or Seminar? (t/s): ") "t") ;; this technically lies, t for "tutorial", any other input means "seminar"
				   "TUTORIAL: "
				 "SEMINAR: ")
	    "\n + %^T : " (read-from-minibuffer "Room Location: ") "\n")))

(defun file-path (&rest junk)
  "Return the path of a file."
  (buffer-file-name (get-buffer (car buffer-name-history))))

(defun dir-path (&rest junk)
  "Return the path of a directory."
  (car (rassq (get-buffer (car buffer-name-history)) dired-buffers)))

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
(org-babel-do-load-languages 'org-babel-load-languages
			     '((R . t)
			       (emacs-lisp . t)
			       (haskell . t)
			       (latex . t) ; this is the entry to activate LaTeX
			       (gnuplot . nil)
			       (perl . nil)
			       (python . nil)
			       (ruby . nil)
			       (screen . nil)
			       (sh . nil)))

(setq org-confirm-babel-evaluate nil) ;; NOTE: don't worry about confirmation before evaluating code
(setq org-src-fontify-natively t) ;; NOTE: fontify source code
(setq org-src-tab-acts-natively t) ;; NOTE: tab works properly

;;; COMMENT: `org-latex-export'
;; SOURCE: `http://orgmode.org/worg/org-tutorials/org-latex-export.html'
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
	     '("paper"
	       "\\documentclass[12pt,a4paper,oneside]{paper}
               \\usepackage{hyperref}
               \\usepackage{amsmath}
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
               \\usepackage{hyperref}
               \\usepackage{amsmath}
               \\usepackage{amssymb}
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
               \\usepackage{hyperref}
               \\usepackage{amsmath}
               \\usepackage{amssymb}
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
               \\usepackage{hyperref}
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

;;; COMMENT: `org-entities'
;; SOURCE: `http://orgmode.org/manual/Special-symbols.html'
(add-to-list 'org-entities-user '("neg" "\\neg" t "&not;" "[negation]" nil "¬"))
(add-to-list 'org-entities-user '("iff" "\\iff" t "&iff;" "[if and only if]" nil "↔"))
(add-to-list 'org-entities-user '("top" "\\top" t "&top;" "[top (true)]" nil "⊤"))
(add-to-list 'org-entities-user '("bot" "\\bot" t "&bot;" "[bot (false)]" nil "⊥"))
(add-to-list 'org-entities-user '("vdash" "\\vdash" t "&vdash;" "[derives]" nil "⊢"))
(add-to-list 'org-entities-user '("models" "\\models" t "&mod;" "[models]" nil "⊨"))
(add-to-list 'org-entities-user '("Box" "\\Box" t "&box;" "[box]" nil "☐"))
(add-to-list 'org-entities-user '("cdots" "\\cdots" t "&cdots;" "[center dots]" nil "⋯"))
(add-to-list 'org-entities-user '("ldots" "\\ldots" t "&ldots;" "[line dots]" nil "…"))
(add-to-list 'org-entities-user '("reals" "\\mathbb{R}" t "&reals;" "[real numbers]" nil "ℝ"))
(add-to-list 'org-entities-user '("integers" "\\mathbb{Z}" t "&integers;" "[integers]" nil "ℤ"))
(add-to-list 'org-entities-user '("primes" "\\mathbb{P}" t "&primes;" "[prime numbers]" nil "ℙ"))
(add-to-list 'org-entities-user '("naturals" "\\mathbb{N}" t "&naturals;" "[natural numbers]" nil "ℕ"))
(add-to-list 'org-entities-user '("irrationals" "\\mathbb{I}" t "&irrationals;" "[irrational numbers]" nil "𝕀"))
(add-to-list 'org-entities-user '("rationals" "\\mathbb{Q}" t "&rationals;" "[rational numbers]" nil "ℚ"))
(add-to-list 'org-entities-user '("complex" "\\mathbb{C}" t "&complex;" "[complex numbers]" nil "ℂ"))

;; \mathbb{R}		\mathbf{R}		\mathcal{R}		\mathfrak{R}
;; \mathbb{Z}		\mathbf{Z}		\mathcal{Z}		\mathfrak{Z}
;; \mathbb{Q}		\mathbf{Q}		\mathcal{Q}		\mathfrak{Q}

;; TODO: add customizations for \mathcal{}'s

;; COMMENT: enable latex source code highlighting
(setq org-export-latex-listings t) ;; NOTE: enable listings features

;; TODO: modify `org-export-latex-packages-alist' (i.e. include some LaTeX packages)
(add-to-list 'org-export-latex-packages-alist '("" "listings")) ;; NOTE: listings package
(add-to-list 'org-export-latex-packages-alist '("" "color")) ;; NOTE: colored source code
(add-to-list 'org-export-latex-packages-alist '("" "bussproofs")) ;; NOTE: for sequent style proofs

;;; COMMENT: Insert a custom file template
(defvar org-custom-file-alist (list "paper" "beamer" "assignment") "List of custom file types for use with `org-mode' documents.")

(define-skeleton insert-org-paper
  "Inserts an `org-mode' paper template."
  "Insert paper title: "
  "#+LATEX_CLASS: paper\n#+OPTIONS: toc:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball, u4537508\n\n* " str "\n* Footnotes\n")

(define-skeleton insert-org-assignment
  "Inserts an `org-mode' assignment template."
  "Insert assignment title: "
  "#+LATEX_CLASS: assignment\n#+OPTIONS: toc:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball, u4537508\n\n* " str "\n* Footnotes\n")

(define-skeleton insert-org-beamer
  "Inserts an `org-mode' beamer presentation template."
  "Insert presentation title: "
  "#+LATEX_CLASS: beamer\n#+LATEX_HEADER: \\usetheme{Warsaw}\n#+OPTIONS: toc:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball, u4537508\n\n* " str "\n* Footnotes\n")

(defun org-insert-custom-file ()
  "Insert custom `org-mode' file template."
  (interactive)
  (let ((custom-file-type (ido-completing-read "Select file type: " org-custom-file-alist)))
    (funcall (intern (concat "insert-org-" custom-file-type)))))

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

;;; COMMENT: `org-ref-man'
;; NOTE: this is the beginning of a sort of "reference manager" extension which utilises org-mode functionality
;; TODO:
;; - Learn `org-bibtex'.
;; - Integrate `org-ref-man' and `org-bibtex'.
(defun generate-paper-entry (file-name) ;; TODO: update this to reflect spreadsheet format
  "Generate an `org-mode' style file link."
  (insert "[[file:" file-name "][" (file-name-sans-extension (file-relative-name file-name)) "]]\n" ))

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
  (define-key org-mode-map (kbd "C-c p") 'insert-org-paper) ;; NOTE: insert paper template with C-c p
  (define-key org-mode-map (kbd "C-c b") 'insert-org-beamer) ;; NOTE: insert beamer template with C-c b
  (define-key org-mode-map (kbd "C-c i") 'org-insert-latex-clause) ;; NOTE: insert a LaTeX clause with C-c i
  (define-key org-mode-map (kbd "C-c f") 'custom-org-insert-footnote)) ;; NOTE: insert a footnote with C-c f

(defun turn-on-custom-org ()
  "Active custom `org-mode' functionality."
  (org-toggle-pretty-entities) ;; NOTE: toggle UTF-8 unicode symbols
  (turn-on-custom-org-bindings)) ;; NOTE: enable custom org-mode bindings

(add-hook 'org-mode-hook (lambda () (turn-on-custom-org)))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)
;; TODO: add an org-post-export-hook function (to delete `latex' buffers)

(provide 'org-config)

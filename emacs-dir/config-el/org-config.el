;; ==================================
;; ~/.emacs.d/config-el/org-config.el
;; Matthew Ball (copyleft 2012)
;; ==================================

;;; org mode
(autoload 'org-install "Organise tasks with org-mode." t)
(autoload 'org-entities "Enable unicode support for org-mode." t)
(autoload 'org-protocol "org-protocol" "Use org-mode with emacsclient." t)
(autoload 'org-latex "org-latex" "Render LaTeX with org-mode." t)
(autoload 'org-special-blocks "org-special-blocks" "Render blocks of code with org-mode." t)
;; (require 'org-protocol) ;; FIXME: change this to an autoload
;; (require 'org-latex) ;; FIXME: change this to an autoload
;; (require 'org-special-blocks) ;; FIXME: change this to an autoload

(setq org-support-shift-select 1 ;; enable using SHIFT + ARROW keys to highlight text
      org-return-follows-link t ;; use RETURN to follow links
      ;; org-read-date-display-live nil ;; disable the live date-display
      org-log-done 'time ;; capture a timestamp for when a task changes state
      ;; org-insert-mode-line-in-empty-file t
      org-deadline-warning-days 7
      org-timeline-show-empty-dates t
      org-completion-use-ido t ;; enable ido for target (buffer) completion
      org-log-into-drawer 'LOGBOOK ;; log changes in the LOGBOOK drawer
      org-refile-target '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)) ;; targets include this file and any file contributing to the agenda - up to 5 levels deep
      org-refile-use-outline-path 'file ;; targets start with the file name - allows creating level 1 tasks
      org-outline-path-complete-in-steps t ;; targets complete in steps so we start with filename, TAB shows the next level of targets etc
      org-refile-allow-creating-parent-nodes 'confirm ;; allow refile to create parent tasks with confirmation
      org-footnote-auto-adjust t ;; automatically handle footnotes
      org-agenda-skip-additional-timestamps-same-entry nil ;; don't skip multiple entries per day
      ;; org-indent-mode t ;; enable org indent mode
      ;; org-indent-indentation-per-level 2 ;; two indents per level
      ;; org-startup-indented t ;; indent text in org documents (WARNING: can crash emacs)
      ;; org-odd-levels-only t ;; use only odd levels for an outline
      ;; org-hide-leading-stars t ;; hide leading stars in a headline
      ;; org-treat-S-cursor-todo-selection-as-state-change nil ;; ignore processing
      ;; org-use-property-inheritance t ;; children tasks inherit properties from their parent
      org-archive-location (concat (expand-file-name user-org-archive-file) "::* Archives") ;; file for archiving items
      ;; org-agenda-include-diary t ;; include entries from the emacs diary
      org-use-fast-todo-selection t ;; enable fast task state switching
      org-use-tag-inheritance nil ;; disable tag inheritance
      org-agenda-dim-blocked-tasks nil ;; do not dim blocked tasks
      org-directory (expand-file-name user-organisation-directory) ;; default directory for org mode
      org-default-notes-file (expand-file-name user-org-notes-file) ;; file for quick notes
      org-agenda-span 'month ;; show a month of agendas
      org-agenda-files `(,(expand-file-name user-org-notes-file)
			 ,(expand-file-name user-org-university-file)
			 ,(expand-file-name user-org-projects-file)
			 ,(concat user-organisation-directory "home.org")
			 ,(concat user-organisation-directory "bookmarks.org")
			 ,(concat user-reading-directory "readings.org")
			 ,(concat user-writing-directory "writings.org")))

(setq org-tag-alist '(("HOME" . ?h) ("UNIVERSITY" . ?u) ("ASSIGNMENT" . ?a) ("READING" . ?r) ("GENERAL" . ?g) ("PROJECT" . ?p) ("NOTES" . ?n)
		      ("WEBSITE" . ?w) ("BOOKMARK" . ?b) ("PHILOSOPHY" . ?s) ("COMPUTER SCIENCE" . ?c) ("MATHEMATICS" . ?m) ("WRITING" . ?t)) ;; tags for `org-set-tags'

      org-agenda-custom-commands '(("q" "Show All Tasks" ((agenda "" ((org-agenda-ndays 7) ;; overview of tasks
								  (org-agenda-start-on-weekday nil) ;; calendar begins today
								  (org-agenda-repeating-timestamp-show-all t)
								  (org-agenda-entry-types '(:timestamp :sexp))))
						      (agenda "" ((org-agenda-ndays 1) ;; daily agenda
								  (org-deadline-warning-days 7) ;; seven day advanced warning for deadlines
								  (org-agenda-todo-keyword-format "[ ]")
								  (org-agenda-scheduled-leaders '("" ""))
								  (org-agenda-prefix-format "%t%s")))
						      (todo "TODO" ;; todos searched by context
							    ((org-agenda-prefix-format "[ ] %T: ")
							     (org-agenda-sorting-strategy '(tag-up priority-down))
							     (org-agenda-todo-keyword-format "")
							     (org-agenda-overriding-header "\n==================\n Tasks by Context\n=================="))))
				    ((org-agenda-compact-blocks t)
				     (org-agenda-remove-tags t)))
				   ("h" "Home" ((org-agenda-list nil nil 1) (tags-todo "HOME") (tags-todo "GENERAL")) "HOME"
				    (org-agenda-files '("home.org" "notes.org"))) ;; tasks for HOME
				   ("u" "University" ((org-agenda-list nil nil 1) (tags "UNIVERSITY") (tags-todo "ASSIGNMENT")) "UNIVERSITY") ;; tasks for UNIVERSITY
				   ("p" "Projects" ((org-agenda-list nil nil 1) (tags-todo "PROJECTS")) "PROJECTS") ;; PROJECT tasks
				   ("g" "General" ((org-agenda-list nil nil 1) (tags-todo "GENERAL") (tags "NOTES")) "GENERAL") ;; tasks for GENERAL
				   ("r" "Reading" ((org-agenda-list nil nil 1) (tags "READING") (tags "WEBSITE")) "READING")) ;; tasks for READING

      org-capture-templates '(("h" "Home" entry (file+headline "home.org" "Home")
			       "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
			      ;; ("u" "University" entry (file+headline "school.org" "University")
			      ;;  "** %^{Course Code} %?%^g\n TITLE: %^{Course Title}\n LECTURER: %^{Lecturer}\n" :empty-lines 1 :immediate-finish 1)
			      ("u" "University" entry (file+headline "school.org" "University")
			       "%(add-course)" :empty-lines 1 :immediate-finish 1)
			      ("a" "Assignment" plain (file+function "school.org" course-code)
			       "*** TODO %^{Title} %?%^g\n DEADLINE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
			      ;; ("b" "Book to Purchase" table-line (file+headline "books/books.org" "Books to Purchase") ;; WARNING: out-dated books.org
			      ;;  "| [[%^{Link}][%^{Title}]] | %^{Author} | %^{Price} |" :immediate-finish 1) ;; manual version
			      ("b" "Purchase" table-line (file+headline "readings/readings.org" "Purchase") ;; TODO: fix file name
			       "| %c | %i | %^{Price} |" :immediate-finish 1) ;; conkeror version
			      ("r" "Book to Read" table-line (file+headline "readings/readings.org" "Reading") ;; TODO: fix file name
			       "| %^{Title} | %^{Author} | %^{Year} | N/A |" :immediate-finish 1)
			      ("p" "Paper to Read" table-line (file+headline "readings/readings.org" "Reading") ;; TODO: fix file name
			       "| %^{Title} | %^{Author} | %^{Year} | %^{Journal} |" :immediate-finish 1)
			      ("k" "Internet Bookmark" table-line (file+headline "bookmarks.org" "Internet Bookmarks")
			       "| %c |" :immediate-finish 1)
			      ("f" "File Bookmark" table-line (file+headline "bookmarks.org" "File Bookmarks")
			       "| [[file:%(if (not (buffer-file-name (get-buffer (car buffer-name-history)))) (dir-path) (file-path))][%(car buffer-name-history)]] |"
			       :immediate-finish 1)
			      ("w" "Website" table-line (file+headline "readings/readings.org" "Websites")
			       "| [[%^{Link}][%^{Title}]] |" :immediate-finish 1)
			      ("j" "Project" entry (file+headline "projects.org" "Projects")
			       "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
			      ("g" "General" entry (file+headline "notes.org" "General")
			       "** TODO %^{Title} %?%^g\n SCHEDULE: %^T\n\n" :empty-lines 1 :immediate-finish 1)
			      ("n" "Note" entry (file+headline "notes.org" "Notes")
			       "** %^{Title} %?%^g\n %^{Text}\n\n" :empty-lines 1 :immediate-finish 1)))

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

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
	     '("paper"
	       "\\documentclass[12pt,a4paper,oneside]{paper}
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
	     '("beamer"
	       "\\documentclass[10pt]{beamer}
               [NO-DEFAULT-PACKAGES]
               [EXTRA]"
	       org-beamer-sectioning))

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

(setq org-export-latex-listings t)

(add-to-list 'org-entities-user '("neg" "\\neg" t "&not;" "[negation]" nil "¬"))
(add-to-list 'org-entities-user '("iff" "\\iff" t "&iff;" "[if and only if]" nil "↔"))
(add-to-list 'org-entities-user '("top" "\\top" t "&top;" "[top (true)]" nil "⊤"))
(add-to-list 'org-entities-user '("bot" "\\bot" t "&bot;" "[bot (false)]" nil "⊥"))
(add-to-list 'org-entities-user '("vdash" "\\vdash" t "&vdash;" "[derives]" nil "⊢"))
(add-to-list 'org-entities-user '("models" "\\models" t "&mod;" "[models]" nil "⊨"))
(add-to-list 'org-entities-user '("Box" "\\Box" t "&box;" "[box]" nil "☐"))
(add-to-list 'org-entities-user '("cdots" "\\cdots" t "&cdots;" "[center dots]" nil "∙"))
;; (add-to-list 'org-entities-user '("ldots" "\\ldots" t "&ldots;" "[line dots]" nil ".")) ;; FIXME: this does not work

;; (add-to-list 'org-entities-user '("newline" "\\newline" t "&newline;" "[new line]" nil "")) ;; TODO: get arrow picture
;; (add-to-list 'org-entities-user '("newpage" "\\newpage" t "&newpage;" "[new page]" nil "")) ;; TODO: get arrow picture

(defun turn-on-custom-org-bindings ()
  "Activate custom org-mode bindings."
  (define-key org-mode-map (kbd "C-M-j") 'org-insert-heading) ;; M-RET inserts a new heading
  (define-key org-mode-map (kbd "C-c p") 'insert-org-paper) ;; insert paper template with C-c p
  (define-key org-mode-map (kbd "C-c b") 'insert-org-beamer)) ;; insert beamer template with C-c b

(defun turn-on-custom-org ()
  "Active custom org-mode functionality."
  (org-toggle-pretty-entities) ;; toggle UTF-8 unicode symbols
  (turn-on-custom-org-bindings)) ;; enable custom org-mode bindings

(add-hook 'org-mode-hook (lambda () (turn-on-custom-org)))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;; TODO: add an org-post-export-hook function

(define-skeleton insert-org-paper
  "Inserts an org-mode paper template."
  "Insert paper title: "
  "#+LATEX_CLASS: paper\n#+OPTIONS: toc:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball, u4537508\n\n* " str "\n* Footnotes\n")

(define-skeleton insert-org-beamer
  "Inserts an org-mode beamer presentation template."
  "Insert presentation title: "
  "#+LATEX_CLASS: beamer\n#+LATEX_HEADER: \\usetheme{Warsaw}\n#+OPTIONS: toc:nil\n\n#+TITLE: " str "\n#+AUTHOR: Matthew Ball, u4537508\n\n* " str "\n* Footnotes\n")

(provide 'org-config)

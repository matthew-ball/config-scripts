;; =================================
;; custom emacs lisp code
;; Matthew Ball (copyleft 2008-2011)
;; =================================

;; ==============
;;; ansi terminal
;; ==============
(defun symbol-value-in-buffer (sym buf)
  "Return the value of 'sym' in 'buf'."
  (save-excursion
    (with-current-buffer buf
      (symbol-value sym))))

(defun start-term (&rest junk)
 "Start an ansi shell in the directory of current buffer."
 (interactive)
 (ansi-term "/bin/bash")
 (term-line-mode))

(defun switch-term (&rest junk)
  "Switch to an active shell (if one exists) or create a new shell (if none exists)."
  (interactive)
  (let ((found nil))
    (loop for b in (buffer-list)
	  if (eq (symbol-value-in-buffer 'major-mode b) 'term-mode)
	  do (switch-to-buffer b) (setq found t))
    (when (not found) (start-term))))

(defun kill-term (&rest junk)
  "Close an ansi shell session and kill the remaining buffer."
  (interactive)
  (when (equal major-mode (or 'term-mode 'eshell-mode))
      (progn
	(term-kill-subjob)
	(kill-buffer))))

;; =============
;;; desktop save
;; =============
(setq desktop-path '("~/.emacs.d/")
      desktop-dirname "~/.emacs.d/"
      desktop-base-file-name "emacs-desktop"
      history-length 250)

(defun saved-session ()
  "Does a previously saved session exist?"
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

(defun session-restore () ;; use session-restore to restore the desktop manually
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun session-save () ;; use session-save to save the desktop manually
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
    (desktop-save-in-desktop-dir)))

(add-hook 'after-init-hook ;; ask user whether to restore desktop at start-up
	  '(lambda () (if (saved-session)
		     (if (y-or-n-p "Restore desktop? ")
			 (session-restore)))))

(defun emacs-process-p (pid) ;; over-ride stale lock
  "If pid is the process ID of an emacs process, return t, else nil. Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t) pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(add-hook 'kill-emacs-query-functions (lambda () (session-save) t)) ;; save sessions automatically each time we kill emacs

;; =============
;;; custom LaTeX
;; =============
(define-skeleton bold ;; insert a bold text clause in a LaTeX document
  "Insert a bold clause in a LaTeX document."
  "String: "
  "\\textbf{" str | "insert text" "}")

(define-skeleton footnote ;; insert a footnote in a LaTeX document
  "Insert a footnote in a LaTeX document."
  "String: "
  "\\footnote{" str | "insert text" "}")

(define-skeleton tele-type ;; insert a tele-type text clause in a LaTeX document
  "Insert a tele-type text clause in a LaTeX document."
  "String: "
  "\\texttt{" str | "insert text" "}")

(define-skeleton emph ;; insert an emphasis text clause in a LaTeX document
  "Inserts an emphasis clause in a LaTeX document."
  "String: "
  "\\emph{" str | "insert text" "}")

(define-skeleton small-capitals ;; inserts a small capitals text clause in a LaTeX document
  "Inserts a small capital text clause in a LaTeX document."
  "String: "
  "\\textsc{" str | "insert text" "}")

;; ===========
;;; word count
;; ===========
(defun word-count (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "wc -w"))

;; =======================
;;; org-remember templates
;; =======================
(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler)
      org-remember-templates
      '(("Home" ?h "\n* TODO %^{Title} :HOME:\n + ADDED: %U\n + SCHEDULE: %^T%?\n" "home.org" "Home") ;; add a TODO for a task at home
	("Course" ?C "\n* %^{Course Code} :UNIVERSITY:\n + TITLE: %^{Course Title}\n + ADDED: %U%?\n" "school.org" "University") ;; add a new entry for a COURSE at university
	("Assignment" ?A "\n* TODO %^{Title} :ASSIGNMENT:\n + ADDED: %U\n + DEADLINE: %^T%?\n" "school.org" "%^{Course Code}") ;; add a TODO for a COURSE at university
	("Projects" ?p "\n* TODO %^{Title} :PROJECT:\n + ADDED: %U\n + SCHEDULE: %^T%?\n" "projects.org" "Projects") ;; add a TODO for a project task
	("General" ?g "\n* TODO %^{Title} :GENERAL:\n + ADDED: %U\n + SCHEDULE: %^T%?\n" "notes.org" "General") ;; add a TODO for a general task
	("Website" ?w "\n* %^{Title} :WEBSITE:\n + ADDED: %U\n + LINK: [[%^{Address}][%^{Name}]]\n%?\n" "notes.org" "Notes") ;; add an interesting web site
	("Notes" ?n "\n* %^{Title} :NOTES:\n + ADDED: %U\n + %^{Text}\n%?" "notes.org" "Notes"))) ;; add an interesting note

(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; =================
;;; org-mode journal
;; =================
(defvar org-journal-file "~/Documents/Organisation/journal.org" "Path to org-mode journal file.")

(defvar org-journal-date-format "%Y-%m-%d" "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (unless (org-goto-local-search-headings today nil t)
      ((lambda ()
         (org-insert-heading)
         (insert today)
         (insert "\n\n  \n"))))
    (beginning-of-buffer)
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (backward-char 2)
    (unless (= (current-column) 2)
      (insert "\n\n  "))))

;; =============
;;; abbreviation
;; =============
(setq abbrev-file-name "~/.emacs.d/abbrev_defs.el") ;; set abbreviation file
(abbrev-mode t) ;; enable abbreviations
(setq default-abbrev-mode t ;; turn on abbrev-mode
      save-abbrevs 'ask ;; ask to save abbreviations
      dabbrev-case-replace nil) ;; preserve case when expanding

(read-abbrev-file abbrev-file-name t)
(add-hook 'kill-emacs-hook 'write-abbrev-file) ;; update abbreviations when exiting emacs

;; =========================
;;; code folding (hide/show)
;; =========================
(hs-minor-mode 1)

(setq hs-hide-comments nil ;; hide the comments too when you do a 'hs-hide-all'
      hs-isearch-open 'code) ;; set whether isearch opens folded comments, code, or both

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))

;; =========================
;;; show dot emacs structure
;; =========================
(defun string-repeat (str n)
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str))) retval))

(defun show-dot-emacs-structure (&rest junk)
  "Show the outline structure of ~/.emacs."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
      (progn
	(occur "^;;;+")
	(other-window 1))))

;; ======
;;; dired
;; ======
(defun dired-launch-command ()
  (interactive)
  (dired-do-shell-command
   (case system-type
     (gnu/linux "gnome-open")) ;; works for gnome (ubuntu), not for other systems
   nil
   (dired-get-marked-files t current-prefix-arg)))

(setq dired-load-hook (lambda () (define-key dired-mode-map (kbd "l") 'dired-launch-command))) ;; launch custom applications with dired

;; ============
;;; try-require
;; ============
(defvar missing-packages-list nil "List of packages that 'try-require' can't find.")

(defun try-require (feature) ;; attempt to load a feature/library, failing silently
  "Attempt to load a library or module. Return true if the library given as argument is successfully loaded.
If not, instead of an error, just add the package to a list of missing packages."
  (condition-case err
      (progn ;; protected form
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    (file-error  ;; condition
     (progn ;; error handler
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

;; ==============
;;; auto-complete
;; ==============
(define-globalized-minor-mode real-global-auto-complete-mode ;; dirty fix for having AC everywhere
  auto-complete-mode (lambda () (if (not (minibufferp (current-buffer)))
			       (auto-complete-mode 1))))
(real-global-auto-complete-mode t)

;; ========
;;; ibuffer
;; ========
(defun ibuffer-get-current-buffers (&optional including-empty-groups sort-mode)
  "Return active ibuffer filter groups."
  (with-temp-buffer
    (flet ((message (f &rest args)))
      (ibuffer nil (current-buffer)))
    (let* ((ibuffer-default-sorting-mode (or sort-mode 'alphabetic))
           (buffers (ibuffer-generate-filter-groups (ibuffer-current-state-list))))
      (remove-if-not (lambda (x) (if including-empty-groups t (cdr x))) buffers))))

(defun ibuffer-get-current-group ()
  "Return current ibuffer filter group."
  (remove-if-not (lambda (group)
                   (remove-if-not (lambda (buffer) (equal (current-buffer)
                                                     (car buffer))) (cdr group)))
                 (ibuffer-get-current-buffers)))

(defun ibuffer-next-buffer ()
  "Change to the next buffer in the current ibuffer filter group."
  (interactive)
  (let* ((all (ibuffer-get-current-group))
         (buffers (cdar all))
         (current (current-buffer))
         (next (mod (loop for b in buffers summing 1 into i
                          when (equal (car b) current) return i) (length buffers)))
         (names (loop for b in buffers and i = 0 then (1+ i)
		      collecting (if (= i next) (list (car b)) (car b)))))
    (set-window-buffer (selected-window) (car (nth next buffers)))
    (message "%s: %s" (caar all) names)))

(defun ibuffer-next-group ()
  "Change to the next ibuffer filter group."
  (interactive)
  (let* ((group (caar (ibuffer-get-current-group)))
         (groups (ibuffer-get-current-buffers nil 'recency))
	 (ignore '("Default" "Help" "Completions" "Regular Expressions" "Bookmarks" "EmacsLisp Package Archiver")) ;; ignore these groups
	 (groups (remove-if (lambda (n) (member (car n) ignore)) groups))
         (next (nth (loop for g in (mapcar (lambda (n) (car n)) groups) summing 1 into i
                          when (string= g group) return (mod i (length groups))) groups))
         (names (mapcar (lambda (n) (if (string= (car next) (car n))
				   (list (car n)) (car n))) groups)))
    (set-window-buffer (selected-window) (caadr next))
    (message "%s" names)))

;; =================================
;; custom stumpwm lisp code
;; Matthew Ball (copyleft 2011)
;; =================================
;; old deprecated code ...
;; ================
;;; OLD KEYBINDINGS
;; ================
;; defined root bindings ...
;; (define-key *root-map* (kbd "B") "chromium") ;; open (or switch to an existing instance of) chromium
;; (define-key *root-map* (kbd "T") "terminal") ;; open (or switch to an existing instance of) terminal
;; (define-key *root-map* (kbd "H") "htop") ;; open (or switch to an existing instance of) htop
;; (define-key *root-map* (kbd "M") "mutt") ;; open (or switch to an existing instance of) mutt
;; (define-key *root-map* (kbd "I") "irssi") ;; open (or switch to an existing instance of) irssi
;; (define-key *root-map* (kbd "E") "emacs") ;; open (or switch to an existing instance of) emacs
;; (define-key *root-map* (kbd ".") "menu") ;; open quick-menu
;; (define-key *root-map* (kbd "s-w") "wikipedia") ;; quick search wikipedia
;; (define-key *root-map* (kbd "s-g") "google") ;; quick search google
;; (define-key *root-map* (kbd "M-b") "show-battery") ;; show battery status
;; (define-key *root-map* (kbd "M-i") "show-window-properties") ;; show current window's properties
;; (define-key *root-map* (kbd "DEL") "repack-window-numbers") 

;; define toplevel bindings (these bindings don't require the prefix key) ...
;; (define-key *top-map* (kbd "M-1") "do-something") ;; WARNING: will conflict with irssi

;; =============
;;; OLD COMMANDS
;; =============
;; (defcommand mutt () ()
;;   "Run an instance of `mutt'."
;;   (run-or-raise "gnome-terminal -t \"mutt\" -e \"mutt -n\"" '(:title "mutt")))

;; (defcommand emacs () ()
;;   "Run an instance of `emacs'."
;;   (run-or-raise "gnome-terminal -t \"emacs\" -e \"emacs\"" '(:title "emacs")))

;; (defcommand htop () ()
;;   "Run an instance of `htop'."
;;   (run-or-raise "gnome-terminal -t \"htop\" -e \"htop\"" '(:title "htop")))

;; (defcommand irssi () ()
;;   "Run an instance of `irssi'."
;;   (run-or-raise "gnome-terminal -t \"irssi\" -e \"irssi\"" '(:title "irssi")))

;; (defcommand aptitude () ()
;;   "Run an instance of `aptitude'."
;;   (run-or-raise "gnome-terminal -t \"aptitude\" -e \"aptitude\"" '(:title "aptitude")))

;; (defcommand stumpish () ()
;;   "Run an instance of `stumpish'."
;;   (run-or-raise "gnome-terminal -t \"stumpish\" -e \"stumpish\"" '(:title "stumpish")))

;; ===============
;;; BATTERY STATUS
;; ===============
;; (defun show-battery-charge ()
;;   (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t))) 
;;     (substitute #\Space #\Newline raw-battery)))

;; ================
;;; old keybindings
;; ================
;; ("B" "chromium-browser") ;; temp
;; ("E" "emacs") ;; temp
;; ("T" "gnome-terminal") ;; temp
;; ("." "menu") ;; open quick-menu

;; =============
;;; old launches
;; =============
;; (quick-launch-non-cli chromium-browser '(:instance "chromium-browser")) ;; temp
;; (quick-launch-non-cli emacs '(:instance "emacs")) ;; temp
;; (quick-launch-non-cli gnome-terminal '(:title "terminal")) ;; temp
;; (quick-launch-cli irssi '(:title "irssi"))
;; (quick-launch-cli mutt '(:title "mutt") -n)

;; ======================
;;; old frame-preferences
;; ======================
;; (define-frame-preference "writing" ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t t :title *terminal*) ;; TODO: fix
;;   (0 t t :instance *editor*)) ;; TODO: fix

;; (define-frame-preference "internet" ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t t :instance *browser*)) ;; TODO: fix

;; =============
;;; quick launch
;; =============
;; (defmacro quick-launch-non-cli (cmd prop) ;; quick launch non-CLI command
;;   (let ((cmd-str (string-downcase (symbol-name cmd))))
;;     `(defcommand ,cmd () ()
;;        (run-or-raise ,cmd-str ,prop))))

;; TODO: clean up this macro ...
;; (defmacro quick-launch-cli (cmd prop &optional args) ;; quick launch CLI command
;;   (let ((cmd-str (string-downcase (symbol-name cmd)))
;; 	(args-str (string-downcase (symbol-name args))))
;;     (if (null args)
;; 	(progn
;; 	  `(defcommand ,cmd () ()
;;      	     (run-or-raise (concatenate 'string *terminal* " -t \"" ,cmd-str "\" -e \"" ,cmd-str "\"") ,prop)))
;;       (progn
;; 	`(defcommand ,cmd () ()
;; 	   (run-or-raise (concatenate 'string *terminal* " -t \"" ,cmd-str "\" -e \"" ,cmd-str " " ,args-str "\"") ,prop))))))

;; (quick-launch-non-cli referencer '(:instance "referencer"))

;; (defcommand ask-user (&optional (initial "")) (:rest) ;; prompt the user for an interactive command. The first arg is an optional initial contents.
;;   (let ((cmd (read-one-line (current-screen) ": " initial)))
;;     (when cmd (eval-command cmd t))))
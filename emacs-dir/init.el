
(eval-when-compile (require 'cl-lib))

(defgroup user-variables nil "User variables.")
(defcustom user-config-directory "~/.config-scripts/" "Directory for user's configuration files." :type 'directory :group 'user-variables)
(defcustom user-projects-directory "~/Public/" "Directory for user's project files." :type 'directory :group 'user-variables)
(defcustom user-documents-directory "~/Documents/" "Directory for user's files." :type 'directory :group 'user-variables)
(defcustom user-notes-file (concat (expand-file-name user-documents-directory) "notes.org") "File for user's notes." :type 'file :group 'user-variables)
(defcustom user-packages-list '(magit gist markdown-mode undo-tree browse-kill-ring yasnippet auto-complete diminish) "List of user packages." :type '(repeat symbol) :group 'user-variables)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(file-name-shadow-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)
(global-prettify-symbols-mode 1)
(global-visual-line-mode 1)
(midnight-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(desktop-save-mode 1)
(fringe-mode '(nil . 1))

(setq show-paren-delay 1.0
	  inhibit-startup-message 1
	  inhibit-echo-area-message 1
	  read-buffer-completion-ignore-case 1
	  read-file-name-completion-ignore-case 1
	  disabled-command-function nil
	  use-dialog-box nil
	  confirm-nonexistent-file-or-buffer nil
	  custom-file (concat (expand-file-name user-emacs-directory) "custom.el")
	  visual-line-fringe-indicators '(left-curly-arrow nil)
	  uniquify-buffer-name-style 'reverse
	  uniquify-separator "/"
	  uniquify-ignore-buffers-re "^\\*"
	  sentence-end-double-space nil
	  x-stretch-cursor t)

(setq-default tab-width 4
			  show-trailing-whitespace 1
			  delete-old-versions t)

(load custom-file t)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defun user-packages-install ()
  (dolist (package user-packages-list)
	(unless (package-installed-p package)
	  (package-install package))))

(unless package-archive-contents
  (package-refresh-contents)
  (user-packages-install))

(require 'ido)

(ido-mode 1)
(ido-everywhere 1)

(setq ido-enable-flex-matching 1
	  ido-use-virtual-buffers 1
	  ido-create-new-buffer 'always
	  ido-show-dot-for-dired 1)

(require 'ibuffer)
(require 'ibuf-ext)

(setq ibuffer-show-empty-filter-groups nil
	  ibuffer-default-sorting-mode 'filename/process
	  ibuffer-expert 1
	  ibuffer-old-time 24
	  ibuffer-saved-filter-groups `(("default"
									 ("Configuration"
									  (or (filename . ,(expand-file-name user-emacs-directory))
										  (filename . ,(expand-file-name user-config-directory))))
									 ("Projects"
									  (filename . ,(expand-file-name user-projects-directory)))
									 ("Documents"
									  (filename . ,(expand-file-name user-documents-directory))))))

(defvar ibuffer-default-collapsed-groups '("Default") "Filter groups to be collapsed by default.")

(defadvice ibuffer (after collapse)
  (dolist (group ibuffer-default-collapsed-groups)
	(progn
	  (goto-char 1)
	  (when (search-forward (concat "[ " group " ]") (point-max) t)
		(progn
		  (move-beginning-of-line nil)
		  (ibuffer-toggle-filter-group)))))
  (goto-char 1)
  (search-forward "[ " (point-max) t))

(ad-activate 'ibuffer)

(defun custom-ibuffer-mode ()
  (ibuffer-auto-mode)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook #'custom-ibuffer-mode)

(require 'dired)
(require 'dired-x)

(setq dired-listing-switches "--color=auto -DaGghlv --group-directories-first --time-style=long-iso"
	  dired-dwim-target 1
	  dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

(defun custom-dired-bindings ()
  (define-key dired-mode-map (kbd "<return>") #'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") #'(lambda () (interactive) (find-alternate-file ".."))))

(defun custom-dired-mode ()
  (custom-dired-bindings)
  (dired-hide-details-mode)
  (dired-omit-mode))

(add-hook 'dired-mode-hook #'custom-dired-mode)

(require 'ispell)

(setq ispell-dictionary "en_GB")

(require 'flyspell)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(require 'hl-line)

(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-indent)
(require 'ox-latex)
(require 'ox-beamer)

(add-to-list 'org-latex-classes
			 '("paper"
			   "\\documentclass[12pt,a4paper,oneside]{paper}
\\setcounter{secnumdepth}{0}
[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-return-follows-link 1
	  org-completion-use-ido 1
	  org-hide-emphasis-markers 1
	  org-directory user-documents-directory
	  org-default-notes-file user-notes-file
	  org-agenda-inhibit-startup 1
	  org-agenda-span 'month
	  org-agenda-files `(,(expand-file-name user-notes-file))
	  org-agenda-skip-scheduled-if-done t
	  org-agenda-skip-deadline-if-done t
	  org-support-shift-select t
	  org-confirm-babel-evaluate nil
	  org-special-ctrl-a/e t
	  org-special-ctrl-k t
	  org-special-ctrl-o t
	  org-ctrl-k-protect-subtree t
	  org-yank-adjusted-subtrees t
	  org-fontify-done-headline t
	  org-src-fontify-natively 1
	  org-src-tab-acts-natively 1
	  org-tag-alist '(("NOTES" . ?n) ("TASKS" . ?t) ("PROJECTS" . ?p) ("UNIVERSITY" . ?u))
	  org-capture-templates '(("N" "Note" entry (file+headline (expand-file-name user-notes-file) "Notes") "*** %^{Title}\n%^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
							  ("T" "Task" entry (file+headline (expand-file-name user-notes-file) "Tasks") "*** TODO %^{Description}\n%^{Text}\n\n" :empty-lines 1 :immediate-finish 1)
							  ("P" "Project" entry (file+headline (expand-file-name user-notes-file) "Projects") "*** TODO %^{Description}\n\n" :empty-lines 1 :immediate-finish 1)
							  ("U" "University" entry (file+headline (expand-file-name user-notes-file) "University") "*** TODO %^{Description} %^g\nDEADLINE: %^t\n" :empty-lines 1 :immediate-finish 1))
	  org-latex-default-class "article"
	  org-latex-with-hyperref nil
	  org-highlight-latex-and-related '(latex script entities)
	  org-export-with-toc nil
	  org-export-with-tasks nil
	  org-export-with-todo-keywords nil
	  org-export-with-smart-quotes t)

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "bussproofs"))

(org-babel-do-load-languages 'org-babel-do-load-languages '((emacs-lisp . t)
															(sh . t)
															(latex-mode . t)))

(defun surrounded-by-p (char)
  (save-excursion
	(and (forward-word -1)
		 (equal char (char-before))
		 (forward-word 1)
		 (equal char (char-after)))))

(defun surround-word (char &optional force)
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

(defmacro org-propertise-word (prop char)
  `(defun ,(intern (format "org-%s-word" prop)) (&optional force)
	 (interactive "p")
	 (surround-word ,char force)))

(org-propertise-word bold ?*)
(org-propertise-word italic ?/)
(org-propertise-word underline ?_)
(org-propertise-word verbatim ?~)
(org-propertise-word teletype ?=)

(define-skeleton org-paper-skeleton
  "Insert an `org-mode' skeleton for a paper."
  "Title: "
  "#+TITLE: " str | "Title" "\n"
  "#+AUTHOR: " (user-full-name) "\n"
  "#+OPTIONS: date:nil num:nil\n"
  "\n"
  "* Introduction\n"
  "* Footnotes\n")

(defun custom-org-bindings ()
  (define-key org-mode-map (kbd "C-c b") #'org-bold-word)
  (define-key org-mode-map (kbd "C-c i") #'org-italic-word)
  (define-key org-mode-map (kbd "C-c u") #'org-underline-word)
  (define-key org-mode-map (kbd "C-c v") #'org-verbatim-word)
  (define-key org-mode-map (kbd "C-c t") #'org-teletype-word)
  (define-key org-mode-map (kbd "C-c s") #'org-paper-skeleton))

(defun custom-org-mode ()
  (custom-org-bindings)
  (org-indent-mode)
  (org-toggle-pretty-entities))

(add-hook 'org-mode-hook #'custom-org-mode)
(add-hook 'org-agenda-mode-hook #'hl-line-mode 'append)

(require 'hideshow)

(defun display-line-count (ov)
  (when (eq 'code (overlay-get ov 'hs))
	(overlay-put ov 'display (format "... / %d" (count-lines (overlay-start ov) (overlay-end ov))))))

(setq hs-hide-comments-when-hiding-all nil
	  hs-isearch-open 'code
	  hs-set-up-overlay #'display-line-count)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(require 'eldoc)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(defun complete-string (source)
  (cond
   ((fboundp #'auto-complete) (auto-complete source))
   (t (dabbrev-expand source))))

(defun smart-tab ()
  (interactive)
  (if (minibufferp)
	  (unless (minibuffer-complete)
		(complete-string nil))
	(if mark-active
		(indent-region (region-beginning) (region-end))
	  (if (looking-at "\\_>")
		  (complete-string nil)
		(indent-for-tab-command)))))

(load-theme 'misterioso t)

;; user packages
(browse-kill-ring-default-keybindings)

(ac-config-default)

(add-hook 'text-mode-hook #'undo-tree-mode)
(add-hook 'prog-mode-hook #'undo-tree-mode)

(undo-tree-mode 1)
(yas-global-mode 1)
(global-auto-complete-mode 1)

(setq browse-kill-ring-highlight-inserted-item t
	  undo-tree-visualizer-diff 1
	  undo-tree-visualizer-timestamps 1
	  yas-snippet-dirs `(,(concat (expand-file-name user-emacs-directory) "snippets"))
	  yas-triggers-in-field 1)

(defun ac-add-yasnippet-source ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'prog-mode-hook #'ac-add-yasnippet-source)
(add-hook 'text-mode-hook #'auto-complete-mode)

(require 'autorevert)
(require 'with-editor)

(diminish 'flyspell-mode)
(diminish 'visual-line-mode)
(diminish 'hs-minor-mode)
(diminish 'eldoc-mode)
(diminish 'org-indent-mode)
(diminish 'yas-minor-mode)
(diminish 'auto-complete-mode)
(diminish 'abbrev-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'with-editor-mode)

(global-set-key (kbd "TAB") #'smart-tab)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c g b") #'gist-buffer)
(global-set-key (kbd "C-c g r") #'gist-region)
(global-set-key (kbd "M-+") #'hs-toggle-hiding)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x p") #'browse-kill-ring)
(global-set-key (kbd "C-z") #'undo-tree-visualize)

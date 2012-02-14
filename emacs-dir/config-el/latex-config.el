;; FILE: ~/.emacs.d/config-el/latex-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: latex
(autoload 'latex-mode "tex-mode" "LaTeX major mode for GNU Emacs." t)
(autoload 'reftex-mode "reftex" "RefTeX minor mode for GNU Emacs." t)
(autoload 'turn-on-reftex "reftex" "RefTeX minor mode for GNU Emacs." t)
(autoload 'reftex-citation "reftex-cite" "RefTeX inert citation." nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "RefTeX phrase mode." t)

(defun org-mode-article-modes ()
  (reftex-mode t)
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all)))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

(defun org-mode-custom-latex ()
  "Enable custom settings for org-mode."
  (turn-on-reftex) ;; turn on reftex
  (org-mode-reftex-setup) ;; enable org-mode/reftex integration
  (lambda () (if (member "REFTEX" org-todo-keywords-1 (org-mode-article-modes)))))

;; (add-hook 'org-mode-hook 'org-mode-custom-latex)

;;; COMMENT: reftex formats (for biblatex)
(setq reftex-cite-format
      '((?c . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?x . "[]{%l}")
        (?X . "{%l}")))

(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("cites" "[{}]")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{")
        ("citetitle" "[{")
        ("citetitles" "[{")
        ("headlessfullcite" "[{")))

(setq reftex-enable-partial-scans t ;; make reftex faster
      reftex-save-parse-info t ;; save the information gathered while reading a file
      reftex-use-multiple-selection-buffers t ;; use a separate buffer for each selection type
      reftex-default-bibliography '("default.bib" "other-default.bib") ;; default bibliography file
      reftex-cite-prompt-optional-args nil
      reftex-cite-cleanup-optional-args t
      reftex-extra-bindings t ;; enable extra reftex bindings
      latex-run-command "pdflatex") ;; use `pdflatex' to compile LaTeX documents

(defun latex-bibtex-file (&rest junk)
  "Produce a bibliography for current LaTeX document."
  (interactive)
  (call-process "bibtex" nil 0 nil (file-name-sans-extension (buffer-file-name))) ;; execute asynchronous bibtex process
  (message (concat "bibtex called on " (buffer-file-name))))

(defun latex-file (&rest junk)
  "Produce current LaTeX document as PDF."
  (interactive)
  (tex-file)
  (switch-to-buffer (current-buffer)) ;; refocus on main window
  (delete-other-windows)) ;; delete remaining window

(defun latex-view (&rest junk)
  "Produce and preview current LaTeX document as PDF."
  (interactive)
  (tex-file)
  (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (message pdf-file) ;; print pdf-file in minibuffer
    (call-process "evince" nil 0 nil pdf-file)) ;; execute asynchronous evince process
    (switch-to-buffer (current-buffer)) ;; refocus on main window
    (delete-other-windows)) ;; delete remaining window

(defmacro latex-skeleton (code) ;; inserts a custom clause skeleton in a LaTeX document
  (let ((func (intern (concat "latex-skeleton-" code)))
	(doc (format "Inserts a %s clause in a LaTeX document." code)))
    `(define-skeleton ,func ,doc "String: "
       "\\" ,code  "{" str | "insert text" "}")))

(defun latex-smart-underscore ()
  "Smart \"_\" key: insert \"_{}\". If the underscore key is pressed a second time, \"_{}\" is removed and replaced by the underscore."
  (interactive)
  (let ((assign-len (length "_{")))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;; check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at "_{}")))
        (progn ;; if we are currently looking at ess-S-assign, replace it with _
          (forward-char)
          (delete-backward-char (+ 1 assign-len))
          (insert "_"))
      (delete-horizontal-space)
      (insert "_{}")
      (backward-char))))

(defun latex-smart-caret ()
  "Smart \"^\" key: insert \"^{}\". If the caret key is pressed a second time, \"^{}\" is removed and replaced by the caret."
  (interactive)
  (let ((assign-len (length "^{")))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;; check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at "\\^{}"))) ;; looking-at reads regexp, so need to escape the caret character
        (progn ;; if we are currently looking at ess-S-assign, replace it with ^
          (forward-char)
          (delete-backward-char (+ 1 assign-len))
          (insert "^"))
      (delete-horizontal-space)
      (insert "^{}")
      (backward-char))))

(defun latex-smart-period ()
  "Smart \".\" key: insert \".  \n\". If the period key is pressed a second time, \".  \n\" is removed and replaced by the period."
  (interactive)
  (let ((assign-len (length ".  %%\n")))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;; check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at "\\.  %%")))
        (progn ;; if we are currently looking at ess-S-assign, replace it with _
          (delete-backward-char assign-len)
          (insert "."))
      (delete-horizontal-space)
      (insert ".  %%\n"))))

(latex-skeleton "textbf") ;; inserts a bold clause
(latex-skeleton "footnote") ;; inserts a footnote
(latex-skeleton "texttt") ;; inserts a tele-type clause
(latex-skeleton "emph") ;; inserts an emphasis clause
(latex-skeleton "textsc") ;; inserts a small capitals clause

(defun turn-on-custom-latex-bindings ()
  "Activate custom LaTeX bindings."
  (define-key latex-mode-map (kbd "C-c C-v") 'latex-view) ;; enable latex-view
  (define-key latex-mode-map (kbd "C-c C-f") 'latex-file) ;; enable latex-file
  (define-key latex-mode-map (kbd "C-c C-b") 'latex-bibtex-file) ;; enable latex-bibtex-file
  (define-key latex-mode-map (kbd "C-c b") 'latex-skeleton-textbf) ;; bind bold to keyboard
  (define-key latex-mode-map (kbd "C-c f") 'latex-skeleton-footnote) ;; bind footnote to keyboard
  (define-key latex-mode-map (kbd "C-c t") 'latex-skeleton-texttt) ;; bind tele-type to keyboard
  (define-key latex-mode-map (kbd "C-c e") 'latex-skeleton-emph) ;; bind emphasise to keyboard
  (define-key latex-mode-map (kbd "C-c s") 'latex-skeleton-textsc) ;; bind small-capitals to keyboard
  (define-key latex-mode-map (kbd "_") 'latex-smart-underscore) ;; bind _ to smart underscore
  (define-key latex-mode-map (kbd "^") 'latex-smart-caret) ;; bind ^ to smart caret
  (define-key latex-mode-map (kbd ".") 'latex-smart-period)) ;; bind . to smart period

(defun turn-on-custom-latex ()
  "Activate custom LaTeX functionality."
  (turn-on-reftex) ;; enable reftex mode
  (turn-on-custom-latex-bindings) ;; enable custom latex bindings
  (setq ispell-parser 'tex))

(add-hook 'latex-mode-hook (lambda () (turn-on-custom-latex)))

(provide 'latex-config)

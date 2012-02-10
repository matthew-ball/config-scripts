;; ~/.emacs.d/config-el/eshell-config.el
;; Matthew Ball (copyleft 2012)

;;; COMMENT: eshell
(require 'ansi-color)
(require 'eshell)
(require 'esh-opt)
(require 'em-smart)
(require 'em-cmpl)
(require 'em-prompt)
(require 'em-term)

;; (autoload 'ansi-color "ansi-color" "ANSI colour library for GNU Emacs lisp." t)
;; (autoload 'eshell "eshell" "GNU Emacs Shell." t)
;; (autoload 'em-smart "esh-mode" "..." t)

(setenv "PAGER" "cat")

(defun eshell/git-branch (&rest junk)
  "Return the current git branch, if applicable."
  (let ((branch (shell-command-to-string "git branch")))
    (string-match "^\\* \\(.*\\)" branch)
    (match-string 1 branch)))

(defun eshell/emacs (&rest args)
  "Open a list of files in emacs."
  (if (null args)
      (bury-buffer) ;; return to emacs
    ;; have to expand the file names or else naming a directory in an argument causes later arguments to be looked for in that directory, not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/clear (&rest junk)
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/deb (&rest args)
  "Interface with a debian system."
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
      (find
       (format "apt-cache search %s" find))
      (installed
       (format "dlocate -l %s | grep '^.i'" installed))
      (list-files
       (format "dlocate -L %s | sort" list-files))
      (show
       (format "apt-cache show %s" show))
      (version
       (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
      (where
       (format "dlocate %s" where))))
    t)))

(defun eshell/ssh (&rest args) ;; NOTE: this is an amazing command (not what it does, but the idea behind it)
  "Secure shell launch.
When run inside a GNU Screen session, launch the ssh command in a new screen.
When run inside an X session, launch a new terminal session.
Else, start a new term buffer."
  (let ((cmd (eshell-flatten-and-stringify (cons "ssh" args)))
        (display-type (framep (selected-frame))))
    (cond ((and (eq display-type 't) (getenv "STY"))
	   (send-string-to-terminal (format "\033]83;screen %s\007" cmd))) ;; NOTE: consider removing the screen stuff ...
	  ((eq display-type 'x)
	   (eshell-do-eval (eshell-parse-command (format "x-terminal-emulator -e %s &" cmd)))
	   nil)
	  (t (apply 'eshell-exec-visual (cons "ssh" args))))))

(setq eshell-prompt-function ;; TODO: find out how to add colour to the eshell prompt
      (lambda ()
	(concat (user-login-name) "@" (system-name) ":"	(eshell/pwd)
		(if (string= (substring (shell-command-to-string "git branch") 0 1) "f")
		    ""
		  (concat " (" (eshell/git-branch) ")"))
		(if (= (user-uid) 0) "# " "$ "))))

(setq eshell-ls-use-in-dired t) ;; NOTE: use eshell to read directories in `dired'

(setq ;; eshell-prompt-function (lambda () (concat (user-login-name) "@" (system-name) ":" (eshell/pwd) (if (= (user-uid) 0) "# " "$ "))) ;; modify eshell prompt
      eshell-prompt-regexp "^[^#$\n]*[#$] " ;; fix shell auto-complete
      eshell-cmpl-cycle-completions nil ;; avoid cycle-completion
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'" ;; ignore file prefixes
      eshell-save-history-on-exit t ;; save eshell history on exit
      eshell-where-to-jump 'begin ;; jump to beginning of line
      eshell-review-quick-commands nil ;; enable quick review
      eshell-smart-space-goes-to-end t) ;; save buffer history

(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")
(add-to-list 'eshell-visual-commands "htop")
;; (add-to-list 'eshell-visual-commands "mutt") ;; NOTE: I don't use mutt anymore

(add-to-list 'eshell-command-completions-alist '("gunzip" "gz\\'"))
(add-to-list 'eshell-command-completions-alist '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply) ;; WARNING: will be very slow

(add-hook 'eshell-mode-hook '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol))) ;; for some reason this needs to be a hook

;;; COMMENT: aliases
;; (defalias 'em 'find-file) ;; TODO: this was made redundant ...

(provide 'eshell-config)

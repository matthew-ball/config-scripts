;; FILE: /home/chu/.conf-scripts/emacs-dir/my-modes/configuration-files.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;; COMMENT:
;; <insert description of file>

;; TODO: this is the beginning of a basic project management mode ... this now depends upon the custom variables defined above
;; TODO: include `README' files
;; NOTE: I think maybe this should just be a general `open-directory' mode ... I don't need to worry about using `desktop-mode' (???)
(defvar config-files-alist '() "Stores a list of the names of the configuration files.")

(defun add-config-file (file)
  "Add FILE to the list `config-files'."
  (push file config-files-alist))

(defun add-emacs-config-files (&rest junk)
  "Adds all GNU Emacs related configuration files to the `config-files-alist' list."
  (save-excursion   ;; NOTE: add the file `init.el' and the contents of the `config-el' directory
    (add-config-file (concat (expand-file-name user-emacs-directory) "init.el"))   ;; NOTE: add `init.el'
    (if (file-exists-p (concat user-emacs-directory "config-el/"))
	(let (files result)
	  (setq files (directory-files (concat (expand-file-name user-emacs-directory) "config-el/") t "\.el$" t))
	  (dolist (file-name files)
	    (when (and (file-readable-p file-name) (not (file-directory-p file-name)))
	      ;; (setq result (cons file-name result))
	      (add-config-file file-name))))))
  (save-excursion   ;; NOTE: add the contents of the `my-modes' directory
    (if (file-exists-p (concat user-emacs-directory "my-modes/"))
	(let (files result)
	  (setq files (directory-files (concat (expand-file-name user-emacs-directory) "my-modes/") t "\.el$" t))
	  (dolist (file-name files)
	    (when (and (file-readable-p file-name) (not (file-directory-p file-name)))
	      ;; (setq result (cons file-name))
	      (add-config-file file-name)))))))

(defun add-stumpwm-config-files (&rest junk)
  "Adds all StumpWM related configuration files to the `config-files-alist' list."
  (save-excursion
    (add-config-file (concat (expand-file-name user-scripts-directory) "stumpwmrc"))))

(defun add-bash-config-files (&rest junk)
  "Adds all BASH related configuration files to the `config-files-alist' list."
  (save-excursion
    (add-config-file (concat (expand-file-name user-scripts-directory) "bashrc"))
    (if (file-exists-p (concat user-scripts-directory "bash-dir/"))
	(let (files result)
	  (setq files (directory-files (concat (expand-file-name user-scripts-directory) "bash-dir/") t "\.sh$" t))
	  (dolist (file-name files)
	    (when (and (file-readable-p file-name) (not (file-directory-p file-name)))
	      (add-config-file file-name)))))))

(defun open-config-files (&rest junk)
  "Opens all configuration files."
  (interactive)
  (add-emacs-config-files) ;; NOTE: add emacs related files
  (add-stumpwm-config-files) ;; NOTE: add stumpwm related files
  (add-bash-config-files) ;; NOTE: add bashrc related files
  (dolist (file config-files-alist) ;; NOTE: open files
    (find-file file)))

(provide 'configuration-files)

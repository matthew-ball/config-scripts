;; FILE: /home/chu/.conf-scripts/emacs-dir/my-modes/configuration-files.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;; COMMENT:
;; <insert description of file>

;; An example configuration would be:
;; (require 'configuration-files)
;; (add-config-file (concat (expand-file-name user-emacs-directory) "init.el")) ;; NOTE: add ~/.conf-scripts/emacs-dir/init.el
;; (add-config-file (concat (expand-file-name user-scripts-directory) "stumpwmrc")) ;; NOTE: add ~/.conf-scripts/stumpwmrc
;; (add-config-file (concat (expand-file-name user-scripts-directory) "bashrc")) ;; NOTE: add ~/.conf-scripts/bashrc

;; (add-config-directory (concat user-emacs-directory "config-el/") "\.el$") ;; NOTE: add .el files in ~/.conf-scripts/emacs-dir/config-el/
;; (add-config-directory (concat user-emacs-directory "my-modes/") "\.el$") ;; NOTE: add .el files in ~/.conf-scripts/emacs-dir/my-modes/
;; (add-config-directory (concat user-scripts-directory "bash-dir/") "\.sh$") ;; NOTE: add .sh files in ~/.conf-scripts/bash-dir/

;; ;;; IMPORTANT: configuration files
;; ;; TODO: add `README' files
;; (require 'configuration-files)

;; ;; TODO: incorporate "pair values" (i.e. [file,ext]) somehow
;; ;; NOTE: could make this an alist?
;; (setq config-dir-and-ext '((concat user-emacs-directory "config-el/")
;; 			   (concat user-emacs-directory "my-modes/")
;; 			   (concat user-scripts-directory "bash-dir/")
;; 			   (concat user-scripts-directory "conkeror-dir/")
;; 			   (concat user-scripts-directory "stumpwm-dir/")))

;; (add-config-file (concat user-emacs-directory "init.el")) ;; NOTE: add `~/.conf-scripts/emacs-dir/init.el'
;; (add-config-directory (concat user-emacs-directory "config-el/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/config-el/'
;; (add-config-directory (concat user-emacs-directory "my-modes/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/my-modes/'
;; (add-config-directory (concat user-scripts-directory "bash-dir/") "\.sh$") ;; NOTE: add `*.sh' files in `~/.conf-scripts/bash-dir/'
;; (add-config-directory (concat user-scripts-directory "conkeror-dir/") ".js$") ;; NOTE: add `*.js' files in `~/.conf-scripts/conkeror-dir/'
;; (add-config-directory (concat user-scripts-directory "stumpwm-dir/") ".lisp$") ;; NOTE: add `*.lisp' files in `~/.conf-scripts/stumpwm-dir/'

;; TODO: include `README' files

;; NOTE: an alist is technically (key . value) in this case we could do (title . file) or something
(defvar config-files-alist '() "Stores a list of the names of the configuration files.")

;; (setq config-dir-and-ext '((concat user-emacs-directory "config-el/")
;; 			   (concat user-emacs-directory "my-modes/")
;; 			   (concat user-scripts-directory "bash-dir/")
;; 			   (concat user-scripts-directory "conkeror-dir/")
;; 			   (concat user-scripts-directory "stumpwm-dir/")))

;; (add-config-file (concat user-emacs-directory "init.el")) ;; NOTE: add `~/.conf-scripts/emacs-dir/init.el'
;; (add-config-directory (concat user-emacs-directory "config-el/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/config-el/'
;; (add-config-directory (concat user-emacs-directory "my-modes/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/my-modes/'
;; (add-config-directory (concat user-scripts-directory "bash-dir/") "\.sh$") ;; NOTE: add `*.sh' files in `~/.conf-scripts/bash-dir/'
;; (add-config-directory (concat user-scripts-directory "conkeror-dir/") ".js$") ;; NOTE: add `*.js' files in `~/.conf-scripts/conkeror-dir/'
;; (add-config-directory (concat user-scripts-directory "stumpwm-dir/") ".lisp$") ;; NOTE: add `*.lisp' files in `~/.conf-scripts/stumpwm-dir/'

;; (defvar user-emacs-files-directory (concat user-emacs-directory "config-el/") . "User emacs files.")
;; (defvar user-bash-directory (concat user-scripts-directory "bash-dir/") "User bash files.")
;; (defvar user-stumpwm-directory (concat user-scripts-directory "stumpwm-dir/") "User stumpwm files.")

;; (setq config-files-alist '((user-emacs-files-directory . ".el")
;;                            (user-bash-directory . ".sh")
;;                            (user-stumpwm-directory . ".lisp")))


(defun reset-config-files (&rest junk)
  "Reset the variable `config-files-alist' to nil."
  (setq config-files-alist '()))

(defun add-config-file (file &rest junk)
  "Add FILE to the list `config-files-alist'."
  (push file config-files-alist))

(defun add-config-directory (directory extension &rest junk)
  "Add all files in DIRECTORY matching EXTENSION to the `config-file-alist' list."
  (save-excursion
    (if (file-exists-p directory)
	(let (files result)
	  (setq files (directory-files directory t extension t))
	  (dolist (file files)
	    (when (and (file-readable-p file) (not (file-directory-p file)))
	      (add-config-file file)))))))

(defun open-config-files (&rest junk)
  "Opens all configuration files."
  (interactive)
  (save-excursion
    (dolist (file config-files-alist) ;; NOTE: open files listed in `config-files-alist'
      (find-file file))))

(provide 'configuration-files)

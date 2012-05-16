;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/gnus-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 15:04:15 EST

;; TODO:
;; if: there is no file at ~/.authinfo
;; then: create new file ~/.authinfo generating the file:
;;  machine imap.gmail.com login user-primary-email-address port 993
;;  machine smtp.gmail.com login user-primary-email-address port 587
;; else: 
;; 1. use that information (i.e. start gnus)
;; 2. re-write the file to disk (i.e. something has changed)

;;; COMMENT: gnus
;; SOURCE: http://emacswiki.org/emacs/CategoryGnus
;; SOURCE: http://emacswiki.org/emacs/Gnus
;; (require 'gnus nil 'noerror)
(autoload 'gnus "gnus" "Read mail and news with GNU Emacs." t)
;; (autoload 'gnus-parameters "gnus" "Parameters for Gnus mail." t)

;; (require 'nnimap)
;; (require 'starttls)
;; (require 'smtpmail)

;;; COMMENT: personal settings
(setq user-mail-address user-primary-email-address ;; NOTE: user primary email address
      ;; user-mail-address "mathew.ball@gmail.com" ;; NOTE: user mail address
      user-full-name "Matthew Ball" ;; NOTE: user full-name
      mail-aliases t ;; NOTE: enable mail aliases
      auth-source-save-behavior nil
      gnus-inhibit-startup-message t
      gnus-agent-expire-all t  ;; NOTE: allow uncaching of unread articles
      gnus-agent-article-alist-save-format 2 ;; NOTE: compress cache
      ;; mail-personal-alias-file "~/.conf-scripts/mailrc" ;; NOTE: change directory where mail aliases are located
      ;; nnimap-authinfo-file "~/.conf-scripts/passwords/authinfo" ;; NOTE: change directory where authentication information is found
      message-from-style 'angles ;; NOTE: specifies how the "From" header appears
      read-mail-command 'gnus
      message-send-mail-function 'smtpmail-send-it ;; NOTE: for gnus (message-mode)
      send-mail-function 'smtpmail-send-it) ;; NOTE: not for gnus (mail-mode)

;; TODO: set these to something else (???)
(setq custom-mail-dir "~/Mail/") ;; NOTE: set directory for mail
(setq custom-news-dir "~/News/") ;; NOTE: set directory for news

;;; COMMENT: gnus settings
(setq gnus-select-method '(nnml "")
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\(\\|$\\)\\|^[\"]\"[#'()]"
      gnus-invalid-group-regexp "[:`'\"]\\|^$"
      gnus-permanently-visible-groups "mail"
      gnus-thread-hide-subtree t
      gnus-fetch-old-headers t
      gnus-thread-ignore-subject t
      gnus-always-read-dribble-file t ;; NOTE: don't bugger me with dribbles
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;; NOTE: threads
      gnus-posting-styles '((".*" (name "Matthew Ball")) ;; TODO: change email addresses
			    ("gmail" (address "mathew.ball@gmail.com"))
			    ("anumail" (address "u4537508@anu.edu.au"))))

(setq gnus-check-new-newsgroups nil ;; NOTE: suppress checking for new groups
      gnus-save-newsrc-file nil ;; NOTE: turn off writing the `.newsrc' file
      gnus-read-newsrc-file nil ;; NOTE: ignore the `.newsrc' file
      gnus-interactive-exit nil
      gnus-save-killed-list nil ;; NOTE: do not save a list of killed groups to startup file
      )

(setq message-kill-buffer-on-exit t) ;; NOTE: kill the mail buffer after sending message

;;; COMMENT: visible headers
(setq gnus-visible-headers
      (concat "^From:\\|^Subject:\\|^Newsgroups:"
	      "\\|^Organization:"
	      "\\|^To:\\|^Cc:\\|^Date:"))

;;; COMMENT: imap setup
(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p" ;; NOTE: set ssl
      imap-log t ;; NOTE: log the imap session
      imap-store-password t ;; NOTE: store the session password
      gnus-secondary-select-methods
      '((nnimap "gmail" ;; NOTE: gmail login
		(nnimap-address "imap.gmail.com") ;; NOTE: being the "gmail" account, this hard-coding is ok?
		(nnimap-server-port 993)
		;; (nnimap-authinfo-file "~/.authinfo")
		(nnimap-authenticator login)
		(nnimap-expunge-on-close 'never)
		(nnimap-stream ssl))
	;; (nnimap "anumail" ;; NOTE: anumail login (ERROR: this does not work)
	;; 	(nnimap-address "anumail.anu.edu.au")
	;; 	(nnimap-server-port 993)
	;; 	;; (nnimap-authinfo-file "~/.authinfo")
	;; 	;; (nnimap-authenticator login)
	;; 	;; (nnimap-expunge-on-close 'never)
	;; 	(nnimap-stream ssl))
	))


;;; COMMENT: smtp setup(single account)
(require 'smtpmail)

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "mathew.ball@gmail.com" nil))) ;; TODO: replace email address

;;; COMMENT: smtp setup (multipl accounts) (ERROR: this doe not work)
;; (require 'smtpmail)

;; (defvar smtp-accounts ;; available smtp accounts
;;   '((ssl "mathew.ball@gmail.com" "smtp.gmail.com" 587 "key" nil)
;;     (ssl "u4537508@anu.edu.au.com" "smtphost.anu.edu.au" 465 "key" nil)))

;; (setq starttls-use-gnutls t
;;       starttls-gnutls-program "gnutls-cli"
;;       starttls-extra-arguments '("--insecure"))

;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "mathew.ball@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       ;; smtpmail-local-domain "mail.bigpond.com"
;;       smtpmail-debug-verb t
;;       smtpmail-debug-info t) ;; to debug

;; (defun set-smtp-plain (server port)
;;   "Set related SMTP variables for supplied parameters."
;;   (setq smtpmail-smtp-server server
;; 	smtpmail-smtp-service port
;; 	;; smtpmail-auth-credentials "~/.authinfo" ;; I have not set this up
;; 	smtpmail-starttls-credentials nil)
;;   (message "Setting SMTP server to `%s:%s'."
;; 	    server port address))

;; (defun set-smtp-ssl (server port key cert)
;;   "Set related SMTP and SSL variables for supplied parameters."
;;   (setq starttls-use-gnutls t
;; 	starttls-gnutls-program "gnutls-cli"
;; 	starttls-extra-arguments nil
;; 	smtpmail-smtp-server server
;; 	smtpmail-smtp-service port
;; 	smtpmail-starttls-credentials (list (list server port key cert))
;; 	;; smtpmail-auth-credentials "~/.authinfo" ;; I have not set this up
;; 	)
;;   (message "Setting SMTP server to `%s:%s' (SSL enabled)."
;; 	   server port address))

;; (defun change-smtp ()
;;   "Change the SMTP server according to the current from line."
;;   (save-excursion
;;     (loop with from = (save-restriction
;; 			(message-narrow-to-headers)
;; 			(message-fetch-field "from"))
;; 	  for (acc-type address . auth-spec) in smtp-accounts
;; 	  when (string-match address from)
;; 	  do (cond
;; 	      ((eql acc-type 'plain)
;; 	       (return (apply 'set-smtp-plain auth-spec)))
;; 	      ((eql acc-type 'ssl)
;; 	       (return (apply 'set-smtp-ssl auth-spec)))
;; 	      (t (error "Unrecognized SMTP account type: `%s'." acc-type)))
;; 	  finally (error "Cannot interfere SMTP information."))))

;;; COMMENT: email config
;; (add-hook 'message-send-hook 'change-smtp) ;; change smtp server appropriately
;; (add-hook 'message-mode-hook (function (lambda () (local-set-key (kbd "<tab>") 'bbdb-complete-name)))) ;; NOTE: add tab completion to name in the "To:" field

;; (remove-hook 'gnus-summary-prepare-exit-hook
;; 	     'gnus-summary-expire-articles)

;;; COMMENT: html display
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-with-w3m-keymap nil)

;;; COMMENT: mode-line
;; (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n") ;; NOTE: set mode-line

;;; COMMENT: rss config
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ;; NOTE: topic mode - tree view - is always active

;; (eval-after-load "gnus-sum" ;; NOTE: set the default value of mm-discouraged-alternatives
;;   '(add-to-list 'gnus-newsgroup-variables '(mm-discouraged-alternatives . '("text/html" "image/.*"))))

;; NOTE: display 'text/html' parts in nnrss groups
;; (add-to-list 'gnus-parameters '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;;; COMMENT: gnus parameters
;; (setq gnus-parameters
;;       '(("mail\\..*"
;; 	 (gnus-show-threads nil)
;; 	 (gnus-use-scoring nil)
;; 	 (gnus-summary-line-format "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
;; 	 (gcc-self . t)
;; 	 (display . all))

;; 	("^nnimap:\\(foo.bar\\)$"
;; 	 (to-group . "\\1"))

;; 	("mail\\.me"
;; 	 (gnus-use-scoring t))

;; 	("list\\..*"
;; 	 (total-expire . t)
;; 	 (broken-reply-to . t))))

;; (add-hook 'gnus-summary-mode-hook
;;           (lambda () (when (string-match "^nnrss:.*" gnus-newsgroup-name)
;; 		  (progn
;; 		    (make-local-variable 'gnus-show-threads)
;; 		    (make-local-variable 'gnus-article-sort-functions)
;; 		    (make-local-variable 'gnus-use-adaptive-scoring)
;; 		    (make-local-variable 'gnus-use-scoring)
;; 		    (make-local-variable 'gnus-score-find-score-files-function)
;; 		    (make-local-variable 'gnus-summary-line-format)
;; 		    (setq gnus-show-threads nil
;; 			  gnus-article-sort-functions 'gnus-article-sort-by-date
;; 			  gnus-use-adaptive-scoring nil
;; 			  gnus-use-scoring t
;; 			  gnus-score-find-score-files-function 'gnus-score-find-single)))))

;; (defun browse-nnrss-url (arg)
;;   "Browse RSS url."
;;   (interactive "p")
;;   (let ((url (assq nnrss-url-field (mail-header-extra (gnus-data-header (assq (gnus-summary-article-number) gnus-newsgroup-data))))))
;;     (if url
;; 	(browse-url (cdr url))
;;       (gnus-summary-scroll-up arg))))

;; (add-hook 'gnus-summary-mode-hook (lambda () (define-key gnus-summary-mode-map (kbd "C-<return>") 'browse-nnrss-url)))

;; (add-to-list 'nnmail-extra-headers nnrss-url-field)

(provide 'gnus-config)

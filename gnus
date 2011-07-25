;; ============================
;; custom .gnus file
;; Matthew Ball (copyleft 2011)
;; ============================
;; TODO: move ~/.mailrc to ~/.conf-scripts/mailrc

;; ==================
;;; personal settings
;; ==================
(setq user-mail-address "mathew.ball@gmail.com" ;; user mail address (could use my school mail)
      user-full-name "Matthew Ball" ;; user full-name
      mail-aliases t ;; enable mail aliases
      ;; mail-personal-alias-file "~/.conf-scripts/mailrc" ;; change directory where mail aliases are located
      ;; nnimap-authinfo-file "~/.conf-scripts/passwords/authinfo" ;; change directory where authentication information is found
      message-from-style 'angles ;; specifies how the "From" header appears
      read-mail-command 'gnus
      message-send-mail-function 'smtpmail-send-it ;; for gnus (message-mode)
      send-mail-function 'smtpmail-send-it) ;; not for gnus (mail-mode)

;; (setq custom-mail-dir "~/.mail/") ;; set directory for mail
;; (setq custom-news-dir "~/.news/") ;; set directory for news

;; ==============
;;; gnus settings
;; ==============
(setq gnus-select-method '(nnml "")
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\(\\|$\\)\\|^[\"]\"[#'()]"
      gnus-invalid-group-regexp "[:`'\"]\\|^$"
      gnus-permanently-visible-groups "mail"
      gnus-thread-hide-subtree t
      gnus-fetch-old-headers t
      gnus-thread-ignore-subject t
      gnus-always-read-dribble-file t ;; don't bugger me with dribbles
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-posting-styles '((".*"
			     (name "Matthew Ball"))
			    ("gmail" (address "mathew.ball@gmail.com"))
			    ("anumail" (address "u4537508@anu.edu.au"))))

(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-interactive-exit nil
      gnus-save-killed-list nil
      gnus-check-new-newsgroups nil)

;; ================
;;; visible headers
;; ================
(setq gnus-visible-headers
      (concat "^From:\\|^Subject:\\|^Newsgroups:"
	      "\\|^Organization:"
	      "\\|^To:\\|^Cc:\\|^Date:"))

;; ===========
;;; imap setup
;; ===========
(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p" ;; set ssl
      imap-log t ;; log the imap session
      imap-store-password t ;; store the session password
      gnus-secondary-select-methods
      '((nnimap "gmail" ;; gmail login
		(nnimap-address "imap.gmail.com")
		(nnimap-server-port 993)
		;; (nnimap-authinfo-file "~/.authinfo")
		(nnimap-authenticator login)
		(nnimap-expunge-on-close 'never)
		(nnimap-stream ssl))
	(nnimap "anumail" ;; anumail login (THIS DOES NOT WORK)
		(nnimap-address "anumail.anu.edu.au")
		(nnimap-server-port 993)
		;; (nnimap-authinfo-file "~/.authinfo")
		;; (nnimap-authenticator login)
		;; (nnimap-expunge-on-close 'never)
		(nnimap-stream ssl))))

;; ===========
;;; smtp setup
;; ===========
(require 'smtpmail)

(defvar smtp-accounts ;; available smtp accounts
  '((ssl "mathew.ball@gmail.com" "smtp.gmail.com" 587 "key" nil)
    (ssl "u4537508@anu.edu.au.com" "smtphost.anu.edu.au" 465 "key" nil)))

(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments '("--insecure"))

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "mathew.ball@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; smtpmail-local-domain "mail.bigpond.com"
      smtpmail-debug-verb t
      smtpmail-debug-info t) ;; to debug

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

;; =============
;;; email config
;; =============
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ;; topic mode - tree view - is always active
(add-hook 'message-send-hook 'change-smtp) ;; change smtp server appropriately
;; (add-hook 'message-mode-hook (function (lambda () (local-set-key (kbd "<tab>") 'bbdb-complete-name)))) ;; add tab completion to name in the "To:" field

(remove-hook 'gnus-summary-prepare-exit-hook
	     'gnus-summary-expire-articles)

;; ===========
;;; rss config
;; ===========
;; (eval-after-load "gnus-sum" ;; set the default value of mm-discouraged-alternatives
;;   '(add-to-list 'gnus-newsgroup-variables '(mm-discouraged-alternatives . '("text/html" "image/.*"))))
     
;; (add-to-list ;; display ‘text/html’ parts in nnrss groups
;;  'gnus-parameters '("\\`nnrss:" (mm-discouraged-alternatives nil)))

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

;; ====
;;; old
;; ====
;; (setq gnus-startup-file "~/.newsrc" 
;;       gnus-default-directory "~/mail/" 
;;       gnus-home-directory "~/mail/" 
;;       gnus-article-save-directory "~/mail/save/" 
;;       gnus-kill-files-directory "~/mail/trash/" 
;;       gnus-agent-directory "~/mail/agent/" 
;;       gnus-cache-directory "~/mail/cache/" 
;;       mail-source-directory "~/mail/incoming/" 
;;       nnmail-message-id-cache-file "~/mail/.nnmail-cache" 
;;       nnml-newsgroups-file "~/mail/newsgroup" 
;;       message-directory "~/mail/" 
;;       message-auto-save-directory "~/mail/drafts/")

;; (setq gnus-save-newsrc-file nil 
;;       gnus-read-newsrc-file nil 
;;       gnus-use-dribble-file nil 
;;       gnus-interactive-exit nil 
;;       gnus-save-killed-list nil)

;; (setq mail-sources
;;       '((imap :server "imap.gmail.com" 
;; 	      :port 993 
;; 	      :user "mathew.ball@gmail.com" 
;; 	      :password "secret" ;; this is obviously *not* my password
;; 	      :authentication 'login 
;; 	      :stream ssl 
;; 	      :fetchflag "\\Seen") 
;; 	(imap :server "imap.gmail.com" 
;; 	      :port 993 
;; 	      :user "mathew.ball@gmail.com" 
;; 	      :password "secret" ;; this is obviously *not* my password 
;; 	      :authentication 'login 
;; 	      :stream ssl 
;; 	      :fetchflag "\\Seen")))

;; (setq nnmail-split-methods 
;;       '(("Emacs" "^To:.*help-gnu-emacs\\|^Cc:.*help-gnu-emacs") 
;; 	("Kernel-zh" "^To:.*Linux-kernel\\|^Cc:.*Linux-kernel") 
;; 	("Sawfish" "^To:.*sawfish-list\\|^Cc:.*sawfish-list") 
;; 	("Living" "^To:.*xxx") 
;; 	("Working" "^To:.*yyy") 
;; 	("NotHere" "")))

;; (setq gnus-group-line-format "\t%M%S%p%P%4y / %4t: %B%*%(%-10g%)%O\t%ud\n" 
;;       gnus-check-new-newsgroups nil)

;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; (defun gnus-user-format-function-d (headers) 
;;   (let ((time (gnus-group-timestamp gnus-tmp-group))) 
;;     (if time 
;; 	(format-time-string "(%d/%m/%Y %H:%M)" time) 
;;       "")))

;; (setq gnus-parameters 
;;       '(("Emacs" 
;; 	 (subscribed . t) 
;; 	 (expiry-wait . 7) 
;; 	 (broken-reply-to . t) 
;; 	 (to-address . "help-gnu-em...@gnu.org") 
;; 	 (admin-address . "help-gnu-emacs-requ...@gnu.org") 
;; 	 (posting-style (address "y...@gmail.com") (signature "Regards,\nyyy"))) 
;; 	("Sawfish" 
;; 	 (subscribed . t) 
;; 	 (expiry-wait . 15) 
;; 	 (broken-reply-to . t) 
;; 	 (to-address . "sawfish-l...@gnome.org") 
;; 	 (admin-address . "sawfish-list-requ...@gnome.org") 
;; 	 (posting-style (address "y...@gmail.com") (signature "Regards,\nyyy"))) 
;; 	("Kernel-zh" 
;; 	 (subscribed . t) 
;; 	 (expiry-wait . 7) 
;; 	 (broken-reply-to . t) 
;; 	 (to-address . "Linux-ker...@kernel-zh.org") 
;; 	 (admin-address . "Linux-kernel-requ...@kernel-zh.org") 
;; 	 (posting-style (name "yyy") (address "y...@gmail.com") (signature "yyy"))) 
;; 	("Living" 
;; 	 (auto-expire . t) 
;; 	 (posting-style (name "xxx") (address "x...@gmail.com") (signature "祝一切顺利，\n    xxx"))) 
;; 	("Working" 
;; 	 (auto-expire . t) 
;; 	 (posting-style (name "yyy") (address "y...@gmail.com") (signature "Regards,\nyyy")))))

;; (setq gnus-face-0 'gnus-server-offline 
;;       gnus-face-1 'gnus-server-agent 
;;       gnus-face-2 'gnus-server-opened 
;;       gnus-user-date-format-alist '(((gnus-seconds-today) . "Today  %2H:%2M") 
;; 				    (t . "%b %d %2H:%2M")) 
;;       gnus-summary-line-format "%U%R%1{|%}%2{%-10&user-date;%}%1{|%}%0{%-24,24n%}%1{|%}%B%s\n" 
;;       gnus-show-threads t 
;;       gnus-thread-hide-subtree t 
;;       gnus-thread-sort-functions '((not gnus-thread-sort-by-number) (not gnus-thread-sort-by-date)) 
;;       gnus-article-sort-functions '((not gnus-article-sort-by-number) (not gnus-article-sort-by-date)) 
;;       gnus-sum-thread-tree-root "* " 
;;       gnus-sum-thread-tree-single-indent "* " 
;;       gnus-sum-thread-tree-leaf-with-other "|-> " 
;;       gnus-sum-thread-tree-vertical "|" 
;;       gnus-sum-thread-tree-single-leaf "`-> ") 

;; (setq gnus-visible-headers "From:\\|Subject:\\|Date:\\|Organization:\\|To:\\|Cc:\\|Reply-To:" 
;;       message-fill-column nil 
;;       message-kill-buffer-on-exit t 
;;       gnus-treat-display-smileys nil 
;;       gnus-treat-display-face nil 
;;       mm-inline-large-images t)

;; (setq gnus-default-charset 'utf-8 
;;       mm-coding-system-priorities '(utf-8) 
;;       gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit)) 
;;       gnus-summary-show-article-charset-alist '((1 . utf-8) (2 . chinese-iso-8bit) (3 . gbk) (4 . big5)) 
;;       gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown iso-8859-1))

;; (eval-after-load "mm-decode" 
;;   '(progn 
;;      (add-to-list 'mm-discouraged-alternatives "text/html") 
;;      (add-to-list 'mm-discouraged-alternatives "text/richtext"))) 

;; (setq message-sendmail-envelope-from 'header)

;; (setq send-mail-function 'smtpmail-send-it 
;;       message-send-mail-function 'smtpmail-send-it)

;; (setq smtpmail-default-smtp-server "smtp.gmail.com" 
;;       smtpmail-smtp-server "smtp.gmail.com" 
;;       smtpmail-smtp-service 587 
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "y...@gmail.com" "secret")) 
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))

;; (defun change-smtpmail-auth-credentials () 
;;   "Change the smtpmail-starttls-credentials according to current from line." 
;;   (save-excursion 
;;     (let ((from (save-restriction 
;; 		  (message-narrow-to-headers) 
;; 		  (message-fetch-field "from")))) 
;;       (if (string-match "x...@gmail.com" from) 
;; 	  (setq smtpmail-auth-credentials '(("smtp.gmail.com" 587 "x...@gmail.com" "secret"))) 
;; 	(setq smtpmail-auth-credentials '(("smtp.gmail.com" 587 "y...@gmail.com" "secret")))))))

;; (add-hook 'message-setup-hook 'change-smtpmail-auth-credentials) 

;; ==========
;;; rss setup
;; ==========
;; (add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;; Support ATOM feeds in mm-insert-url (convert them to RSS, http://www.emacswiki.org/emacs/GnusRss).
;; (require 'mm-url)
;; (defadvice mm-url-insert (after DE-convert-atom-to-rss () )
;;   "Converts atom to RSS by calling xsltproc."
;;   (when (re-search-forward "xmlns=[\"']http://www.w3.org/.*/Atom[\"']"
;; 			   nil t)
;;     (goto-char (point-min))
;;     (message "Converting Atom to RSS... ")
;;     (call-process-region (point-min) (point-max)
;; 			 "xsltproc"
;; 			 t t nil
;; 			 (expand-file-name "~/.el/atom2rss.xsl") "-")
;;     (goto-char (point-min))
;;     (message "Converting Atom to RSS... done")))
;; (ad-activate 'mm-url-insert)
;; (require 'nnrss)

;; (setq nnrss-use-local t) ;; feeds are updated by download script which runs regularly.

;; Update asynchronous download script file
;; (setq nnrss-download-script-file (expand-file-name "rss-download.sh" gnus-directory))
;; (defun nnrss-update-download-script()
;;   "Update RSS download script"
;;   (interactive)
;;   (with-temp-buffer
;;     (nnrss-generate-download-script)
;;     (write-file nnrss-download-script-file nil)))

;; (defun nnrss-run-download-script() 
;;   "Run RSS download script"
;;   (interactive)
;;   (gnus-message 5 (concat "Running download script: " nnrss-download-script-file ".."))
;;   (shell-command nnrss-download-script-file))

;; Re-define nnrss-generate-download-script to something better.
;; Do conversion of Atom to RSS for all ATOM feeds in the download script.
;; (defun nnrss-generate-download-script ()
;;   "Generate a download script in the current buffer.
;; It is useful when `(setq nnrss-use-local t)'."
;;   (interactive)
;;   (insert "#!/bin/sh\n")
;;   (insert "# Generated on " (format-time-string "%Y-%m-%d %H:%M %Z") "\n")
;;   (insert "ATOM2RSS='" (expand-file-name "~/.el/atom2rss.xsl") "'\n")
;;   (insert "RSSDIR='" (expand-file-name nnrss-directory) "'\n\n")
;;   (dolist (elem nnrss-group-alist)
;;     (let ((url (nth 1 elem))
;;           (filename (nnrss-translate-file-chars (concat (car elem) ".xml"))))
;;       (insert "wget -t 3 -T 60 -q -O \"$RSSDIR/feed.tmp\" '" url "'\n")
;;       (insert "if [ $? -eq 0 ]; then\n")
;;       (insert "  touch \"$RSSDIR/feed.tmp\"\n")
;;       (insert "  if grep -qE \"xmlns=[\\\"']http://www.w3.org/.*/Atom[\\\"']\" \"$RSSDIR/feed.tmp\"; then\n")
;;       (insert "    xsltproc \"$ATOM2RSS\" \"$RSSDIR/feed.tmp\" > \"$RSSDIR\"/'" filename "'\n")
;;       (insert "  else\n")
;;       (insert "    mv \"$RSSDIR/feed.tmp\" \"$RSSDIR\"/'" filename "'\n")
;;       (insert "  fi\n")
;;       (insert "fi\n")
;;       ))
;;   (insert "\n" "rm -f \"$RSSDIR/feed.tmp\"\n")
;;   (insert "exit 0" "\n"))

;; WORKAROUND Redefine nnrss-make-hash-index to something more appropriate..
;; Only hash certain fields, exclude all others (since they tend to change way
;; too often, causing too many article "duplicates" in RSS feeds).
;; (defvar nnrss-article-hash-fields '(title link guid pubDate author)
;;   "List of fields to use as basis for generating an RSS feed item hash value.
;; The fields are symbols.

;; Common fields include:
;; title, link, guid, pubDate, description and author

;; In theory, if you never want a an article to appear more than
;; once (first publication), then the guid field alone should be
;; good enough (if the feed provides it, most do).

;; Be careful with including description if you don't like duplicates.
;; Some RSS-feeds update the description with number of comments and
;; other data that frequently change after the time of publication.
;; That will result in duplicate articles, if you include it in hashing.")
;; ;; Re-defining function from nnrss.el:
;; (defun nnrss-make-hash-index (item)
;;   "Make a hash value of the given RSS feed item, based on the fields
;; set in `nnrss-article-hash-fields'."
;;   (setq item (gnus-remove-if 
;; 	      (lambda (field)
;; 		(cond 
;; 		 ((not (listp field)))
;; 		 ((not (memq (car field) nnrss-article-hash-fields)))
;; 		 ((eq 'link (car field)))))))) ; only hash plain link field (discard links in Atom xmlns for instance)

;; (add-hook 'gnus-summary-mode-hook
;;           (lambda ()
;;             (if (string-match "^nnrss:.*" gnus-newsgroup-name)
;;                 (progn
;;                   (make-local-variable 'gnus-show-threads)
;;                   (make-local-variable 'gnus-article-sort-functions)
;;                   (make-local-variable 'gnus-use-adaptive-scoring)
;;                   (make-local-variable 'gnus-use-scoring)
;;                   (make-local-variable 'gnus-score-find-score-files-function)
;;                   (make-local-variable 'gnus-summary-line-format)
;;                   (setq gnus-show-threads nil)
;;                   (setq gnus-article-sort-functions 'gnus-article-sort-by-date)
;;                   (setq gnus-use-adaptive-scoring nil)
;;                   (setq gnus-use-scoring t)
;;                   (setq gnus-score-find-score-files-function 'gnus-score-find-single)
;;                   (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n")))))


;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/user-config.el
;; AUTHOR: Matthew Ball (copyleft 2012, 2013)

;; COMMENT: dired application management
(setq dired-guess-shell-alist-user
      (list
       (list "\\.pdf$" "evince")
       (list "\\.PDF$" "evince")
       ))

;;; COMMENT: emacs multimedia system
;; SOURCE: `http://emacswiki.org/cgi-bin/wiki/EMMS'
;; NOTE: this is really messy, could do with some clean-up
;; (require 'emms-autoloads) ;; NOTE: this could work best
;; (require 'emms-player-simple) ;; NOTE: could be needed
;; (autoload 'emms-all "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-default-players "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-player-mplayer "emms-player-mplayer" "MPlayer interface with GNU Emacs multimedia." t)  ;; ERROR: does not work
;; (autoload 'emms-player-mpd-connect  "emms-player-mode" "Interface between `EMMS' and `MPD'." t)

;; (emms-standard) ;; NOTE: runs just the `emms-standard' configuration
;; (emms-devel) ;; DEBUG: apparently not what I want
;; (emms-all) ;; NOTE: runs `emms-standard' and adds stable `emms' features
;; (emms-default-players)

;; TODO: set with variable
;; (setq emms-source-file-default-directory "~/Music/") ;; NOTE: when asked for `emms-play-directory' always start from this

;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "7700")

;; (add-to-list 'emms-info-functions 'emms-info-mpd) ;; NOTE: get track information from `mpd'
;; (add-to-list 'emms-player-list 'emms-player-mpd) ;; NOTE: add `mpd' to the `emms' player list

;; (emms-player-mpd-connect) ;; NOTE: connect `emms' to `mpd'

;; (setq emms-show-format "NP: %s") ;; NOTE: starts to play a track with "NP: "
;; (add-hook 'emms-player-started-hook 'emms-show) ;; NOTE: show the current track with `emms'

;; (setq emms-player-mpg321-parameters '("-o" "alsa")) ;;NOTE: use alsa with mpg321

;; (define-emms-simple-player flash '(file) "\\.flv$" "mplayer" "-fs") ;; NOTE: play `*.flv' files with `mplayer' (opening full-screen)

;; (add-to-list 'emms-player-list 'emms-player-flash)

;; COMMENT: `emms' with `mplayer'
;; NOTE: I don't think I need this
;; (setq emms-player-mplayer-command-name "mplayer"
;;       emms-player-mplayer-parameters '("-slave")
;;       emms-player-mpg321-command-name "mpg123"
;;       ;; emms-player-list '(emms-player-mplayer
;;       ;; 			 emms-player-mplayer-playlist
;;       ;; 			 emms-player-mpg321
;;       ;; 			 emms-player-ogg123)
;;       )

;; (push emms-player-mplayer emms-player-list)
;; (push emms-player-mplayer-playlist emms-player-list)

;; (defun ddliu-emms-player-mplayer-volume-up ()
;;   "Depends on mplayer’s -slave mode."
;;   (interactive)
;;   (process-send-string
;;    emms-player-simple-process-name "volume 1\n"))

;;; COMMENT: project management
;; SOURCE: `http://emacswiki.org/emacs/eproject'
;; (require 'eproject) ;; FIX: change this to an autoload
;; TODO: learn `eproject'

;;; COMMENT: default browser
(setq browse-url-new-window-flag t
      browse-url-browser-function 'choose-browser ;; NOTE: ask which browser to use
      browse-url-generic-program (getenv "BROWSER")) ;; NOTE: use the system's $BROWSER environment variable

;; TODO: do I want to make a `external-browser-alist' variable which is a listn of available browsers?
(defun choose-browser (url &rest junk) ;; NOTE: select which browser to use (i.e. internal or external)
  "Navigate a web browser to URL.

Although this is interactive, call this with \\[browse-url]."
  (interactive "sURL: ")
  (if (y-or-n-p "Use w3m web browser? ")
      (w3m-browse-url url t)
    (browse-url-generic url)))

;;; COMMENT: thesaurus
;; SOURCE: `http://emacswiki.org/emacs/thesaurus.el'
(autoload 'thesaurus-choose-synonym-and-replace "thesaurus" "Choose and replace a word with it's synonym." t)

(setq thesaurus-bhl-api-key "8c5a079b300d16a5bb89246322b1bea6")  ;; NOTE: from registration

;;; COMMENT: dictem
;; SOURCE: ...
;;(require 'dictem)
(autoload 'dictem-run-search "dictem" "" t)

(setq dictem-server "dict.org"
      dictem-port "2628"
      dictem-exclude-databases '("ger-" "-ger" "fra-" "-fra")
      ;; dictem-select-database "*"
      ;; dictem-select-strategy "."
      )

;; TODO: move to key-bindings!!!
;; TODO: C-c d should be a `dictem-map' etc
;; COMMENT: key-bindings
;; NOTE: SEARCH = MATCH + DEFINE
;; ask for word, database and search strategy and show definitions found
(global-set-key "\C-cs" 'dictem-run-search)

;; NOTE: MATCH
;; ask for word, database and search strategy and show matches found
(global-set-key "\C-cm" 'dictem-run-match)

;; NOTE: DEFINE
;; ask for word and database name and show definitions found
(global-set-key "\C-cd" 'dictem-run-define)

;; NOTE: SHOW SERVER
;; show information about DICT server
(global-set-key "\C-c\M-r" 'dictem-run-show-server)

;; NOTE: SHOW INFO
;; show information about the database
(global-set-key "\C-c\M-i" 'dictem-run-show-info)

;; NOTE: SHOW DB
;; show a list of databases provided by DICT server
(global-set-key "\C-c\M-b" 'dictem-run-show-databases)

;; TODO: ...
;; (add-hook 'c-mode-common-hook
;; 	  '(lambda ()
;; 	     (interactive)
;; 	     (make-local-variable 'dictem-default-database)
;; 	     (setq dictem-default-database "man")))

;; the code above sets default database to "man" in C buffers

(eval-after-load "dictem" '(dictem-initialize))

;; NOTE: for creating hyperlinks on database names and found matches (click on them with mouse-2)
(add-hook 'dictem-postprocess-match-hook 'dictem-postprocess-match)

;; NOTE: for highlighting the separator between the definitions found, this also creates hyperlink on database names
(add-hook 'dictem-postprocess-definition-hook 'dictem-postprocess-definition-separator)

;; NOTE: for creating hyperlinks in `dictem' buffer that contains definitions
(add-hook 'dictem-postprocess-definition-hook 'dictem-postprocess-definition-hyperlinks)

;; NOTE: for creating hyperlinks in dictem buffer that contains information about a database
(add-hook 'dictem-postprocess-show-info-hook 'dictem-postprocess-definition-hyperlinks)

;; NOTE: "virtual" dictionary
(setq dictem-user-databases-alist '(("_en-en"  . ("foldoc" "gcide" "wn"))
                                    ("en-en" . ("dict://dict.org:2628/english")) 
                                    ("_unidoc" . ("susv3" "man" "info" "howto" "rfc"))))

;; NOTE: ...
(setq dictem-use-user-databases-only t)

;; NOTE: all functions from dictem-postprocess-each-definition-hook will be run for each definition which in turn will be narrowed
;; NOTE: current database name is kept in dictem-current-dbname variable
;; NOTE: the following code demonstrates how to highlight SUSV3 and ROFF definitions
(add-hook 'dictem-postprocess-definition-hook 'dictem-postprocess-each-definition)

;; NOTE: function for highlighting definition from the database "susv3"
(defun dictem-highlight-susv3-definition ()
  (cond ((string= "susv3" dictem-current-dbname)
	 (goto-char (point-min))
	 (while (search-forward-regexp
		 "^ *[QWERTYUIOPASDFGHJKLZXCVBNM ]+$" nil t)
	   (put-text-property
	    (match-beginning 0) (match-end 0) 'face 'bold)))))

;; NOTE: function to show roff-formatted text from the database "man"
(require 'woman)

(defun dictem-highlight-man-definition ()
  (cond ((string= "man" dictem-current-dbname)
	 (goto-char (point-min))
	 (while (search-forward-regexp "^  " nil t)
	   (replace-match ""))
	 (goto-char (point-min))
	 (forward-line 2)
	 (woman-decode-region (point) (point-max)))))

(add-hook 'dictem-postprocess-each-definition-hook 'dictem-highlight-susv3-definition)
(add-hook 'dictem-postprocess-each-definition-hook 'dictem-highlight-man-definition)

;;; COMMENT: quick and web jumps
(defvar quick-jump-list nil "List of sites for quick jump functionality.")

(setq quick-jump-alist '(("google" . "http://www.google.com/")
                         ("emacswiki" . "http://www.emacswiki.org/")
                         ("recent changes" . "http://www.emacswiki.org/emacs/RecentChanges/")
                         ("wikipedia" . "http://www.wikipedia.org/")
                         ("stanford philosophy" . "http://plato.stanford.edu/")))

(defun quick-jump (&rest junk)
  "Open a quick-jump URL in `w3m-mode'."
  (interactive)
  (let ((user-input (ido-completing-read "Select site: " (mapcar #'(lambda (entry) (car entry)) quick-jump-alist))))
    (w3m-goto-url-new-session (cdr (assoc user-input quick-jump-alist)))))

(defmacro create-web-jump (name url)
  "Create function NAME which browses to URL."
  `(defun ,name (search)
     "Query for SEARCH string."
     (interactive ,(concat "sSearch " (downcase (symbol-name name)) " for: "))
     (browse-url (concat ,url "=" (substitute #'\+ #'\Space search)))))

(create-web-jump google "http://www.google.com/search?q")
(create-web-jump wikipedia "http://en.wikipedia.org/wiki/Special:Search?search")
;; TODO: make an emacs wiki web-jump
;; TODO: make a stumpwm wiki web-jump
;; TODO: make a common-lisp wiki web-jump

;;; COMMENT: w3m buffers
(defun w3m-buffer ()
  (interactive)
  (let ((new-buffer (ido-completing-read "Select buffer: " (w3m-list-buffers))))
    (switch-to-buffer new-buffer)))

;;; COMMENT: w3m browser
;; SOURCE: `http://www.emacswiki.org/emacs/emacs-w3m'
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMTabs'
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMHintsAndTips'
;; TODO: move w3m configuration into a new file ... (???)
(require 'w3m-load) ;; TEST: this still needs to be tested
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (autoload 'w3m-search "w3m-search" "Search with a WWW browser." t)
;; (autoload 'w3m-goto-url-new-session "w3m" "Go to a URL in a new w3m buffer." t)
;; (autoload 'w3m-mode-map "w3m" "The mode map for w3m." t) ;; ERROR: this does not work

;; (w3m-lnum-mode 1) ;; NOTE: apparently an extension to w3m

;; COMMENT: tables
;; (standard-display-ascii ?\200 [15])
;; (standard-display-ascii ?\201 [21])
;; (standard-display-ascii ?\202 [24])
;; (standard-display-ascii ?\203 [13])
;; (standard-display-ascii ?\204 [22])
;; (standard-display-ascii ?\205 [25])
;; (standard-display-ascii ?\206 [12])
;; (standard-display-ascii ?\210 [23])
;; (standard-display-ascii ?\211 [14])
;; (standard-display-ascii ?\212 [18])
;; (standard-display-ascii ?\214 [11])
;; (standard-display-ascii ?\222 [?\'])
;; (standard-display-ascii ?\223 [?\"])
;; (standard-display-ascii ?\224 [?\"])
;; (standard-display-ascii ?\227 " -- ")

;; COMMENT: w3m interface
(setq w3m-key-binding 'info ;; NOTE: use sane key-bindings
      ;; w3m-home-page "www.emacswiki.org"
      ;; w3m-default-display-inline-images t ;; NOTE: display images by default
      w3m-use-toolbar nil
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;; COMMENT: w3m cookies
(require 'w3m-cookie) ;; NOTE: enable cookies support in w3m

(setq w3m-use-cookies t ;; NOTE: use cookies in w3m
      w3m-cookie-file (concat (expand-file-name user-emacs-directory) "w3m/cookie") ;; NOTE: save cookies to ~/.emacs.d/w3m/cookie
      w3m-cookie-accept-bad-cookies t
      w3m-cookie-accept-domains '("www.emacswiki.org" "www.google.com" "www.wikipedia.org" "www.github.com" "http://plato.stanford.edu"))

;; COMMENT: w3m sessions
(setq w3m-make-new-session t) ;; NOTE: open a new tab by typing RET on a url string
(setq w3m-use-tab t) ;; NOTE: C-c C-t creates new tab with line below

;; COMMENT: w3m control and external browser support
;; (defun open-blank-w3m (&rest junk) ;; NOTE: this is redundant - \\[w3m] just opens a blank w3m buffer now
;;   "Open a blank w3m buffer."
;;   (interactive)
;;   (w3m-goto-url-new-session "about:"))

;; (defun w3m-new-tab () ;; TODO: need to rename buffer
;;   "Open a new tab in w3m."
;;   (interactive)
;;   (w3m-copy-buffer nil "*w3m*" nil t))

;; TODO: write a `w3m-download-with-wget' function
;; NOTE: this requires the `emacs-wget' package from ELPA
;; TODO: clean this up
;; (defun w3m-download-with-wget (loc)
;;   (interactive "DSave to: ")
;;   (let ((url (w3m-anchor)))
;;     (if url
;;         (wget url loc))))

;; COMMENT: ...
(setq w3m-form-textarea-edit-mode 'org-mode)

(add-hook 'w3m-form-input-textarea-mode-hook '(lambda nil (setq outline-regexp "=+")))


;; COMMENT: `w3m', `youtube-dl' and `mplayer'
;; NOTE: this doesn't work just yet
;; (defvar youtube-videos-directory nil "Directory location to save YouTube videos.")

;; (setq youtube-videos-directory "~/Videos/youtube/")

;; (defun w3m-youtube-video ()
;;   "..."
;;   (interactive)
;;   (let* ((video (w3m-print-current-url))
;; 	 (output (format "%s/%s.mp4" youtube-videos-directory video)))
;;     (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" output video)
;;     (emms-play-file output)))

;; ;; TODO: ...
;; (defun view-youtube-video (&rest junk)
;;   "Download and play a YouTube video."
;;   )

;; (defun w3m-youtube-view ()
;;   "View a YouTube link with youtube-dl and mplayer."
;;   (interactive)
;;   (let ((url (or (w3m-anchor) (w3m-image))))
;;     (string-match "[^v]*v.\\([^&]*\\)" url)
;;     (let* ((vid (match-string 1 url))
;;            (out (format "%s/%s.mp4" youtube-videos-directory vid)))
;;       (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" out url)
;;       (emms-play-file "out")
;;       ;;(start-process "mplayer" nil "mplayer" "-quiet" out)
;;       )))

;; ;; NOTE: this is redundant as I no longer primarily use chromium as my browser
;; (defun open-url-under-point-chromium (&rest junk)
;;   "If there is a valid URL under point, open that URL in chromium web-browser. Otherwise, open the URL of the current page in chromium web-browser.

;; NOTE: This function requires w3m to be running."
;;   (interactive)
;;   (let ((temp-url (w3m-print-this-url)))
;;     (if (not (eq temp-url nil))
;; 	(browse-url-chromium temp-url)
;;       (browse-url-chromium (w3m-print-current-url)))))

;; ;; COMMENT: w3m and save desktop mode
;; (defun w3m-register-desktop-save ()
;;   "Set `desktop-save-buffer' to a function returning the current URL."
;;   (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

;; (defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
;;   "Restore a `w3m' buffer on `save-desktop' load."
;;   (when (eq 'w3m-mode desktop-buffer-major-mode)
;;     (let ((url d-b-misc))
;;       (when url
;;         (require 'w3m)
;;         (if (string-match "^file" url)
;;             (w3m-find-file (substring url 7))
;;           (w3m-goto-url-new-session url))
;;         (current-buffer)))))

;; (add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))

;; ;; COMMENT: w3m mode hooks
(add-hook 'w3m-display-hook ;; NOTE: remove trailing whitespace in w3m buffer
	  (lambda (url)
	    (let ((buffer-read-only nil))
	      (delete-trailing-whitespace))))

;; (add-hook 'w3m-mode-hook 'w3m-register-desktop-save) ;; NOTE: ...

;; ;; COMMENT: w3m key-bindings
;; ;; (define-key w3m-mode-map (kbd "C-c n") '(lambda () (interactive) (open-blank-w3m))) ;; ERROR: this does not work
;; ;; (define-key w3m-mode-map (kbd "M") '(lambda () (interactive) (open-url-under-point-chromium))) ;; ERROR: this does not work

;; ;; COMMENT: this is meant to disable `ido-mode' in w3m buffers ... it does not work
;; ;; (put 'w3m 'ido 'ignore) 

;; ;; (defadvice ido-read-buffer (around ido-read-buffer-possibly-ignore activate)
;; ;;   "Check to see if use wanted to avoid using ido."
;; ;;   (if (eq (get this-command 'ido) 'ignore)
;; ;;       (let ((read-buffer-function nil))
;; ;;         (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
;; ;;         (setq ad-return-value (apply 'read-buffer (ad-get-args 0))))
;; ;;     ad-do-it))

;; ;; COMMENT: adding a new search engine
;; ;; NOTE: Find the entry point of the search engine you want to add, for example: (where foobar is the term you want to search for)
;; ;;  http://my.searchengine.com/?query=foobar
;; ;; NOTE: Then add info to your ~/.emacs-w3m file:
;; ;;  (eval-after-load "w3m-search" '(add-to-list 'w3m-search-engine-alist '("My engine" "http://my.searchengine.com/?query=%s" nil)))

;; ;; COMMENT: w3m search
;; ;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSearch'
;; (eval-after-load "w3m-search"
;;   '(setq w3m-search-engine-alist
;; 	 '(("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
;; 	   ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" utf-8)
;; 	   ("wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
;; 	   ("stanford" "http://plato.stanford.edu/search/searcher.py?query=%s" utf-8))))

;; ;; COMMENT: open web site in w3m
;; (defun browse-facebook (&rest junk)
;;   "Browse `http://m.facebook.com' with `w3m'."
;;   (interactive)
;;   (w3m-browse-url "http://m.facebook.com"))

;;; COMMENT: gist
;; SOURCE: `https://github.com/defunkt/gist.el'
(autoload 'gist-region-or-buffer "gist" "Integrate with Github." t)

;;; COMMENT: insert date and time
;; SOURCE: `http://www.emacswiki.org/emacs/InsertDate'
(defun insert-date (format)
  "Wrapper around format-time-string."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%c")))

;;; COMMENT: internet connection
(defvar internet-connections-alist (list "WiiBeard" "ANU-Secure" "belkin.90") "List of internet connections available.")
;; TODO: add potter's internet connection

(defun internet-connection (&rest junk) ;; TODO: have to clean this up (somehow)
  "Connect to the internet. A list of available connections is provided in the variable `internet-connections-alist'.

TODO: create a function to add a new connection (???)

NOTE: if the connection is succesful, the async shell command window should be closed.
      if the connection is not succesful, the async window should be centred."
  (interactive)
  (save-excursion
    (let ((connection (ido-completing-read "Select internet connection: " internet-connections-alist)))
      (message (concat "Connecting to internet connection \"" connection "\"..."))
      (shell-command (concat "nmcli con up id " connection " &"))) ;; TODO: add some sort of confirmation message
    (delete-other-windows)))

;;; COMMENT: screenshot
(defun screenshot (file-name &rest junk)
  "Take a screenshot."
  (interactive "sFile name: ")
  (save-excursion
    (shell-command (concat "import -window root \"" (expand-file-name file-name) "\" &")) ;; TODO: add some sort of confirmation message
    ;;(delete-other-windows)
    ))

;;; COMMENT: highlight custom comment tags
;; NOTE: i suppose technically this should be in the `appearance-config.el' file
(require 'custom-comments)

(setq custom-comment-suppress-init-message t) ;; NOTE: suppress initial confirmation message

(setq custom-comment-tag-alist-heading '("AUTHOR"
					 "SOURCE"
					 "COMMENT"
					 "FILE")) ;; NOTE: add `heading' tags to highlighting

(add-to-list 'custom-comment-tag-alist-heading "TIME") ;; NOTE: this is not in the default `heading' list

(setq custom-comment-tag-alist-comment '("IMPORTANT"
					 "NOTE"
					 "TODO")) ;; NOTE: add `comment' tags to highlighting

(setq custom-comment-tag-alist-warning '("BUG"
					 "DEBUG"
					 "ERROR"
					 "FIX"
					 "WARNING"
					 "TEST")) ;; NOTE: add `warning' tages to highlighting

(setq custom-comment-tag-mode-hooks '(emacs-lisp-mode-hook
				      lisp-mode-hook
				      shell-script-mode-hook
				      sh-mode-hook
				      conf-mode-hook
				      haskell-mode-hook
				      scheme-mode-hook
				      cc-mode-hook
				      c++-mode-hook
				      c-mode-hook
				      python-mode-hook
				      js-mode-hook
				      javascript-mode-hook
				      bibtex-mode
				      )) ;; NOTE: add `major-modes' to highlighting list

(activate-highlight-custom-comment-tags) ;; NOTE: activate custom comment tags

;;; COMMENT: configuration files
;; TODO: add `README' files
(require 'configuration-files)

;; TODO: incorporate "pair values" (i.e. [file,ext]) somehow
;; NOTE: could make this an alist?
(setq config-dir-and-ext '((concat user-emacs-directory "config-el/")
			   (concat user-emacs-directory "my-modes/")
			   (concat user-scripts-directory "bash-dir/")
			   (concat user-scripts-directory "conkeror-dir/")
			   (concat user-scripts-directory "stumpwm-dir/")))

(add-config-file (concat user-emacs-directory "init.el")) ;; NOTE: add `~/.conf-scripts/emacs-dir/init.el'
(add-config-directory (concat user-emacs-directory "config-el/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/config-el/'
(add-config-directory (concat user-emacs-directory "my-modes/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/my-modes/'
(add-config-directory (concat user-scripts-directory "bash-dir/") "\.sh$") ;; NOTE: add `*.sh' files in `~/.conf-scripts/bash-dir/'
(add-config-directory (concat user-scripts-directory "conkeror-dir/") ".js$") ;; NOTE: add `*.js' files in `~/.conf-scripts/conkeror-dir/'
(add-config-directory (concat user-scripts-directory "stumpwm-dir/") ".lisp$") ;; NOTE: add `*.lisp' files in `~/.conf-scripts/stumpwm-dir/'

;; TODO: something
;; (defun config-files-add-files (&rest junk)
;;   "..."
;;   )

;;; COMMENT: undo tree
;; SOURCE: `http://www.emacswiki.org/emacs/UndoTree'
(autoload 'global-undo-tree-mode "undo-tree" "Visualize the current buffer's undo tree." t)

(global-undo-tree-mode) ;; NOTE: enable undo-tree mode

;; COMMENT: insert license
(defun insert-mit-license (&rest junk)
  "..."
  (insert (concat "    Copyright (C) 2012 chu
    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
    documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the
    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
    persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
    Software.

    THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
    WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")))

(defun insert-gpl-license (&rest junk)
  "...")

;;; COMMENT: diary and calendar mode
;; SOURCE: `http://www.emacswiki.org/emacs/DiaryMode'
;; SOURCE: `http://www.emacswiki.org/emacs/CalendarMode'
;; (autoload 'calendar "calendar" "Keep a personal diary with GNU Emacs." t)

;; (setq view-diary-entries-initially t
;;       mark-diary-entries-in-calendar t
;;       diary-file "/home/chu/Documents/Organisation/diary"
;;       number-of-diary-entries 7)

;; (eval-after-load "calendar"
;;   (add-hook 'diary-display-hook 'fancy-diary-display)
;;   (add-hook 'today-visible-calendar-hook 'calendar-mark-today))

;;; COMMENT: the insidious big brother database
;; SOURCE: `http://www.emacswiki.org/emacs/BbdbMode'
;; (autoload 'bbdb "bbdb" "" t)

;; (eval-after-load "bbdb" '(bbdb-initialize 'gnus 'message))

;;(setq bbdb-file "~/.emacs.d/contacts-file.el")

;;; COMMENT: the emacs bibliography manager
;; SOURCE: `http://ebib.sourceforge.net/'
(autoload 'ebib "ebib" "A BibTeX database manager for GNU Emacs." t)

;; TODO: investigate @string clauses and abbreviations for common journals
;; TODO: create `philosophy.bib' `mathematics.bib' `linguistics.bib' `computer-science.bib' etc
(setq ebib-preload-bib-files (list (format "%su4537508.bib" user-university-directory) ;; NOTE: university courses
                                   (format "%sPapers/papers.bib" user-documents-directory) ;; NOTE: general papers
                                   ;; "/home/chu/Documents/Papers/papers.bib"
                                   ;; "/home/chu/Documents/ANU/u4537508.bib"
                                   ;; "/home/chu/Documents/Papers/philosophy.bib"
                                   ;; "/home/chu/Documents/Papers/mathematics.bib"
                                   ;; "/home/chu/Documents/Papers/linguistics.bib"
                                   ;; "/home/chu/Documents/Papers/computer-science.bib"
                                   )
      ebib-keywords-list (list "philosophy"
                               "mathematics"
                               "logic"
                               "computer science"
                               "linguistics"
                               "miscellaneous")
      ebib-autogenerate-keys t ;; NOTE: generate unique keys automatically
      ebib-file-search-dirs (list (format "%s" user-home-directory)
                                  (format "%sPapers/" user-documents-directory)) ;; NOTE: directories to search when viewing external files
      )

(eval-after-load "ebib" '(setcdr (assoc "pdf" ebib-file-associations) "evince"))

;; Ebib Entry: [[ebib:horwich1996][Horwich (1996)]]
;; Citation Entry: [[cite:horwich1996][Horwich (1996)]]

;; COMMENT: some personal ebib stuff
(defun ebib-export-directory (extension directory &rest junk)
  "Generates a BibTeX entry for all files with file-extension EXTENSION in directory DIRECTORY.

NOTE: This requires that each file in DIRECTORY be named according to \"<title>.EXTENSION\"."
  (mapc #'(lambda (file)
            ;; TODO: this needs to have a check whether `file' is already known, and if so, skip
	    (let ((title (replace-regexp-in-string "-" " " (file-name-nondirectory (file-name-sans-extension file))))
		  (author "")
		  (year "")
		  (tags nil))
	      (when (and (file-readable-p file) (not (file-directory-p file)))
		(insert
		 (format "@article{%s,\n	author = {%s},\n	title  = {%s},\n	year   = {%s},\n	file   = {%s}\n}\n\n"
			 (file-name-nondirectory (file-name-sans-extension file))
			 author
			 title
			 year
			 file)))))
	(directory-files directory t (concat "\." extension "$") t)))

(defun ebib-print-directory ()
  "Print the BibTeX entries from the TARGET-DIRECTORY variable, according to FILE-EXTENSION."
  (interactive)
  (let ((buffer-name "ebib-")
	(file-extension "pdf")
	(target-directory (format "%sPapers/PDFs/" user-documents-directory)))
    (switch-to-buffer "ebib-directory")
    (ebib-export-directory file-extension target-directory)))

;; TODO: can we do an `ido-completing-read' over a list of keys?
(defun org-insert-citation (key name)
  "Insert a BibTeX citation in an `org-mode' buffer, matching the `org-link' format."
  (interactive "sEnter key: \nsEnter name: ")
  (insert (format "[[%s][%s]]" key name)))

;;; COMMENT: deft
;; SOURCE: `http://jblevins.org/projects/deft/'
(autoload 'deft "deft" "Note taking with deft." t)

(setq deft-extension "org"
      deft-text-mode 'org-mode
      deft-directory (format "%s.deft/" user-organisation-directory))

;;; COMMENT: emacs speaks statistics
;; SOURCE: `http://ess.r-project.org/'
;;(autoload 'ess-mode "ess-mode" "Emacs Speaks Statistics." t)

(provide 'user-config)

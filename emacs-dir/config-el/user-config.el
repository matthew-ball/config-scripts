;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/user-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Wed 16 May 2012 22:40:57 EST

;;; COMMENT: emacs multimedia system
;; SOURCE: `http://emacswiki.org/cgi-bin/wiki/EMMS'
;; (autoload 'emms-all "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-default-players "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-player-mpd-connect  "emms-player-mode" "Interface between `EMMS' and `MPD'." t)

;; ;; (emms-devel) ;; DEBUG: apparently not what I want
;; (emms-all) ;; NOTE: runs `emms-standard' and adds stable `emms' features
;; (emms-default-players)

;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "7700")

;; (add-to-list 'emms-info-functions 'emms-info-mpd) ;; NOTE: get track information from `mpd'
;; (add-to-list 'emms-player-list 'emms-player-mpd) ;; NOTE: add `mpd' to the `emms' player list

;; (emms-player-mpd-connect) ;; NOTE: connect `emms' to `mpd'

;;; COMMENT: project management
;; SOURCE: `http://emacswiki.org/emacs/eproject'
;; (require 'eproject) ;; FIX: change this to an autoload
;; TODO: learn `eproject'

;;; COMMENT: default browser
(setq browse-url-browser-function 'w3m-browse-url ;; NOTE: use `w3m' web browser
      browse-url-new-window-flag t
      ;; browse-url-browser-function 'browse-url-generic ;; NOTE: use generic web browser
      browse-url-generic-program "conkeror" ;; NOTE: default web browser set to `conkeror'
      ;; browse-url-generic-program "chromium-browser" ;; NOTE: default web browser set to `chromium-browser'
      ;; browse-url-generic-program "x-www-browser" ;; NOTE: default web browser set to `x-www-browser'
      )

;;; COMMENT: dictionary and thesaurus
;; SOURCE: `http://emacswiki.org/emacs/dict.el'
;; SOURCE: `http://emacswiki.org/emacs/thesaurus.el'
;; TODO: find a dictionary/thesaurus combination
;; (autoload 'thesaurus-choose-synonym-and-replace "thesaurus" "Choose and replace a word with it's synonym." t)

;; (require 'thesaurus) ;; TODO: change this to an autoload

;; (setq thesaurus-bhl-api-key "8c5a079b300d16a5bb89246322b1bea6")  ;; NOTE: from registration

;; (define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace) ;; NOTE: optional key binding (TODO: move to bindings.el)

(defun dictionary-word (&rest junk) ;; TODO: i don't like how this one works
  "Dictionary definition of the current word."
  (interactive)
  (w3m-goto-url (format "http://dictionary.reference.com/browse/%s" (read-string "Search word: " (current-word)))))

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

;; COMMENT: interface
(setq w3m-key-binding 'info ;; NOTE: use sane key-bindings
      w3m-home-page "www.emacswiki.org"
      ;; w3m-default-display-inline-images t ;; NOTE: display images by default
      w3m-use-toolbar nil
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;; COMMENT: cookies
(require 'w3m-cookie) ;; NOTE: enable cookies support in w3m

(setq w3m-use-cookies t ;; NOTE: use cookies in w3m
      w3m-cookie-file (concat (expand-file-name user-emacs-directory) "w3m/cookie") ;; NOTE: save cookies to ~/.emacs.d/w3m/cookie
      w3m-cookie-accept-bad-cookies t
      w3m-cookie-accept-domains '("www.emacswiki.org" "www.google.com" "www.wikipedia.org" "www.github.com"))

;; COMMENT: w3m sessions
(setq w3m-make-new-session t) ;; NOTE: open a new tab by typing RET on a url string
(setq w3m-use-tab t) ;; NOTE: C-c C-t creates new tab with line below

;; COMMENT: w3m control and external browser support
(defun open-blank-w3m (&rest junk) ;; NOTE: this is redundant - \\[w3m] just opens a blank w3m buffer now
  "Open a blank w3m buffer."
  (interactive)
  (w3m-goto-url-new-session "about:"))

(defun w3m-new-tab () ;; TODO: need to rename buffer
  "Open a new tab in w3m."
  (interactive)
  (w3m-copy-buffer nil "*w3m*" nil t))

(defun open-url-under-point-chromium (&rest junk) ;; NOTE: this is redundant as I no longer primarily use chromium as my browser
  "If there is a valid URL under point, open that URL in chromium web-browser. Otherwise, open the URL of the current page in chromium web-browser.

NOTE: This function requires w3m to be running."
  (interactive)
  (let ((temp-url (w3m-print-this-url)))
    (if (not (eq temp-url nil))
	(browse-url-chromium temp-url)
      (browse-url-chromium (w3m-print-current-url)))))

;; COMMENT: w3m and save desktop mode
(defun w3m-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the current URL."
  (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

(defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `w3m' buffer on `save-desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
    (let ((url d-b-misc))
      (when url
        (require 'w3m)
        (if (string-match "^file" url)
            (w3m-find-file (substring url 7))
          (w3m-goto-url-new-session url))
        (current-buffer)))))

(add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))

;; COMMENT: w3m mode hooks
(add-hook 'w3m-display-hook ;; NOTE: remove trailing whitespace in w3m buffer
	  (lambda (url)
	    (let ((buffer-read-only nil))
	      (delete-trailing-whitespace))))

(add-hook 'w3m-mode-hook 'w3m-register-desktop-save) ;; NOTE: ...

;; COMMENT: w3m key-bindings
;; (define-key w3m-mode-map (kbd "C-c n") '(lambda () (interactive) (open-blank-w3m))) ;; ERROR: this does not work
;; (define-key w3m-mode-map (kbd "M") '(lambda () (interactive) (open-url-under-point-chromium))) ;; ERROR: this does not work

;; COMMENT: this is meant to disable `ido-mode' in w3m buffers ... it does not work
;; (put 'w3m 'ido 'ignore) 

;; (defadvice ido-read-buffer (around ido-read-buffer-possibly-ignore activate)
;;   "Check to see if use wanted to avoid using ido."
;;   (if (eq (get this-command 'ido) 'ignore)
;;       (let ((read-buffer-function nil))
;;         (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
;;         (setq ad-return-value (apply 'read-buffer (ad-get-args 0))))
;;     ad-do-it))

;; COMMENT: adding a new search engine
;; NOTE: Find the entry point of the search engine you want to add, for example: (where foobar is the term you want to search for)
;;  http://my.searchengine.com/?query=foobar
;; NOTE: Then add info to your ~/.emacs-w3m file:
;;  (eval-after-load "w3m-search" '(add-to-list 'w3m-search-engine-alist '("My engine" "http://my.searchengine.com/?query=%s" nil)))

;; COMMENT: w3m search
;; SOURCE: `http://www.emacswiki.org/emacs/WThreeMSearch'
(eval-after-load "w3m-search"
  '(setq w3m-search-engine-alist
	 '(("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
	   ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" utf-8)
	   ("wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))))

;; COMMENT: open web site in w3m
(defun browse-facebook (&rest junk)
  "Browse `http://m.facebook.com' with `w3m'."
  (interactive)
  (w3m-browse-url "http://m.facebook.com"))

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
(defvar internet-connections-alist (list "WiiBeard" "ANU-Secure") "List of internet connections available.")
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
    (shell-command (concat "import -window root \"" file-name "\" &")) ;; TODO: add some sort of confirmation message
  ;; (delete-other-windows)
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
				      haskell-mode-hook
				      scheme-mode-hook
				      cc-mode-hook
				      c++-mode-hook
				      c-mode-hook
				      python-mode-hook
				      js-mode-hook
				      javascript-mode-hook)) ;; NOTE: add `major-modes' to highlighting list

(activate-highlight-custom-comment-tags) ;; NOTE: activate custom comment tags

;;; COMMENT: configuration files
;; TODO: add `README' files
(require 'configuration-files)
(add-config-file (concat user-emacs-directory "init.el")) ;; NOTE: add `~/.conf-scripts/emacs-dir/init.el'
(add-config-directory (concat user-emacs-directory "config-el/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/config-el/'
(add-config-directory (concat user-emacs-directory "my-modes/") "\.el$") ;; NOTE: add `*.el' files in `~/.conf-scripts/emacs-dir/my-modes/'
(add-config-directory (concat user-scripts-directory "bash-dir/") "\.sh$") ;; NOTE: add `*.sh' files in `~/.conf-scripts/bash-dir/'
(add-config-directory (concat user-scripts-directory "conkeror-dir/") ".js$") ;; NOTE: add `*.js' files in `~/.conf-scripts/conkeror-dir/'
(add-config-directory (concat user-scripts-directory "stumpwm-dir/") ".lisp$") ;; NOTE: add `*.lisp' files in `~/.conf-scripts/stumpwm-dir/'

(provide 'user-config)

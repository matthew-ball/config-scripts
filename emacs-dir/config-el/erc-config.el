;; FILE: ~/.emacs.d/config-el/erc-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: erc
(autoload 'doctor-doc "doctor") ;; for use with erc doctor
(autoload 'make-doctor-variables "doctor") ;; ...
(autoload 'erc-select "erc" "The GNU Emacs IRC client." t)
(autoload 'erc-match "erc" "ERC mode for checking whether messages match certain patterns." t)
(autoload 'erc-join "erc" "ERC mode for automatically joining channels." t)
(autoload 'erc-track "erc" "ERC mode for tracking activity in channels." t)
(autoload 'erc-fill "erc" "ERC mode for automatically filling messages." t)
(autoload 'erc-ring "erc" "ERC mode for recalling previous messages (i.e. stores them in a ring)." t)
(autoload 'erc-netsplit "erc" "ERC mode for handling netsplits (i.e. rejoin)." t)
(autoload 'erc-spelling "erc" "ERC mode for interaction with flyspell (aspell)." t)
(autoload 'erc-pcomplete "erc" "ERC mode for completing the nickname before point." t)
(autoload 'erc-highlight-nicknames "erc" "ERC mode for highlighting nicknames." t)
(autoload 'wtf-is "wtf" "ERC command for describing acronyms." t)

;; (require 'erc-match)
;; (require 'erc-join)
;; (require 'erc-track)
;; (require 'erc-fill)
;; (require 'erc-ring)
;; (require 'erc-netsplit)
;; (require 'erc-spelling)
;; (require 'erc-pcomplete)
;; (require 'erc-highlight-nicknames)

(and (require 'erc-highlight-nicknames) (add-to-list 'erc-modules 'highlight-nicknames) (erc-update-modules))

(defface erc-prompt-face '((t (:foreground "yellow" :bold t))) "ERC prompt.")
(defvar erc-insert-post-hook)

(erc-autojoin-mode t) ;; enable autojoining
(erc-track-mode t)
(erc-match-mode t)
(erc-fill-mode 0) ;; disable ERC fill
(erc-fill-disable) ;; disable ERC fill
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t) ;; enable ERC timestamp on
(erc-button-mode t)
(erc-spelling-mode t) ;; enable flyspell in ERC

(setq erc-server "irc.freenode.net" ;; default ERC server
      erc-port 6667 ;; default ERC port
      erc-nick "chu"
      erc-user-full-name user-full-name
      erc-email-userid "mathew.ball@gmail.com"
      erc-current-nick-highlight-type 'all ;; highlight the entire messahe where current nickname occurs
      erc-timestamp-format "[%H:%M] " ;; put timestamps on the left
      erc-fill-prefix "        " ;; ...
      erc-fill-column 90
      erc-fill-mode 0 ;; again, disable ERC fill (not sure why I have done it in two places)
      erc-timestamp-right-column 61
      erc-track-showcount t ;; show count of unseen messages
      erc-timestamp-only-if-changed-flag nil ;; always show timestamp
      erc-insert-timestamp-function 'erc-insert-timestamp-left ;; insert timestamp in the left column
      erc-kill-buffer-on-part t ;; kill buffers for channels after /part
      erc-kill-queries-on-quit t ;; kill buffers for queries after quitting the server
      erc-kill-server-buffer-on-quit t ;; kill buffers for server messages after quitting the server
      erc-interpret-mirc-color t ;; interpret mIRC-style color commands in IRC chats
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT") ;; ignore JOIN, NICK, PART and QUIT messages
      erc-mode-line-format "%t %a" ;; display only the channel name on the mode-line
      erc-header-line-format nil ;; turn off the topic (header) bar
      header-line-format nil ;; turn off the topic (header) bar
      erc-max-buffer-size 20000 ;; truncate buffers (so they don't hog core)
      erc-truncate-buffer-on-save t
      erc-input-line-position -1 ;; keep input at the bottom
      ;; erc-keywords '("") ;; highlight specific keywords
      erc-echo-notices-in-minibuffer-flag t ;; notices in minibuffer
      erc-prompt ;; channel specific prompt
      (lambda () (if (and (boundp 'erc-default-recipients) (erc-default-target))
		(erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
	      (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
      erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs" "#stumpwm" "#conkeror" "#lisp" "#haskell" "#org-mode" "#ubuntu-offtopic" "#debian-offtopic" "##math" "##programming" "##club-ubuntu"))
      erc-join-buffer 'bury)

(setq erc-modules (delq 'fill erc-modules)) ;; disable erc-fill-mode

;; (add-hook 'erc-after-connect '(lambda (SERVER NICK) (erc-message "PRIVMSG" "NickServ identify password"))) ;; authentication details
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(add-hook 'erc-mode-hook '(lambda () (pcomplete-erc-setup) (erc-completion-mode 1)))
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

(remove-hook 'erc-text-matched-hook 'erc-hide-fools)

(setq erc-pals '()
      erc-fool-highlight-type 'nick ;; highlight entire message
      erc-fools '("ubottu" "fsbot" "rudybot" "lisppaste"))

(setq erc-remove-parsed-property nil)

(defvar erc-doctor-id "{Emacs doctor} ") ;; erc doctor

(defun erc-cmd-DOCTOR (&optional last-sender &rest ignore)
  "Get the last message in the channel and doctor it."
  (let ((limit (- (point) 1000))
        (pos (point))
        doctor-buffer
        last-message
        text)
    (when (< limit 0) (setq limit 0)) ;; make sure limit is not negative
    (while (and pos (not (let ((data (get-text-property pos 'erc-parsed))) ;; search backwards for text from someone
                           (and data
                                (string= (aref data 3) "PRIVMSG")
                                (or (not last-sender)
                                    (string= (car (split-string (aref data 2) "!"))
                                             last-sender))))))
      (setq pos (previous-single-property-change
                 pos 'erc-parsed nil limit))
      (when (= pos limit)
        (error "No appropriate previous message to doctor.")))
    (when pos
      (setq last-sender (car (split-string
                              (aref (get-text-property
                                     pos 'erc-parsed) 2) "!"))
            doctor-buffer (concat "*ERC Doctor: " last-sender "*")
            last-message (split-string
                          (replace-regexp-in-string ;; remove punctuation from end of sentence
                           "[ .?!;,/]+$" ""
                           (aref (get-text-property pos
                                                    'erc-parsed) 5)))
            text (mapcar (lambda (s)
                           (intern (downcase s)))
                         (if (string-match ;; remove salutation if it exists
                              (concat "^" erc-valid-nick-regexp
                                      "[:,]*$\\|[:,]+$")
                              (car last-message))
                             (cdr last-message)
                           last-message))))
    (erc-send-message
     (concat erc-doctor-id
             (if (not (erc-query-buffer-p)) ;; only display sender if not in a query buffer
                 (concat last-sender ": "))
             (save-excursion
               (if (get-buffer doctor-buffer)
                   (set-buffer doctor-buffer)
                 (set-buffer (get-buffer-create doctor-buffer))
                 (make-doctor-variables))
               (erase-buffer)
               (doctor-doc text)
               (buffer-string))))))

(defun erc-cmd-SHOW (&rest form)
  "Evaluate FORM and send the result and the original form as: FORM => (eval FORM)."
  (let ((string
         (with-temp-buffer
           (mapc #'(lambda (f) (insert f " ")) form)
           (goto-char (point-min))
           (setq form (read (current-buffer)))
           (let ((res (condition-case err
                          (eval form)
                        (error
                         (format "Error: %s" err)))))
             (insert (format " => %s" res)))
           (buffer-substring-no-properties
            (point-min) (1- (point-max))))))
    (erc-send-message string)))

(defun erc-cmd-UNAME (&rest ignore)
  "Display the result of running `uname -a' to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          "[ \n]+$" "" (shell-command-to-string "uname -a"))))
    (erc-send-message
     (concat "{uname -a} [" uname-output "]"))))

(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
	  ", load average: " "] {Load average} ["
	  ;; collapse spaces, remove
	  (replace-regexp-in-string
	   " +" " "
	   ;; remove beginning and trailing whitespace
	   (replace-regexp-in-string
	    "^ +\\|[ \n]+$" ""
	    (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

(defun erc-cmd-WTF (term &rest ignore) ;; ERROR: this doesn't work
  "Look up definition for TERM."
  (let ((def (wtf-is term)))
    (if def
        (erc-send-message
         (concat "{Term} " (upcase term) " is " def))
      (message (concat "No definition found for " (upcase term))))))

(add-to-list 'erc-noncommands-list 'erc-cmd-SHOW)

(defun erc-start-or-switch (&rest junk)
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; if ERC is already active ...
      (erc-track-switch-buffer 1) ;; switch to last active buffer
    (erc :server "irc.freenode.net" :port erc-port :nick erc-nick :full-name erc-user-full-name))) ;; else, start ERC

(provide 'erc-config)

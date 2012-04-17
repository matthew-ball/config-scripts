;; FILE: ~/.emacs.d/config-el/user-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)

;;; COMMENT: emacs multimedia system
;; (autoload 'emms-all "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-default-players "emms-setup" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-player-mpd-connect  "emms-player-mode" "Interface between EMMS and MPD." t)

;; ;; (emms-devel)
;; (emms-all) ;; NOTE: runs `emms-standard' and adds stable emms features
;; (emms-default-players)

;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "7700")

;; (add-to-list 'emms-info-functions 'emms-info-mpd) ;; NOTE: get track information from mpd
;; (add-to-list 'emms-player-list 'emms-player-mpd) ;; NOTE: add mpd to the emms player list

;; (emms-player-mpd-connect) ;; NOTE: connect emms to mpd

;;; COMMENT: project management
;; (require 'eproject) ;; FIX: change this to an autoload
;; TODO: learn eproject

;;; COMMENT: default browser
(setq browse-url-browser-function 'w3m-browse-url ;; NOTE: use w3m web browser
      browse-url-new-window-flag t
      ;; browse-url-browser-function 'browse-url-generic ;; NOTE: use generic web browser
      ;; browse-url-generic-program "conkeror" ;; default web browser set to conkeror
      browse-url-generic-program "chromium-browser" ;; default web browser set to chromium-browser
      ;; browser-url-generic-program "x-www-browser" ;; default web browser set to x-www-browser (NOTE: this may be Debian only?)
      )

;;; COMMENT: w3m browser
;; (require 'w3m-load) ;; TEST: this still needs to be tested

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-goto-url-new-session "w3m" "Go to a URL in a new w3m buffer." t)
;; (autoload 'w3m-mode-map "w3m" "The mode map for w3m." t) ;; ERROR: this does not work

(setq w3m-key-binding 'info
      ;; w3m-home-page "www.emacswiki.org"
      ;; w3m-default-display-inline-images t
      w3m-use-toolbar nil
      w3m-use-cookies t
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

(defun open-blank-w3m (&rest junk)
  "Open a blank w3m buffer."
  (interactive)
  (w3m-goto-url-new-session "about:"))

;; (define-key w3m-mode-map (kbd "C-c n") '(lambda () (interactive) (open-blank-w3m))) ;; ERROR: this does not work

(defun open-url-under-point-chromium (&rest junk)
  "If there is a valid URL under point, open that URL in chromium web-browser. Otherwise, open the URL of the current page in chromium web-browser.

NOTE: This function requires w3m to be running."
  (interactive)
  (let ((temp-url (w3m-print-this-url)))
    (if (not (eq temp-url nil))
	(browse-url-chromium temp-url)
      (browse-url-chromium (w3m-print-current-url)))))

(defun w3m-new-tab ()
  "Open a new tab in w3m."
  (interactive)
  (w3m-copy-buffer nil nil nil t))

;; (define-key w3m-mode-map (kbd "M") '(lambda () (interactive) (open-url-under-point-chromium))) ;; ERROR: this does not work

(defun w3m-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the current URL."
  (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

(add-hook 'w3m-mode-hook 'w3m-register-desktop-save)

(defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `w3m' buffer on `desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
    (let ((url d-b-misc))
      (when url
        (require 'w3m)
        (if (string-match "^file" url)
            (w3m-find-file (substring url 7))
          (w3m-goto-url-new-session url))
        (current-buffer)))))

(add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))

;;; COMMENT: internet connection
(defvar internet-connections (list "WiiBeard" "ANU-Secure") "List of internet connections available.")

;; TODO: have to clean this up (somehow)
(defun internet-connection (&rest junk)
  "Connect to the internet."
  (interactive)
  (let ((connection (ido-completing-read "Select internet connection: " internet-connections)))
    (message (concat "Connecting to internet connection \"" connection "\"..."))
    (shell-command (concat "nmcli con up id " connection " &"))))

;;; COMMENT: screenshot
(defun screenshot (file-name &rest junk)
  "Take a screenshot."
  (interactive "sFile name: ")
  (shell-command (concat "import -window root \"" file-name "\" &"))
  (delete-other-windows))

(provide 'user-config)

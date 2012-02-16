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
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(setq w3m-key-binding 'info
      w3m-home-page "www.emacswiki.org"
      ;; w3m-default-display-inline-images t
      w3m-use-toolbar nil
      w3m-use-cookies t
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

(provide 'user-config)

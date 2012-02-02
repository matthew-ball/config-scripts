;; ===================================
;; ~/.emacs.d/config-el/user-config.el
;; Matthew Ball (copyleft 2012)
;; ===================================

;;; emacs multimedia system
(autoload 'emms-all "emms-setup" "Start a GNU Emacs multimedia system session." t)
(autoload 'emms-default-players "emms-setup" "Start a GNU Emacs multimedia system session." t)
(autoload 'emms-player-mpd-connect  "emms-player-mode" "Interface between EMMS and MPD." t)

;; (emms-devel)
(emms-all)
(emms-default-players)

(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "7700")

(add-to-list 'emms-info-functions 'emms-info-mpd) ;; get track information from mpd
(add-to-list 'emms-player-list 'emms-player-mpd) ;; add mpd to the emms player list

(emms-player-mpd-connect) ;; connect emms to mpd

;;; project management
;; (require 'eproject) ;; FIXME: change this to an autoload
;; TODO: learn eproject

;;; document headers
(defun insert-custom-dot-file-header-text () ;; TODO: still a bit buggy?
  "Insert the header string for a dot file."
  (interactive)
  (insert (concat (make-string 2 (aref comment-start 0)) " " (buffer-file-name) "\n"
		  (concat (make-string 2 (aref comment-start 0)) " " (user-full-name)
			  " (copyleft " (substring (shell-command-to-string "date +\"%Y\"") 0 4) ")")))) ;; need (substring ...) otherwise we pick up the \n character

(provide 'user-config)

;; ~/.emacs.d/config-el/user-config.el
;; Matthew Ball (copyleft 2012)

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

(provide 'user-config)

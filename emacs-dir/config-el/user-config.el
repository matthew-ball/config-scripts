;; ===================================
;; ~/.emacs.d/config-el/user-config.el
;; Matthew Ball (copyleft 2012)
;; ===================================

;;; emacs multimedia system
(require 'emms-setup) ;; FIXME: change this to an autoload
(require 'emms-player-mpd) ;; FIXME: change this to an autoload
;; (autoload 'emms "emms-playlist-mode" "Start a GNU Emacs multimedia system session." t)
;; (autoload 'emms-player-mpd-connect  "emms-player-mode" "Interface between EMMS and MPD." t)

;; (emms-devel)
(emms-all)
(emms-default-players)

(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "7700")

(add-to-list 'emms-info-functions 'emms-info-mpd) ;; get track information from mpd
(add-to-list 'emms-player-list 'emms-player-mpd) ;; add mpd to the emms player list

(emms-player-mpd-connect) ;; connect emms to mpd

(provide 'user-config)

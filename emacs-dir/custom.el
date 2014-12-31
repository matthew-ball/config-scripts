;; FILE: /home/chu/.conf-scripts/emacs-dir/custom.el
;; AUTHOR: Matthew Ball (copyleft 2012)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" default)))
 '(doc-view-continuous t)
 '(ebib-entry-types
   (quote
    ((course
      (lecturer title semester year)
      nil)
     (article
      (author title journal year)
      (volume number pages month note))
     (book
      (author title publisher year)
      (editor volume number series address edition month note))
     (booklet
      (title)
      (author howpublished address month year note))
     (inbook
      (author title chapter pages publisher year)
      (editor volume series address edition month note))
     (incollection
      (author title booktitle publisher year)
      (editor volume number series type chapter pages address edition month note))
     (inproceedings
      (author title booktitle year)
      (editor pages organization publisher address month note))
     (manual
      (title)
      (author organization address edition month year note))
     (misc nil
	   (title author howpublished month year note))
     (mastersthesis
      (author title school year)
      (address month note))
     (phdthesis
      (author title school year)
      (address month note))
     (proceedings
      (title year)
      (editor publisher organization address month note))
     (techreport
      (author title institution year)
      (type number address month note))
     (unpublished
      (author title note)
      (month year)))))
 '(ecb-options-version "2.40")
 '(org-agenda-files
   (quote
    ("/home/chu/Documents/journal.org" "/home/chu/Documents/notes.org"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-current-nick-face ((t (:weight bold))))
 '(erc-input-face ((t nil)))
 '(erc-pal-face ((t (:foreground "light gray" :weight bold))))
 '(erc-prompt-face ((t (:inherit erc-prompt :foreground "black" :weight bold))))
 '(erc-timestamp-face ((t (:inherit erc-timestamp :foreground "black" :weight bold))))
 '(info-header-xref ((t (:inherit info-xref :foreground "dark slate gray" :weight bold)))))

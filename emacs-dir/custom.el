;; FILE: /home/chu/.conf-scripts/emacs-dir/custom.el
;; AUTHOR: Matthew Ball (copyleft 2012)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
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
 '(org-agenda-files
   (quote
    ("~/Documents/Organisation/thailand-trip.org" "~/Documents/Organisation/europe-trip.org" "/home/chu/Documents/Organisation/school.org" "/home/chu/Documents/Organisation/journal.org" "/home/chu/Documents/Organisation/projects.org" "/home/chu/Documents/Organisation/home.org" "/home/chu/Documents/Organisation/birthday.org"))))

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

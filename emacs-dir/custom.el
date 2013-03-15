;; FILE: /home/chu/.conf-scripts/emacs-dir/custom.el
;; AUTHOR: Matthew Ball (copyleft 2012)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ebib-entry-types (quote ((course (lecturer title semester year) nil) (article (author title journal year) (volume number pages month note)) (book (author title publisher year) (editor volume number series address edition month note)) (booklet (title) (author howpublished address month year note)) (inbook (author title chapter pages publisher year) (editor volume series address edition month note)) (incollection (author title booktitle publisher year) (editor volume number series type chapter pages address edition month note)) (inproceedings (author title booktitle year) (editor pages organization publisher address month note)) (manual (title) (author organization address edition month year note)) (misc nil (title author howpublished month year note)) (mastersthesis (author title school year) (address month note)) (phdthesis (author title school year) (address month note)) (proceedings (title year) (editor publisher organization address month note)) (techreport (author title institution year) (type number address month note)) (unpublished (author title note) (month year)))))
 '(org-agenda-files (quote ("~/Documents/ANU/PHIL2094/phil2094.org" "~/Documents/ANU/LING1001/ling1001.org" "/home/chu/Documents/Organisation/school.org" "/home/chu/Documents/Organisation/journal.org" "/home/chu/Documents/Organisation/projects.org" "/home/chu/Documents/Organisation/home.org" "/home/chu/Documents/Organisation/contacts.org" "/home/chu/Documents/Organisation/birthday.org" "/home/chu/Documents/Organisation/bookmarks.org")))
 '(wg-prefix-key "w"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-timestamp-face ((t (:foreground "gray" :weight bold))))
 '(info-header-xref ((t (:inherit info-xref :foreground "dark slate gray"))))
 '(wg-brace-face ((t (:foreground "dim gray"))))
 '(wg-command-face ((t (:foreground "blue"))))
 '(wg-current-workgroup-face ((t (:foreground "dim gray"))))
 '(wg-divider-face ((t (:foreground "black"))))
 '(wg-filename-face ((t (:foreground "blue"))))
 '(wg-frame-face ((t (:foreground "dim gray"))))
 '(wg-message-face ((t (:foreground "blue"))))
 '(wg-mode-line-face ((t (:foreground "blue"))))
 '(wg-previous-workgroup-face ((t (:foreground "deep sky blue")))))

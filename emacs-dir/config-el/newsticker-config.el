;; FILE: /home/chu/.conf-scripts/emacs-dir/config-el/newsticker-config.el
;; AUTHOR: Matthew Ball (copyleft 2012)
;; TIME: Mon 20 Aug 2012 10:40:59 EST

(setq newsticker-dir "~/News/newsticker"
      newsticker-url-list-defaults nil
      newsticker-automatically-mark-items-as-old t
      newsticker-automatically-mark-visited-items-as-old t
      newsticker-retrieval-interval 600
      newsticker-html-renderer 'w3m-region
      newsticker-retrieval-method 'extern
      newsticker-treeview-treewindow-width 40
      newsticker-treeview-listwindow-height 30
      newsticker-wget-arguments '("-q" "-O" "-" "--user-agent" "testing"))

(setq newsticker-url-list
      '(("EmacsWiki" "http://www.emacswiki.org/emacs?action=rss" nil nil nil)
	("ANU" "http://anubis.anu.edu.au/billboard/rss_feed_news.asp" nil nil nil)
	("CECS News" "http://cecs.anu.edu.au/rss/" nil nil nil)
	("CECS Seminars" "http://cecs.anu.edu.au/seminars/rss.pl" nil nil nil)
        ))

(provide 'newsticker-config)


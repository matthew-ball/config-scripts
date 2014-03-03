;;; wireless --- Display wireless status information

;; Copyright (C) 2007, 2008 Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;; 
;; Derived from battery.el by Ralph Schleicher
;; <rs@nunatak.allgaeu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; To install this library, place it in your load path with:
;;
;; (add-to-list 'load-path "/PATH/TO/wireless")
;;
;; Then add to your .emacs:
;;
;; (require 'wireless)
;; (display-wireless-mode t)

;;; History:
;; 
;; Originally written in October of 2007.
;;
;; Thank you to Ralph Schleicher for kindly and patiently repling to
;; my emails about battery.el.

;;; Code:

(require 'timer)
(require 'net-utils)

(defgroup wireless nil
  "*Display wireless status information."
  :prefix "wireless-"
  :group 'wireless)

(defcustom wireless-procfile "/proc/net/wireless"
  "*Location of the wireless information file."
  :type 'string
  :group 'wireless)

(defcustom wireless-procfile-regexp
  "^[ ]+\\(.+\\):[ ]+\\([0-9]+\\)[ ]+\\([0-9]+\\)[ .]+\\(-?[0-9]+\\)[ .]+\\(-?[0-9]+\\)"
  "*Regular expression for extracting proc-file data."
  :type 'string
  :group 'wireless)

(defcustom wireless-mode-line-format "[%k]"
  "*Format string for mode-line display of the wireless status.
All characters are printed as-is unless they are prefixed with %,
in which case both the prefix and the control character are
replaced with data corresponding to the following table (to print
a single percent sign use \"%%\"):

%n, name of the network interface
%k, quality level (percentile) for the primary wireless network interface
%l, signal level
%s, noise level

If any of the above are not available, \"N/A\" will be displayed
instead."
  :type 'string
  :group 'wireless)

(defcustom wireless-remove-when-no-link-p t
  "*Control the display of the link status.
When true, the link status will be removed from the mode-line
completely if there is no wireless info."
  :type 'boolean
  :group 'wireless)

(defcustom wireless-remove-when-zero-link-p t
  "*Control the display of the link status.
When true, the link status will be removed from the mode-line
completely if the wireless quality is zero."
  :type 'boolean
  :group 'wireless)

(defvar wireless-mode-line-string ""
  "String to display in the mode line.")

(defvar wireless-update-timer nil
  "Interval timer object.")

(defun wireless-info-readable-p ()
  "Return true if there is readable data available."
  (and (eq system-type 'gnu/linux)
       (file-readable-p wireless-procfile)))

;; This function should have separate COND clauses for each supported
;; system and should always return a list like:
;;
;; (list name status link level noise)
;;
(defun wireless-read-interface-line ()
  "Return a list of the interface data."
  (cond ((eq system-type 'gnu/linux)
	 (let (name status link level noise)
	   (re-search-forward wireless-procfile-regexp (point-at-eol) t)
	   (setq name (match-string 1)
		 status (match-string 2)
		 link (match-string 3)
		 level (match-string 4)
		 noise (match-string 5))
	   (list name status link level noise)))))

;; This function should be system type agnostic. When there is no
;; wireless interface at all, it should always return NIL.
(defun wireless-read ()
  "Return a list of all interfaces with their data."
  (let (result)
    (with-temp-buffer
      (condition-case nil
	  (insert-file-contents wireless-procfile)
	(error nil))
      (forward-line 2) ;; go past the comments
      (while (not (eobp))
	(goto-char (point-at-bol))
	(setq result
	      (append result (list (wireless-read-interface-line))))
	(forward-line 1)))
    result))

(defun wireless-format-function ()
  "Return an alist that `wireless-format' can parse."
  (let* ((primary-interface (car (wireless-read)))
	 (name (nth 0 primary-interface))
	 (link (nth 2 primary-interface))
	 (level (nth 3 primary-interface))
	 (noise (nth 4 primary-interface)))
    (list (cons ?n (or name "N/A"))
	  (cons ?k (or link "N/A"))
	  (cons ?l (or level "N/A"))
	  (cons ?s (or noise "N/A")))))

(defun wireless-format (format)
  "Substitute %-sequences in FORMAT."
  (let ((alist (wireless-format-function)))
    (replace-regexp-in-string
     "%."
     (lambda (str)
       (let ((char (aref str 1)))
	 (if (eq char ?%) "%"
	   (or (cdr (assoc char alist)) ""))))
     format t t)))

(defun wireless-update ()
  "Update wireless status information in the mode line."
  (setq wireless-mode-line-string
	(if (or (and wireless-remove-when-no-link-p 
		     (not (wireless-info-readable-p)))
		(and wireless-remove-when-zero-link-p
		     (wireless-info-readable-p)
		     (= (string-to-number (nth 2 (car (wireless-read)))) 0)))
	    ""
	  (wireless-format wireless-mode-line-format)))
  (force-mode-line-update))

(defun wireless-update-handler ()
  "Handler function for updating the mode-line."
  (wireless-update)
  (sit-for 0))

;; If there are multiple wireless interfaces, they will be displayed
;; here.
(defun wireless ()
  "Display a message with wireless interface data."
  (interactive)
  (when (not (wireless-info-readable-p))
    (error "No wireless info available or unsupported system type"))
  (let ((data (wireless-read))
	(str ""))
    (mapc
     #'(lambda (e)
	 (setq str (concat str (format "%s quality is at %s. " (nth 0 e) (nth 2 e)))))
     data)
    (message str)))

;;;###autoload
(define-minor-mode display-wireless-mode
  "Display wireless status information."
  :global t :group 'wireless
  (setq wireless-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and wireless-update-timer (cancel-timer wireless-update-timer))
  (if (not display-wireless-mode)
      (setq global-mode-string
	    (delq 'wireless-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'wireless-mode-line-string t)
    (setq wireless-update-timer (run-at-time nil 60
					     'wireless-update-handler))
    (wireless-update)))

(provide 'wireless)

;;; wireless.el ends here

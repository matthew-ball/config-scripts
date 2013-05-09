;;; mode-line-hack.el --- vim style mode line display

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Jaeyoun Chung <jay@kldp.org>
;; Keywords: convenience

;;; Commentary:

;; Let the modeline disappear unless multiple windows are shown. But mode
;; line is temporarily shown M-x what-cursor-position (C-x =) is called.

;;; Code:

(require 'cl)

(defvar org-mode-line-format nil "I will hold the original value of `mode-line-format'.")

(make-variable-buffer-local 'org-mode-line-format)

(put 'org-mode-line-format 'permanent-local t)

;;(setq-default header-line-format '("" (:eval (header-liner))))

(add-hook 'window-configuration-change-hook 'update-mode-line-format)

(defun update-mode-line-format ()
  "mode line is shown iff more than one-window is displayed."
  (when (and mode-line-format org-mode-line-format)
    (setq org-mode-line-format nil))
  (when (or (and (one-window-p) mode-line-format)
	    (and (not (one-window-p)) (null mode-line-format)))
    (rotatef mode-line-format org-mode-line-format))

  (unless (or mode-line-format header-line-format (not (one-window-p)))
    (setq header-line-format (default-value 'header-line-format)))
  (when (and mode-line-format
	     (eq header-line-format (default-value 'header-line-format)))
    (setq header-line-format nil))
  (force-mode-line-update 'all))

;; TODO: this doesn't work
(defun header-liner ()
  (let* ((bl (or (buffer-file-name) (buffer-name)))
	 (len (length bl) )
	 (width (window-width)))
    (format (format "%%%dc%%s%%s" (- width len 12)) \? bl ":%3l.%2c [%*%+]")))

(defadvice what-cursor-position (after show-mode-line-temporarily activate compile)
  "Show modeline for a short time."
  (let ((mode-line-format (or mode-line-format org-mode-line-format)))
    (force-mode-line-update)
    (sit-for 1)))

(update-mode-line-format)

(provide 'mode-line-hack)
;;; mode-line-hack.el ends here

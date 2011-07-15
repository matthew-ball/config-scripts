;;; logic4fun.el --- Major mode for "Logic for Fun" - an interface to John Slaney's "Finite Domain Enumerator"

;; Copyright (C) 2011  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: extensions, languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This will add syntax highlighting and inferior process support to files related to FINDER.

;;; Code:

(defvar logic4fun-mode-hook nil)

(defvar logic4fun-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    (define-key map (kbd "C-c s") 'logic4fun-insert-sort)
    (define-key map (kbd "C-c f") 'logic4fun-insert-function)
    (define-key map (kbd "C-c c") 'logic4fun-insert-clause)
    (define-key map (kbd "C-c d") 'execute-diagnose-on-current-buffer)
    (define-key map (kbd "C-c e") 'execute-finder-on-current-buffer)
    map)
  "Keymap for `logic4fun-mode'.")

;; unicode implication (this doesn't work)
(defvar logic4fun-implication-regex "->"
  "A regular expression matching things to convert to the unicode arrow symbols.")

(defvar logic4fun-implication-symbol (string 10230) "The symbol to use for arrows")

(defun logic4fun-fontify (beg end)
  (save-excursion
    (logic4fun-unfontify beg end) ;; mark incorrect uses of spacing
    (goto-char beg)
    (while (re-search-forward logic4fun-implication-regex end t)
      (let ((o (car (overlays-at (match-beginning 0)))))
        (unless (and o (eq (overlay-get o 'type) '->))
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'type '->)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'display logic4fun-implication-symbol-symbol)))))))

(defun logic4fun-unfontify (beg end)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) '->)
              (delete-overlay o)))
        (overlays-in beg end)))


(define-minor-mode logic4fun-implication-mode
  "Indicate where only a single space has been used."
  nil " lambda" nil
  (cond ((not logic4fun-implication-mode)
         (jit-lock-unregister 'logic4fun-fontify)
         (lambda-unfontify (point-min) (point-max)))
        (t (lambda-fontify (point-min) (point-max))
           (jit-lock-register 'logic4fun-fontify))))

;;; old syntax highlighting
(defconst logic4fun-font-lock-keywords-1
  (list
   '("% .*" . font-lock-comment-face)
   '("\\<\\(AND\\|DIF\\|EST\\|IMP\\|M\\(?:AX\\|IN\\)\\|NOT\\|OR\\|PRED\\|SUCC\\|all_different\\|b\\(?:ijective\\|ool\\)\\|c\\(?:lauses\\|ommutative\\|ut\\|ardinality\\)\\|en\\(?:um\\|d\\)\\|functions\\|hidden\\|in\\(?:jective\\|t\\)\\|no_cut\\|one_one\\|partial\\|s\\(?:orts\\|urjective\\|etting\\)\\|total\\)\\>" . font-lock-builtin-face))
  "Minimal highlighting for `logic4fun-mode' mode")

(defconst logic4fun-font-lock-keywords-2
  (append logic4fun-font-lock-keywords-1
	  (list
	   ;; '("\\<\\([A-Za-z0-9_]+\\)\\>:" . font-lock-function-name-face)))
	   '("\\<\\([A-Za-z0-9_]+\\)\\>[:(]" . font-lock-keyword-face)))
  "Additional highlighting for `logic4fun-mode' mode")

(defconst logic4fun-font-lock-keywords-3
  (append logic4fun-font-lock-keywords-2
	  (list
	   '("\\<\\([A-Za-z0-9_]+\\)\\>" . font-lock-variable-name-face)
	   '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)))
  "Maximum highlighting for `logic4fun-mode' mode")

(defvar logic4fun-font-lock-keywords logic4fun-font-lock-keywords-3
  "Default highlighting expressions for `logic4fun-mode' mode")

;;; new syntax highlighting
(defvar logic4fun-keywords
  '("clause" "clauses" "function" "functions" "sort" "sorts" "setting" "end")
  "FINDER keywords.")

(defvar logic4fun-properties
  '("all_different" "injective" "surjective" "bijective" "commutative" "partial" "total" "enum" "hidden" "one_one" "cut")
  "FINDER properties.")

(defvar logic4fun-constants
  '("AND" "OR" "IMP" "EST" "NOT" "DIF" "MIN" "MAX" "PRED" "SUCC" "TRUE" "FALSE")
  "FINDER constants.")

(defvar logic4fun-variables
  '("[A-Za-z0-9_]+")
  "FINDER variables.")

(defvar logic4fun-functions
  ;; '("\\([A-Za-z0-9_]+\\)[:(]" . 0) ;; DOES NOT WORK
  '("[A-Za-z0-9_]+")
  "FINDER functions.")

;; create the regex string for each class of keywords ...
(defvar logic4fun-keywords-regexp (regexp-opt logic4fun-keywords 'words))
(defvar logic4fun-properties-regexp (regexp-opt logic4fun-properties 'words))
(defvar logic4fun-constants-regexp (regexp-opt logic4fun-constants 'words))
(defvar logic4fun-variables-regexp (regexp-opt logic4fun-variables 'words))
(defvar logic4fun-functions-regexp (regexp-opt logic4fun-functions 'words))

;; clear memory (to save memory) ...
(setq logic4fun-keywords nil
      logic4fun-properties nil
      logic4fun-constants nil
      logic4fun-variables nil
      logic4fun-functions nil)

;; note: order above matters ...
(setq logic4fun-font-lock-keywords-new
      `((,logic4fun-properties-regexp . font-lock-type-face)
	(,logic4fun-constants-regexp . font-lock-constant-face)
	(,logic4fun-variables-regexp . font-lock-builtin-face)
	(,logic4fun-functions-regexp . font-lock-function-name-face)
	(,logic4fun-keywords-regexp . font-lock-keyword-face)))

(defvar logic4fun-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st) ;; the underscore is a valid character in a word ...
    (modify-syntax-entry ?% "< b" st) ;; the percentage character begins a comment ...
    (modify-syntax-entry ?\n "> b" st) ;; the newline character ends a comment ...
    st)
  "Syntax table for `logic4fun-mode'.")

(add-to-list 'auto-mode-alist '("\\.l4f\\'" . logic4fun-mode))

;;; indentation
;; TODO: at some point ...

;;;###autoload
(define-derived-mode logic4fun-mode fundamental-mode "logic4fun"
  "A major mode for editing logic4fun (FINDER) source code."
  :syntax-table logic4fun-mode-syntax-table
  (set (make-local-variable 'comment-start) "% ")
  (set (make-local-variable 'comment-start-skip) "%+\\s-*")
  ;; code for syntax highlighting ...
  ;; (set (make-local-variable 'font-lock-defaults) '(logic4fun-font-lock-keywords)) ;; old highlighting
  (set (make-local-variable 'font-lock-defaults) '(logic4fun-font-lock-keywords-new)) ;; new highlighting

  ;; clear memory ...
  (setq logic4fun-keywords-regexp nil
  	logic4fun-properties-regexp nil
  	logic4fun-constants-regexp nil
  	logic4fun-variables-regexp nil
  	logic4fun-functions-regexp nil))

(setq major-mode 'logic4fun-mode
      mode-name "logic4fun")

(run-hooks 'logic4fun-mode-hook)

;; actually do something with the following three skeletons ...
(define-skeleton logic4fun-insert-sort
  "Insert a user defined sort."
  "Type name of sort: "
  str ":")

(define-skeleton logic4fun-insert-function
  "Insert a user defined function."
  "Type name of function: "
  str ":")

(define-skeleton logic4fun-insert-clause
  "Insert a user defined clause."
  "Type clause: "
  str)

;; TODO: fix this ...
(defvar inferior-finder-buffer nil)

;; seems to work as desired ...
(defun execute-finder-on-current-buffer ()
  "Execute FINDER on the current buffer."
  (interactive)
  (save-buffer)
  (progn
    (apply 'make-comint "FINDER" "./finder2007_4/finder" nil '())
    (switch-to-buffer-other-window "*FINDER*")
    (other-window -1))
  (comint-send-string (get-buffer-process "*FINDER*") (buffer-string)))

;; does not work ...
(defun execute-diagnose-on-current-buffer ()
  "Execute FINDER's diagnostic tool on the current buffer."
  (interactive)
  (save-buffer)
  (progn
    (apply 'make-comint "DIAGNOSE" "./finder2007_4/diagnose" (buffer-file-name) '())
    (switch-to-buffer-other-window "*DIAGNOSE*")
    (other-window -1)))

(provide 'logic4fun)
;;; logic4fun.el ends here
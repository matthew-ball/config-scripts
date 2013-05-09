;;; flymake-config.el --- Generate a flymake configuration

;; Copyright (C) 2013  Matthew Ball

;; Author: Matthew Ball <mathew.ball@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ...

;;; Code:

;; TODO: write some sort of `flymake' setup configuration function in elisp
;; TODO: set up a project thingy (keep track of files in project, create makefile based on project)
;; TODO: this is probably of sufficient size to now warrant its own project file

(defvar compiler-list  '() "List of available compilers for `flymake-mode'.")
(defvar compiler-flags '() "List of compiler flags to set.")
(defvar link-flags     '() "List of libraries to link during compilation phase.")

(add-to-list 'compiler-list  '"gcc")       ;; NOTE: C programming
(add-to-list 'compiler-list  '"python")    ;; NOTE: python programming
(add-to-list 'compiler-list  '"ghc")       ;; NOTE: haskell programming
(add-to-list 'compiler-flags '"-Wall")     ;; NOTE: compile with warnings (all)
(add-to-list 'compiler-flags '"-ggdb")     ;; NOTE: compile with debug information
(add-to-list 'link-flags     '"-lpthread") ;; NOTE: link with pthreads library

(defun generate-makefile (projectname &rest junk)
  "..."
  (interactive "sEnter project name: ")
  (let ((cc       (ido-completing-read "Select compiler: " compiler-list))
	(cflags   "-Wall -ggdb")
	(ldflags  "-lpthread"))
    (insert-custom-header-text)
    (add-makefile-compiler-string cc cflags ldflags)
    (add-makefile-project)
    (add-makefile-default-directory-files)
    (add-makefile-suffix-string)
    (add-makefile-clean-string)
    (add-makefile-flymake-string)))

(defun add-makefile-project (&rest junk)
  "..."
  (insert (concat "\nall: " projectname
		  "\n"
		  "\n" projectname ": "
		  "\n"
		  "\n")))

(defun add-makefile-default-directory-files (&rest junk)
  "..."
  (let (files result)
    (setq files (directory-files default-directory t "\.c$" t))
    (dolist (file-name files)
      (when (and (file-readable-p file-name) (not (file-directory-p file-name)))
	(insert (concat "# " file-name "\n"))
	(setq result (cons file-name result))))
    result))

(defun add-makefile-suffix-string (&rest junk)
  "..."
  (insert (concat "\n.SUFFIXES: .c .o"
		  "\n.c.o:"
		  "\n\t$(CC) $(CFLAGS) -c $*.c"
		  "\n")))

(defun add-makefile-clean-string (&rest junk)
  "..."
  (insert (concat "\nclean:"
		  "\n\trm *.o"
		  "\n")))

(defun add-makefile-compiler-string (compiler flags library-flags &rest junk)
  "..."
  (insert (concat "\nCC      = " compiler
		  "\nCFLAGS  = " flags
		  "\nLDFLAGS = " library-flags
		  "\n")))

(defun add-makefile-flymake-string (&rest junk)
  "..."
  (insert (concat "\n # flymake-mode"
		  "\ncheck-syntax:"
		  "\n\t" "$(CC) -o nul -S ${CHK_SOURCES}"
		  "\n")))

;; TODO: move this to the `generel-programming-hook' function
;; (add-hook 'find-file-hook 'flymake-find-file-hook) ;; NOTE: start `flymake' when a new file is opened

(provide 'flymake-config)
;;; flymake-config.el ends here

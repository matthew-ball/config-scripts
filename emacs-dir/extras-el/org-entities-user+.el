;;; org-entities-user+.el --- Extra unicode symbols for `org-entities-user'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Matthew Ball

;; Author: Matthew Ball <chu@lispux>
;; Keywords: convenience

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

;; This just adds extra symbols.

;;; Code:

(autoload 'org-entities "org-entities" "Enable unicode support for `org-mode'." t)

(unless (fboundp 'org-entities-user)
  (require 'org-entities))

;; IMPORTANT: logic symbols
(add-to-list 'org-entities-user '("top" "\\top" nil nil nil nil "⊤"))
(add-to-list 'org-entities-user '("bot" "\\bot" nil nil nil nil "⊥"))
(add-to-list 'org-entities-user '("derives" "\\vdash" nil nil nil nil "⊢"))
(add-to-list 'org-entities-user '("notderives" "\\not\\vdash" nil nil nil nil "⊬"))
(add-to-list 'org-entities-user '("models" "\\models" nil nil nil nil "⊨"))
(add-to-list 'org-entities-user '("notmodels" "\\not\\models" nil nil nil nil "⊭"))
(add-to-list 'org-entities-user '("forces" "\\Vdash" nil nil nil nil "⊩"))
(add-to-list 'org-entities-user '("notforces" "\\not\\Vdash" nil nil nil nil "⊮"))
(add-to-list 'org-entities-user '("boxconditional" "\\boxconditional" nil nil nil nil "□→"))
(add-to-list 'org-entities-user '("box" "\\Box" nil nil nil nil "□"))
(add-to-list 'org-entities-user '("diamond" "\\Diamond" nil nil nil nil "◇"))
(add-to-list 'org-entities-user '("ldots" "\\ldots" nil nil nil nil "…"))
;; IMPORTANT: mathematics symbols
(add-to-list 'org-entities-user '("reals" "\\mathbb{R}" nil nil nil nil "ℝ"))
(add-to-list 'org-entities-user '("integers" "\\mathbb{Z}" nil nil nil nil "ℤ"))
(add-to-list 'org-entities-user '("primes" "\\mathbb{P}" nil nil nil nil "ℙ"))
(add-to-list 'org-entities-user '("naturals" "\\mathbb{N}" nil nil nil nil "ℕ"))
(add-to-list 'org-entities-user '("irrationals" "\\mathbb{I}" nil nil nil nil "𝕀"))
(add-to-list 'org-entities-user '("rationals" "\\mathbb{Q}" nil nil nil nil "ℚ"))
(add-to-list 'org-entities-user '("complex" "\\mathbb{C}" nil nil nil nil "ℂ"))
;; IMPORTANT: misc
(add-to-list 'org-entities-user '("mid" "\\mid" t nil nil nil "|"))
;; IMPORTANT: phonetic symbols
;; TODO: investigate the \textipa{} environments as possible LaTeX exports (as done with \eng and \esh)
;; SOURCE: `http://www.phon.ucl.ac.uk/home/wells/ipa-unicode.htm'
;; SOURCE: `ftp://ftp.tex.ac.uk/ctan/ctan/tex-archive/bibliography/biber/documentation/utf8-macro-map.html'
;; SOURCE: `http://en.wikibooks.org/wiki/LaTeX/Linguistics#IPA_characters'
(add-to-list 'org-entities-user '("eng" "\\textipa{N}" nil nil nil nil "ŋ"))
(add-to-list 'org-entities-user '("esh" "\\textipa{S}" nil nil nil nil "ʃ"))
(add-to-list 'org-entities-user '("thy" "\\eth" nil nil nil nil "ð"))
(add-to-list 'org-entities-user '("thi" "\\theta" nil nil nil nil "θ"))
(add-to-list 'org-entities-user '("darkl" "\\textltilde" nil nil nil nil "ɫ"))
(add-to-list 'org-entities-user '("schwa" "\\textipa{@}" nil nil nil nil "ə"))
(add-to-list 'org-entities-user '("dotlessj" "\\textbardotlessj" nil nil nil nil "ɟ"))
(add-to-list 'org-entities-user '("curvedt" "\\textsubarch{t}" nil nil nil nil "ʈ"))
(add-to-list 'org-entities-user '("retracteddiacritic" "\\b{n}" nil nil nil nil "n̠"))
;;(add-to-list 'org-entities-user '("alveolarapproximate" "\\textipa{\*r}" nil nil nil nil "ɹ"))
(add-to-list 'org-entities-user '("alveolarapproximate" "\\textturnr" nil nil nil nil "ɹ"))
(add-to-list 'org-entities-user '("fishhook" "\\textfishhookr" nil nil nil nil "ɾ"))
(add-to-list 'org-entities-user '("palatalfricative" "\\textipa{C}" nil nil nil nil "ç"))
(add-to-list 'org-entities-user '("bilabialclick" "\\textbullseye" nil nil nil nil "ʘ"))
(add-to-list 'org-entities-user '("glottalstop" "" nil nil nil nil "ʔ"))
(add-to-list 'org-entities-user '("alveolarstop" "\\textyogh" nil nil nil nil "ʒ"))
(add-to-list 'org-entities-user '("pharyngealfricative" "" nil nil nil nil "ʕ"))
;;(add-to-list 'org-entities-user '("Eng" "\\textipa{N}" nil nil nil nil "Ŋ"))
;;(add-to-list 'org-entities-user '("Esh" "\\textipa{S}" nil nil nil nil "Ʃ"))

;; ⟨ʋ⟩ ⟨ɑ⟩ ⟨ɣ⟩ ⟨ɛ⟩ ⟨ɸ⟩ ⟨ʋ⟩ ⟨β⟩ ⟨θ⟩ ⟨χ⟩

;; \mathbb{R}		\mathbf{R}		\mathcal{R}		\mathfrak{R}
;; \mathbb{Z}		\mathbf{Z}		\mathcal{Z}		\mathfrak{Z}
;; \mathbb{Q}		\mathbf{Q}		\mathcal{Q}		\mathfrak{Q}

;; TODO: add customizations for \mathcal{}'s

(provide 'org-entities-user+)
;;; org-entities-user+.el ends here

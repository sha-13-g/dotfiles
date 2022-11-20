;;; gbl-spell.el --- Spelling-related extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my spelling-related extensions, for use in my Emacs
;; setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup gbl-spell ()
  "Extensions for ispell and flyspell."
  :group 'ispell)

(defcustom gbl-spell-dictionaries
  '(("EN English" . "en")
    ("EL Ελληνικά" . "el")
    ("FR Français" . "fr")
    ("ES Espanõl" . "es"))
  "Alist of strings with descriptions and dictionary keys.
Used by `gbl-spell-change-dictionary'."
  :type 'alist
  :group 'gbl-spell)

(defvar gbl-spell--dictionary-hist '()
  "Input history for `gbl-spell-change-dictionary'.")

(defun gbl-spell--dictionary-prompt ()
  "Helper prompt to select from `gbl-spell-dictionaries'."
  (let ((def (car gbl-spell--dictionary-hist)))
    (completing-read
     (format "Select dictionary [%s]: " def)
     (mapcar #'car gbl-spell-dictionaries)
     nil t nil 'gbl-spell--dictionary-hist def)))

;;;###autoload
(defun gbl-spell-change-dictionary (dictionary)
  "Select a DICTIONARY from `gbl-spell-dictionaries'."
  (interactive
   (list (gbl-spell--dictionary-prompt)))
  (let* ((key (cdr (assoc dictionary gbl-spell-dictionaries)))
         (desc (car (assoc dictionary gbl-spell-dictionaries))))
    (ispell-change-dictionary key)
    (message "Switched dictionary to %s" (propertize desc 'face 'bold))))

(autoload 'flyspell-region "flyspell")
(autoload 'thing-at-point "thingatpt")
(autoload 'ispell-word "ispell")

;;;###autoload
(defun gbl-spell-spell-dwim (beg end)
  "Spellcheck between BEG END, current word, or select dictionary.

Use `flyspell-region' on the active region.  With point over a
word and no active region invoke `ispell-word'.  Else call
`gbl-spell-change-dictionary'."
  (interactive "r")
  (cond
   ((use-region-p)
    (flyspell-region beg end))
   ((thing-at-point 'word)
    (call-interactively 'ispell-word))
   (t
    (call-interactively 'gbl-spell-change-dictionary))))

(provide 'gbl-spell)
;;; gbl-spell.el ends here

;;; gbl-recentf.el --- Extensions to recentf.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
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
;; Extensions to `recentf.el' for my Emacs configuration:
;; <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'recentf)
(require 'gbl-common)

;;;###autoload
(defun gbl-recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'.
Add this function to `recentf-keep'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(defvar gbl-recentf--history-files '()
  "Minibuffer history for gbl-recentf files.")

(defvar gbl-recentf--history-dirs '()
  "Minibuffer history for gbl-recentf directories.")

(defun gbl-recentf--files ()
  "Return completion table with files in `recentf-list'."
  (gbl-common-completion-table
   'file
   (mapcar 'abbreviate-file-name recentf-list)))

(defun gbl-recentf--files-prompt (files)
  "Helper of `gbl-recentf-recent-files' to read FILES."
  (let ((def (car gbl-recentf--history-files)))
    (completing-read
     (format "Recentf [%s]: " def)
     files nil t nil 'gbl-recentf--history-files def)))

;;;###autoload
(defun gbl-recentf-recent-files (file)
  "Select FILE from `recentf-list' using completion."
  (interactive
   (list (gbl-recentf--files-prompt (gbl-recentf--files))))
  (find-file file)
  (add-to-history 'gbl-recentf--history-files file))

(defun gbl-recentf--dirs ()
  "Return completion table with directories in `recentf-list'."
  (let ((list (mapcar 'abbreviate-file-name recentf-list)))
    (gbl-common-completion-table
     'file
     (delete-dups
      (mapcar (lambda (file)
                (if (file-directory-p file)
                    (directory-file-name file)
                  (substring (file-name-directory file) 0 -1)))
              list)))))

(defun gbl-recentf--dirs-prompt (dirs)
  "Helper of `gbl-recentf-recent-dirs' to read DIRS."
  (let ((def (car gbl-recentf--history-dirs)))
    (completing-read
     (format "Recent dir [%s]: " def)
     dirs nil t nil 'gbl-recentf--history-dirs def)))

;;;###autoload
(defun gbl-recentf-recent-dirs (dir)
  "Select DIR from `recentf-list' using completion."
  (interactive
   (list (gbl-recentf--dirs-prompt (gbl-recentf--dirs))))
  (find-file dir)
  (add-to-history 'gbl-recentf--history-dirs dir))

;;;###autoload
(defun gbl-recentf-recent-files-or-dirs (&optional arg)
  "Select recent file or, with ARG, recent directory."
  (interactive "P")
  (if arg
      (call-interactively 'gbl-recentf-recent-dirs)
    (call-interactively 'gbl-recentf-recent-files)))

(provide 'gbl-recentf)
;;; gbl-recentf.el ends here

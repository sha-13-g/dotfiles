;;; gbl-elfeed.el --- Elfeed extensions for my dotemacs -*- lexical-binding: t -*-

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

;; NOTE 2022-06-08: This is old code.  There are things I would like to
;; improve.

;;
;; Extensions for Elfeed, intended for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'elfeed nil t)
(require 'url-util)
(require 'gbl-common)

(defgroup gbl-elfeed ()
  "Personal extensions for Elfeed."
  :group 'elfeed)

(defcustom gbl-elfeed-feeds-file
  (thread-last user-emacs-directory (expand-file-name "feeds.el.gpg"))
  "Path to file with `elfeed-feeds'."
  :type 'string
  :group 'gbl-elfeed)

(defcustom gbl-elfeed-archives-directory "~/Documents/feeds/"
  "Path to directory for storing Elfeed entries."
  :type 'string
  :group 'gbl-elfeed)

(defcustom gbl-elfeed-tag-faces nil
  "Add faces for certain tags.
The tags are: critical, important, personal."
  :type 'boolean
  :group 'gbl-elfeed)

(defcustom gbl-elfeed-search-tags '(critical important personal)
  "List of user-defined tags.
Used by `gbl-elfeed-toggle-tag'."
  :type 'list
  :group 'gbl-elfeed)

(defface gbl-elfeed-entry-critical '((t :inherit font-lock-warning-face))
  "Face for Elfeed entries tagged with `critical'.")

(defface gbl-elfeed-entry-important '((t :inherit font-lock-constant-face))
  "Face for Elfeed entries tagged with `important'.")

(defface gbl-elfeed-entry-personal '((t :inherit font-lock-variable-name-face))
  "Face for Elfeed entries tagged with `personal'.")

;;;; Utilities

;;;###autoload
(defun gbl-elfeed-load-feeds ()
  "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
  (let ((feeds gbl-elfeed-feeds-file))
    (if (file-exists-p feeds)
        (load-file feeds)
      (user-error "Missing feeds' file"))))

(defvar elfeed-search-face-alist)

;;;###autoload
(defun gbl-elfeed-fontify-tags ()
  "Expand Elfeed faces if `gbl-elfeed-tag-faces' is non-nil."
  (if gbl-elfeed-tag-faces
      (setq elfeed-search-face-alist
            '((critical gbl-elfeed-entry-critical)
              (important gbl-elfeed-entry-important)
              (personal gbl-elfeed-entry-personal)
              (unread elfeed-search-unread-title-face)))
    (setq elfeed-search-face-alist
          '((unread elfeed-search-unread-title-face)))))

(defvar gbl-elfeed--tag-hist '()
  "History of inputs for `gbl-elfeed-toggle-tag'.")

(defun gbl-elfeed--character-prompt (tags)
  "Helper of `gbl-elfeed-toggle-tag' to read TAGS."
  (let ((def (car gbl-elfeed--tag-hist)))
    (completing-read
     (format "Toggle tag [%s]: " def)
     tags nil t nil 'gbl-elfeed--tag-hist def)))

(defvar elfeed-show-entry)
(declare-function elfeed-tagged-p "elfeed")
(declare-function elfeed-search-toggle-all "elfeed")
(declare-function elfeed-show-tag "elfeed")
(declare-function elfeed-show-untag "elfeed")

;;;###autoload
(defun gbl-elfeed-toggle-tag (tag)
  "Toggle TAG for the current item.

When the region is active in the `elfeed-search-mode' buffer, all
entries encompassed by it are affected.  Otherwise the item at
point is the target.  For `elfeed-show-mode', the current entry
is always the target.

The list of tags is provided by `gbl-elfeed-search-tags'."
  (interactive
   (list
    (intern
     (gbl-elfeed--character-prompt gbl-elfeed-search-tags))))
  (if (derived-mode-p 'elfeed-show-mode)
      (if (elfeed-tagged-p tag elfeed-show-entry)
          (elfeed-show-untag tag)
        (elfeed-show-tag tag))
    (elfeed-search-toggle-all tag)))

(defvar elfeed-show-truncate-long-urls)
(declare-function elfeed-entry-title "elfeed")
(declare-function elfeed-show-refresh "elfeed")

;;;; General commands

(defvar elfeed-search-filter-active)
(defvar elfeed-search-filter)
(declare-function elfeed-db-get-all-tags "elfeed")
(declare-function elfeed-search-update "elfeed")
(declare-function elfeed-search-clear-filter "elfeed")

(defun gbl-elfeed--format-tags (tags sign)
  "Prefix SIGN to each tag in TAGS."
  (mapcar (lambda (tag)
            (format "%s%s" sign tag))
          tags))

;;;###autoload
(defun gbl-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (gbl-elfeed--format-tags db-tags "+"))
           (minus-tags (gbl-elfeed--format-tags db-tags "-"))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags #'gbl-common-crm-exclude-selected-p t))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force)))

(provide 'gbl-elfeed)
;;; gbl-elfeed.el ends here

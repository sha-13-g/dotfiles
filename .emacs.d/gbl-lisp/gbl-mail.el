;;; gbl-mail.el --- Mail tweaks for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my email tweaks, for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'gbl-common)

(defgroup gbl-mail ()
  "Extensions for mail."
  :group 'mail)

(defcustom gbl-mail-maildir-path-regexp "~/.mail/*/Inbox/new/"
  "Path passed to 'find' for checking new mail in maildir.
As this is passed to a shell command, one can use glob patterns.

The user must ensure that this path or regexp matches the one
specified in the mail syncing program (e.g. mbsync)."
  :type 'string
  :group 'gbl-mail)

(defcustom gbl-mail-mode-line-indicator-commands '(notmuch-refresh-this-buffer)
  "List of commands that will be advised to update the mode line.
The advice is designed to run a hook which is used internally by
the function `gbl-mail-mail-indicator'."
  :type 'list
  :group 'gbl-mail)

;;;; Helper functions

(autoload 'auth-source-search "auth-source")

;;;###autoload
(defun gbl-mail-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let ((source (auth-source-search :host host)))
    (if (eq prop :secret)
       (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

(defvar ebdb-db-list)
(autoload 'ebdb-load "ebdb")

(when (require 'ebdb nil t)
  (defun gbl-mail-ebdb-message-setup ()
    "Load EBDB if not done already.
Meant to be assigned to a hook, such as `message-setup-hook'."
    (unless ebdb-db-list
      (ebdb-load))))

;;;; Mode line indicator

;; NOTE 2021-05-14: The following is a more generic approach that uses
;; find to search for new mail.  In my gbl-notmuch.el I define an
;; alternative that checks for the "unread" tag, which works better for
;; my particular setup (refer to my gbl-emacs.org for the relevant
;; commentary).

(defface gbl-mail-mail-count '((t :inherit bold))
  "Face for mode line indicator that shows a new mail count.")

(defvar gbl-mail-new-mail-string nil
  "New maildir count number for the mode line.")

(defun gbl-mail--new-mail ()
  "Search for new mail in personal maildir paths."
  (with-temp-buffer
    (shell-command
     (format "find %s -type f | wc -l" gbl-mail-maildir-path-regexp) t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun gbl-mail--mode-string (count)
  "Add properties to COUNT string."
  (when (not (string= count "0"))
    (propertize (format "@%s " count)
                'face 'gbl-mail-mail-count
                'help-echo "Number of new items in maildirs")))

(defvar gbl-mail--mode-line-mail-indicator nil
  "Internal variable used to store the state of new mails.")

(defun gbl-mail--mode-line-mail-indicator ()
  "Prepare new mail count mode line indicator."
  (let* ((count (gbl-mail--new-mail))
         (indicator (gbl-mail--mode-string count))
         (old-indicator gbl-mail--mode-line-mail-indicator))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     ((>= (string-to-number count) 1)
      (setq global-mode-string (push indicator global-mode-string))
      (setq gbl-mail--mode-line-mail-indicator indicator))
     (t
      (setq gbl-mail--mode-line-mail-indicator nil)))))

(defvar gbl-mail--mode-line-mail-sync-hook nil
  "Hook to refresh the mode line for the mail indicator.")

(defun gbl-mail--add-hook (&rest _)
  "Run `gbl-mail--mode-line-mail-sync-hook'.
Meant to be used as advice after specified commands that should
update the mode line indicator with the new mail count."
  (run-hooks 'gbl-mail--mode-line-mail-sync-hook))

;;;###autoload
(define-minor-mode gbl-mail-mail-indicator
  "Enable mode line indicator with counter for new mail."
  :init-value nil
  :global t
  (if gbl-mail-mail-indicator
      (progn
        (run-at-time t 60 #'gbl-mail--mode-line-mail-indicator)
        (when gbl-mail-mode-line-indicator-commands
          (dolist (fn gbl-mail-mode-line-indicator-commands)
            (advice-add fn :after #'gbl-mail--add-hook)))
        (add-hook 'gbl-mail--mode-line-mail-sync-hook #'gbl-mail--mode-line-mail-indicator)
        (force-mode-line-update t))
    (cancel-function-timers #'gbl-mail--mode-line-mail-indicator)
    (setq global-mode-string (delete gbl-mail--mode-line-mail-indicator global-mode-string))
    (remove-hook 'gbl-mail--mode-line-mail-sync-hook #'gbl-mail--mode-line-mail-indicator)
    (when gbl-mail-mode-line-indicator-commands
      (dolist (fn gbl-mail-mode-line-indicator-commands)
        (advice-remove fn #'gbl-mail--add-hook)))
    (force-mode-line-update t)))

(provide 'gbl-mail)
;;; gbl-mail.el ends here

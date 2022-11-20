;;; gbl-search.el --- Extensions to isearch, replace, grep for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; This covers my isearch.el, replace.el, and grep.el extensions, for
;; use in my Emacs setup: <https://protesilaos.com/emacs/dotemacs>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'isearch)
(require 'replace)
(require 'grep)
(require 'gbl-common)

(defgroup gbl-search ()
  "Setup for Isearch, Occur, and related."
  :group 'search)

;; NOTE 2021-09-16: Based on my git config for headings in diffs.  Read:
;; <https://protesilaos.com/codelog/2021-01-26-git-diff-hunk-elisp-org/>.
(defcustom gbl-search-outline-regexp-alist
  '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
    (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)"))
  "Alist of regular expressions per major mode.

For best results the key must be a symbol that corresponds to a
major mode.

To be used by `gbl-search-occur-outline'."
  :type 'alist
  :group 'gbl-search)

(defcustom gbl-search-todo-keywords
  (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
          "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG")
  "Regexp with search to-do keywords."
  :type 'string
  :group 'gbl-search)

;;;; Isearch

;;;###autoload
(defun gbl-search-isearch-other-end ()
  "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
  (interactive)
  (isearch-done)
  (when isearch-other-end
    (goto-char isearch-other-end)))

;;;###autoload
(defun gbl-search-isearch-abort-dwim ()
  "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
  (interactive)
  (if (eq (length isearch-string) 0)
      (isearch-cancel)
    (isearch-del-char)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state)))
  (isearch-update))

;;;###autoload
(defun gbl-search-isearch-repeat-forward (&optional arg)
  "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-forward (or arg 1)))

;;;###autoload
(defun gbl-search-isearch-repeat-backward (&optional arg)
  "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and (not isearch-forward) isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward (or arg 1)))

(defmacro gbl-search-isearch-occurrence (name edge &optional doc)
  "Construct function for moving to `isearch' occurrence.
NAME is the name of the function.  EDGE is either the beginning
or the end of the buffer.  Optional DOC is the resulting
function's docstring."
  `(defun ,name (&optional arg)
     ,doc
     (interactive "p")
     (let ((x (or arg 1))
           (command (intern (format "isearch-%s-of-buffer" ,edge))))
       (isearch-forward-symbol-at-point)
       (funcall command x))))

(gbl-search-isearch-occurrence
 gbl-search-isearch-beginning-of-buffer
 "beginning"
 "Run `isearch-beginning-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
beginning of the buffer.")

(gbl-search-isearch-occurrence
 gbl-search-isearch-end-of-buffer
 "end"
 "Run `isearch-end-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
end of the buffer.")

;;;; Replace/Occur

(defvar gbl-search-markup-replacements
  '((elisp-to-org-code "`\\(.*?\\)'" "~\\1~")
    (elisp-to-org-verbatim "`\\(.*?\\)'" "=\\1=")
    (org-to-elisp-quote "[=~]\\(.*?\\)[=~]" "`\\1'")
    (org-to-markdown-code "[=~]\\(.*?\\)[=~]" "`\\1`"))
  "Common markup replacement patterns.")

(defvar gbl-search--replace-markup-history '()
  "Minibuffer history of `gbl-search-replace-markup'.")

(defun gbl-search--replace-markup-prompt ()
  "Prompt for font set (used by `fontaine-set-fonts')."
  (let* ((def (nth 0 gbl-search--replace-markup-history))
         (prompt (if def
                     (format "Replace markup TYPE [%s]: " def)
                   "Replace markup TYPE: ")))
    (intern
     (completing-read
      prompt
      ;; TODO 2022-05-01: maybe older Emacs versions need to explicitly
      ;; map through the car of each list?
      gbl-search-markup-replacements
      nil t nil 'gbl-search--replace-markup-history def))))

(defun gbl-search-replace-markup (type)
  "Perform TYPE of markup replacement.
TYPE is the car of a list in `gbl-search-markup-replacements'.

When used interactively, prompt for completion among the
available types.

When the region is active, only perform replacements within its
boundaries, else start from point to the end of the buffer."
  (interactive (list (gbl-search--replace-markup-prompt)))
  (if-let* ((types gbl-search-markup-replacements)
            ((memq type (mapcar #'car types)))
            (association (alist-get type types))
            (search (nth 0 association))
            (replace (nth 1 association)))
      (if (use-region-p)
          (replace-regexp-in-region search replace (region-beginning) (region-end))
        (while (re-search-forward search nil t)
          (replace-match replace)))
    (user-error "`%s' is not part of `gbl-search-markup-replacements'" type)))

;; TODO: make this work backwardly when given a negative argument
(defun gbl-search-isearch-replace-symbol ()
  "Run `query-replace-regexp' for the symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-query-replace-regexp))

(autoload 'goto-address-mode "goto-addr")

;;;###autoload
(defun gbl-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((buf-name (format "*links in <%s>*" (buffer-name))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 gbl-common-url-regexp "\\&" (list (current-buffer)) buf-name)
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;###autoload
(defun gbl-search-occur-browse-url ()
  "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `gbl-search-occur-urls'."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp gbl-common-url-regexp nil t)
        (push (match-string-no-properties 0) matches)))
    (funcall browse-url-browser-function
             (completing-read "Browse URL: " matches nil t))))

(defvar gbl-search--occur-outline-hist '()
  "Minibuffer history of `gbl-search-occur-outline'.")

(defun gbl-search--occur-outline-prompt ()
  "Helper prompt for `gbl-search-occur-outline'."
  (let* ((alist gbl-search-outline-regexp-alist)
         (key (car (assoc major-mode alist)))
         (default (or key (nth 1 gbl-search--occur-outline-hist))))
    (completing-read
     (format "Outline style [%s]: " default)
     (mapcar #'car alist)
     nil nil nil 'gbl-search--occur-outline-hist default)))

(defvar-local gbl-search--remap-cookie nil
  "Current local value of `gbl-search--remap-match-face'.")

(defface gbl-search-match '((t :inherit default))
  "Face intended to override `match' buffer-locally.")

(defun gbl-search--remap-match-face (buf)
  "Remap `match' to `gbl-search-match' in BUF."
  (with-current-buffer buf
    (setq gbl-search--remap-cookie
          (face-remap-add-relative 'match 'gbl-search-match))))

;;;###autoload
(defun gbl-search-occur-outline (&optional arg)
  "Produce buffer outline from `gbl-search-outline-regexp-alist'.

With optional prefix ARG (\\[universal-argument]), prompt for a
preset among the entries in `gbl-search-outline-regexp-alist'.

ARG may also be a string (or regular expression) when called from
Lisp."
  (interactive "P")
  (let* ((regexp (when (and arg (not (stringp arg)))
                   (gbl-search--occur-outline-prompt)))
         (rx (cond
              ((stringp arg)
               arg)
              ((and arg (string= major-mode regexp))
               (alist-get regexp gbl-search-outline-regexp-alist))
              ((assoc major-mode gbl-search-outline-regexp-alist)
               (alist-get major-mode gbl-search-outline-regexp-alist))
              (t (user-error "Unknown outline style"))))
         (buf-name (format "*outline of <%s>*" (buffer-name))))
    (occur-1 rx nil (list (current-buffer)) buf-name)
    ;; Because we are producing an outline, we do not need to know what
    ;; the exact matches are.
    (gbl-search--remap-match-face buf-name)
    (add-to-history 'gbl-search--occur-outline-hist regexp)))

;;;###autoload
(defun gbl-search-occur-todo-keywords (&optional context)
  "Produce Occur buffer with `gbl-search-todo-keywords'.
With optional numeric prefix argument for CONTEXT, show as many
lines before and after each match.

When called from Lisp CONTEXT must satisfy `natnump'.  A faulty
value is read as 0.

Also see `gbl-search-grep-todo-keywords'."
  (interactive "P")
  (let* ((case-fold-search nil)
         (num (cond
               (current-prefix-arg
	            (prefix-numeric-value current-prefix-arg))
               (t (if (natnump context) context 0))))
         (buf-name (format "*keywords in <%s>*" (buffer-name))))
    (occur-1 gbl-search-todo-keywords num (list (current-buffer)) buf-name)))

;;;; Grep

(defvar gbl-search--grep-hist '()
  "Input history of grep searches.")

;;;###autoload
(defun gbl-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.

Search in the current directory using `lgrep'.  With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting from the current directory with `rgrep'."
  (interactive
   (list
    (read-from-minibuffer (concat (if current-prefix-arg
                                      (propertize "Recursive" 'face 'warning)
                                    "Local")
                                  " grep for PATTERN: ")
                          nil nil nil 'gbl-search--grep-hist)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
    (lgrep regexp "*" default-directory)
    (add-to-history 'gbl-search--grep-hist regexp)))

;;;###autoload
(defun gbl-search-grep-todo-keywords (&optional arg)
  "Use `gbl-search-grep' to find `gbl-search-todo-keywords'.

With optional prefix ARG use git-grep instead for the entire
repository (runs `gbl-search-git-grep-todo-keywords').  If Git
is not available on the system, run `gbl-search-grep'
recursively, starting from the current directory.

Also see `gbl-search-occur-todo-keywords'."
  (interactive "P")
  (cond
   (arg
    (if (executable-find "git")
        (gbl-search-git-grep-todo-keywords)
      (gbl-search-grep gbl-search-todo-keywords t)))
   (t
    (gbl-search-grep gbl-search-todo-keywords))))

;; NOTE 2022-01-30: We could use `project-find-regexp' but I prefer
;; grep's editable buffers.  Besides, where is the fun in that when we
;; can use `compilation-start' instead?
;;;###autoload
(defun gbl-search-git-grep-todo-keywords ()
  "Use the git-grep mechanism for `gbl-search-todo-keywords'."
  (interactive)
  (let ((regexp gbl-search-todo-keywords)
        (default-directory (or (vc-root-dir)
                               (locate-dominating-file "." ".git")
                               default-directory)))
    (compilation-start
     (format "git --no-pager grep -n --color=auto -r -I -E -e %s" regexp)
     'grep-mode
     (lambda (mode) (format "*gbl-search-git-%s for '%s'" mode regexp))
     t)))

(provide 'gbl-search)
;;; gbl-search.el ends here

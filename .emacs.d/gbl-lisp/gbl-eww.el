;;; gbl-eww.el --- Extensions for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Protesilaos Stavrou, Abhiseck Paira

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;;         Abhiseck Paira <abhiseckpaira@disroot.org>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; Extensions for the eww, intended for my Emacs setup:
;; <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.
;;
;; Thanks to Abhiseck Paira for the patches (see commit log for this
;; file, such as with C-x v l (vc-print-log)).  Some of those improved
;; on various aspects of the EWW-specific functionality, while others
;; provide the layer of integration with Elpher.  Abhiseck's online
;; presence:
;;
;; 1. <https://social.linux.pizza/@redstarfish>
;; 2. <gemini://redstarfish.flounder.online/>

;;; Code:

(require 'shr)
(require 'eww)
(require 'elpher nil t)
(require 'url-parse)
(require 'gbl-common)

(defgroup gbl-eww ()
  "Tweaks for EWW."
  :group 'eww)

;;;; Basic setup

;; TODO 2021-10-15: Deprecate this in favour of what we added to Emacs29.
;; <https://protesilaos.com/codelog/2021-10-15-emacs-29-eww-rename-buffers/>.

(defun gbl-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(add-hook 'eww-after-render-hook #'gbl-eww--rename-buffer)
(advice-add 'eww-back-url :after #'gbl-eww--rename-buffer)
(advice-add 'eww-forward-url :after #'gbl-eww--rename-buffer)

;;;; History extras

(defvar gbl-eww-visited-history '()
  "History of visited URLs.")

(defcustom gbl-eww-save-history-file
  (locate-user-emacs-file "gbl-eww-visited-history")
  "File to save the value of `gbl-eww-visited-history'."
  :type 'file
  :group 'gbl-eww)

(defcustom gbl-eww-save-visited-history nil
  "Whether to save `gbl-eww-visited-history'.
If non-nil, save the value of `gbl-eww-visited-history' in
`gbl-eww-save-history-file'."
  :type 'boolean
  :group 'gbl-eww)

(defcustom gbl-eww-list-history-buffer "*gbl-eww-history*"
  "Name of buffer for `gbl-eww-list-history'."
  :type 'string
  :group 'gbl-eww)

;; These history related functions are adapted from eww.
(defun gbl-eww--save-visited-history ()
  "Save the value of `gbl-eww-visited-history' in a file.
The file is determined by the variable `gbl-eww-save-history-file'."
  (when gbl-eww-save-visited-history
    (with-temp-file gbl-eww-save-history-file
      (insert (concat ";; Auto-generated file;"
                      " don't edit -*- mode: lisp-data -*-\n"))
      (pp gbl-eww-visited-history (current-buffer)))))

(defun gbl-eww--read-visited-history (&optional error-out)
  "Read history from `gbl-eww-save-history-file'.
If ERROR-OUT, signal `user-error' if there is no history."
  (when gbl-eww-save-visited-history
    (let ((file gbl-eww-save-history-file))
      (setq gbl-eww-visited-history
            (unless (zerop
                     (or (file-attribute-size (file-attributes file))
                         0))
              (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))
      (when (and error-out (not gbl-eww-visited-history))
        (user-error "No history is defined")))))

(unless gbl-eww-visited-history
  (gbl-eww--read-visited-history t))

(defun gbl-eww--history-prepare ()
  "Prepare dedicated buffer for browsing history."
  (set-buffer (get-buffer-create gbl-eww-list-history-buffer))
  (gbl-eww-history-mode)
  (let ((inhibit-read-only t)
        start)
    (erase-buffer)
    (setq-local header-line-format
                "Unified EWW and Elpher Browsing History (gbl-eww)")
    (dolist (history gbl-eww-visited-history)
      (setq start (point))
      (insert (format "%s" history) "\n")
      (put-text-property start (1+ start) 'gbl-eww-history history))
    (goto-char (point-min))))

;;;###autoload
(defun gbl-eww-list-history ()
  "Display `gbl-eww-visited-history' in a dedicated buffer.
This is a replacement for `eww-list-histories' (or equivalent),
as it can combine URLs in the Gopher or Gemini protocols."
  (interactive)
  (when gbl-eww-visited-history
    (gbl-eww--save-visited-history))
  (gbl-eww--read-visited-history t)
  (pop-to-buffer gbl-eww-list-history-buffer)
  (gbl-eww--history-prepare))

(defvar gbl-eww-history-kill-ring nil
  "Store the killed history element.")

(defun gbl-eww-history-kill ()
  "Kill the current history."
  (interactive)
  (let* ((start (line-beginning-position))
         (history (get-text-property start 'gbl-eww-history))
         (inhibit-read-only t))
    (unless history
      (user-error "No history on the current line"))
    (forward-line 1)
    (push (buffer-substring start (point))
          gbl-eww-history-kill-ring)
    (delete-region start (point))
    (setq gbl-eww-visited-history (delq history
                                         gbl-eww-visited-history))
    (gbl-eww--save-visited-history)))

(defun gbl-eww-history-yank ()
  "Yank a previously killed history to the current line."
  (interactive)
  (unless gbl-eww-history-kill-ring
    (user-error "No previously killed history"))
  (beginning-of-line)
  (let ((inhibit-read-only t)
        (start (point))
        history)
    (insert (pop gbl-eww-history-kill-ring))
    (setq history (get-text-property start 'gbl-eww-history))
    (if (= start (point-min))
        (push history gbl-eww-visited-history)
      (let ((line (count-lines start (point))))
        (setcdr (nthcdr (1- line) gbl-eww-visited-history)
                (cons history (nthcdr line
                                      gbl-eww-visited-history)))))
    (gbl-eww--save-visited-history)))

(defun gbl-eww-history-browse ()
  "Browse the history under point."
  (interactive)
  (let ((history (get-text-property (line-beginning-position)
                                     'gbl-eww-history)))
    (unless history
      (user-error "No history on the current line"))
    (quit-window)
    (gbl-eww history)))

(defvar gbl-eww-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") 'gbl-eww-history-kill)
    (define-key map (kbd "C-y") 'gbl-eww-history-yank)
    (define-key map (kbd "<RET>") 'gbl-eww-history-browse)

    (easy-menu-define nil map
      "Menu for `gbl-eww-history-mode-map'."
      '("gbl-eww history"
        ["Exit" quit-window t]
        ["Browse" gbl-eww-history-browse
         :active (get-text-property (line-beginning-position)
                                    'gbl-eww-history)]
        ["Kill" gbl-eww-history-kill
         :active (get-text-property (line-beginning-position)
                                    'gbl-eww-history)]
        ["Yank" gbl-eww-history-yank
         :active gbl-eww-history-kill-ring]))
    map))

(define-derived-mode gbl-eww-history-mode
  special-mode
  "gbl-eww-history"
  "Mode for listing history.

\\{gbl-eww-history-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

(defun gbl-eww--record-history ()
  "Store URL in `gbl-eww-visited-history'.
To be used by `eww-after-render-hook'."
  (let ((url (plist-get eww-data :url)))
    (add-to-history 'gbl-eww-visited-history url)))

(autoload 'elpher-page-address "elpher")
(autoload 'elpher-address-to-url "elpher")
(defvar elpher-current-page)

(defun gbl-eww--record-elpher-history (arg1 &optional arg2 arg3)
  "Store URLs visited using elpher in `gbl-eww-visited-history'.
To be used by `elpher-visited-page'.  ARG1, ARG2, ARG3 are
unused."
  (let* ((address (elpher-page-address elpher-current-page))
         (url (elpher-address-to-url address)))
    ;; elpher-address-to-url checks for special pages.
    (when url
      (add-to-list 'gbl-eww-visited-history url))))

(add-hook 'eww-after-render-hook #'gbl-eww--record-history)
(advice-add 'eww-back-url :after #'gbl-eww--record-history)
(advice-add 'eww-forward-url :after #'gbl-eww--record-history)
(advice-add 'elpher-visit-page :after #'gbl-eww--record-elpher-history)
;; Is there a better function to add this advice?

;;;; Commands

;; handler that browse-url calls.

(defun gbl-eww--get-current-url ()
  "Return the current-page's URL."
  (cond ((eq major-mode 'elpher-mode)
         (elpher-address-to-url
          (elpher-page-address elpher-current-page)))
        ((eq major-mode 'eww-mode)
         (plist-get eww-data :url))
        ;; (t (user-error "Not a eww or elpher buffer"))
        ))

;; This is almost identical to browse-url-interactive-arg except it
;; calls thing-at-point-url-at-point instead of
;; browse-url-url-at-point[1]. The problem with [1] is that it cancats
;; "http" anything it finds, which is a problem for gemini, gopher
;; etc.  urls. I hope there's something similar or better way to do
;; it, we don't have to use this one.
(defun gbl-eww--interactive-arg (prompt)
  "Read a URL from the minibuffer, prompting with PROMPT.
If Transient-mark-mode is non-nil and the mark is active, it
defaults to the current region, else to the URL at or before
point.  If invoked with a mouse button, it moves point to the
position clicked before acting.

Return URL for use in a interactive."
  (let ((event (elt (this-command-keys) 0)))
    (and (listp event) (mouse-set-point event)))
  (read-string prompt
               (or (and transient-mark-mode mark-active
                        ;; rfc2396 Appendix E.
                        (replace-regexp-in-string
                         "[\t\r\f\n ]+" ""
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))))
                   (thing-at-point-url-at-point t))))

(declare-function elpher-go "elpher")

;;;###autoload
(defun gbl-eww (url &optional arg)
  "Pass URL to appropriate client.
With optional ARG, use a new buffer."
  (interactive
   (list (gbl-eww--interactive-arg "URL: ")
         current-prefix-arg))
  (let ((url-parsed (url-generic-parse-url url)))
    (pcase (url-type url-parsed)
      ((or "gemini" "gopher" "gophers" "finger")
       (elpher-go url))
      (_ (eww url arg)))))

;;;###autoload
(defun gbl-eww-browse-dwim (url &optional arg)
  "Visit a URL, maybe from `eww-prompt-history', with completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new eww buffer.  If URL does not look like a valid link, run a
web query using `eww-search-prefix'.

When called from an eww buffer, provide the current link as
\\<minibuffer-local-map>\\[next-history-element]."
  (interactive
   (let ((all-history (delete-dups
                       (append gbl-eww-visited-history
                               eww-prompt-history)))
         (current-url (gbl-eww--get-current-url)))
     (list
      (completing-read "Run EWW on: " all-history
                       nil nil current-url 'eww-prompt-history current-url)
      (prefix-numeric-value current-prefix-arg))))
  (gbl-eww url arg))

;; NOTE 2021-09-08: This uses the EWW-specific bookmarks, NOT those of
;; bookmark.el.  Further below I provide integration with the latter,
;; meaning that we must either make this obsolete or make it work with
;; the new system.
;;;###autoload
(defun gbl-eww-visit-bookmark (&optional arg)
  "Visit bookmarked URL.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (eww-read-bookmarks)
  (let ((list (gensym)))
    (dolist (bookmark eww-bookmarks)
      (push (plist-get bookmark :url) list))
    (if eww-bookmarks
        (eww (completing-read "Visit EWW bookmark: " list)
             (when arg 4))
      (user-error "No bookmarks"))))

(defun gbl-eww--capture-url-on-page (&optional position)
  "Capture all the links on the current web page.

Return a list of strings.  Strings are in the form LABEL @ URL.
When optional argument POSITION is non-nil, include position info
in the strings too, so strings take the form
LABEL @ URL ~ POSITION."
  (let (links match)
    (save-excursion
      (goto-char (point-max))
      ;; NOTE 2021-07-25: The first clause in the `or' is meant to
      ;; address a bug where if a URL is in `point-min' it does not get
      ;; captured.
      (while (setq match (text-property-search-backward 'shr-url))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (replace-regexp-in-string "\n" " " ; NOTE 2021-07-25: newlines break completion
                                                (buffer-substring-no-properties
                                                 start-point-prop end-point-prop)))
               (point start-point-prop)
               (line (line-number-at-pos point t))
               (column (save-excursion (goto-char point) (current-column)))
               (coordinates (propertize
                             (format "%d,%d (%d)" line column point)
                             'face 'shadow)))
          (when url
            (if position
                (push (format "%-15s ~ %s  @ %s"
                              coordinates label url)
                      links)
              (push (format "%s  @ %s"
                            label url)
                    links))))))
    links))

(defmacro gbl-eww-act-visible-window (&rest body)
  "Run BODY within narrowed-region.
If region is active run BODY within active region instead.
Return the value of the last form of BODY."
  `(save-restriction
     (if (use-region-p)
         (narrow-to-region (region-beginning) (region-end))
       (narrow-to-region (window-start) (window-end)))
     ,@body))

;;;###autoload
(defun gbl-eww-visit-url-on-page (&optional arg)
  "Visit URL from list of links on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links (gbl-eww--capture-url-on-page))
           (selection (completing-read "Browse URL from page: " links nil t))
           (url (replace-regexp-in-string ".*@ " "" selection)))
      (eww url (when arg 4)))))

;;;###autoload
(defun gbl-eww-jump-to-url-on-page (&optional arg)
  "Jump to URL position on the page using completion.

When called without ARG (\\[universal-argument]) get URLs only
from the visible portion of the buffer.  But when ARG is provided
consider whole buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links
            (if arg
                (gbl-eww--capture-url-on-page t)
              (gbl-eww-act-visible-window
               (gbl-eww--capture-url-on-page t))))
           (prompt-scope (if arg
                             (propertize "URL on the page" 'face 'warning)
                           "visible URL"))
           (prompt (format "Jump to %s: " prompt-scope))
           (selection (completing-read prompt links nil t))
           (position (replace-regexp-in-string "^.*(\\([0-9]+\\))[\s\t]+~" "\\1" selection))
           (point (string-to-number position)))
      (goto-char point))))

(defvar gbl-eww--occur-feed-regexp
  (concat "\\(rss\\|atom\\)\\+xml.\\(.\\|\n\\)"
          ".*href=[\"']\\(.*?\\)[\"']")
  "Regular expression to match web feeds in HTML source.")

;;;###autoload
(defun gbl-eww-find-feed ()
  "Produce bespoke buffer with RSS/Atom links from XML source."
  (interactive)
  (let* ((url (or (plist-get eww-data :start)
                  (plist-get eww-data :contents)
                  (plist-get eww-data :home)
                  (plist-get eww-data :url)))
         (title (or (plist-get eww-data :title) url))
         (source (plist-get eww-data :source))
         (buf-name (format "*feeds: %s # eww*" title)))
    (with-temp-buffer
      (insert source)
      (occur-1 gbl-eww--occur-feed-regexp "\\3" (list (current-buffer)) buf-name))
    ;; Handle relative URLs, so that we get an absolute URL out of them.
    ;; Findings like "rss.xml" are not particularly helpful.
    ;;
    ;; NOTE 2021-03-31: the base-url heuristic may not always be
    ;; correct, though it has worked in all cases I have tested it on.
    (when (get-buffer buf-name)
      (with-current-buffer (get-buffer buf-name)
        (let ((inhibit-read-only t)
              (base-url (replace-regexp-in-string "\\(.*/\\)[^/]+\\'" "\\1" url)))
          (goto-char (point-min))
          (unless (re-search-forward gbl-common-url-regexp nil t)
            (re-search-forward ".*")
            (replace-match (concat base-url "\\&"))))))))

;;TODO: Add this variable as user-option, that is, define it with
;;`defcustom' so that users can use the customization interface to
;;modify it.

(defvar gbl-eww-search-engines
  '((debbugs . (debbugs
                "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
                hist-var gbl-eww--debbugs-hist))
    (wikipedia . (wikipedia
                  "https://en.m.wikipedia.org/w/index.php?search="
                  hist-var gbl-eww--wikipedia-hist))
    (archwiki . (archwiki
                 "https://wiki.archlinux.org/index.php?search="
                 hist-var gbl-eww--archwiki-hist))
    (aur . (aur "https://aur.archlinux.org/packages/?K="
                hist-var gbl-eww--aur-hist)))
  "Alist of Plist of web search engines related data.
From now on refer to this type of data as APLIST.  Each element
of APLIST is (KEY . VALUE) pair.  KEY is a symbol specifying
search engine name.  The VALUE is property list.

The plist has two key-value pairs.  K1 is the same symbol has KEY
and V1 is search string of the search engine.

K2 is the symbol 'hist-var', V2 is also a symbol that has a format
'gbl-eww--K1-hist'.

NOTE: If you modify this variable after `gbl-eww' is loaded you
need to run the following code after modification:

    (gbl-eww--define-hist-var gbl-eww-search-engines)")

;; Below 's-string' is short for 'search-string'. For wikipedia which
;; is this string: "https://en.m.wikipedia.org/w/index.php?search=". I
;; use this name because I don't know it's proper name.

;; Define constructor and selectors functions to access
;; `gbl-eww-search-engines'.
;; the constructor
(defun gbl-eww--cons-search-engines (name s-string)
  "Include a new Alist element.
The alist element is added to variable `gbl-eww-search-engines'.

NAME should be symbol representing the search engine.  S-STRING
should be string, which is specific to named search engine."
  (let ((my-plist `(,name ,s-string))
        (hist-var-name (format "gbl-eww--%s-hist"
                               (symbol-name name))))
    (plist-put my-plist 'hist-var (intern hist-var-name))
    (let ((my-alist (cons name my-plist)))
      (add-to-list 'gbl-eww-search-engines my-alist))))

;; Selectors definitions start
(defun gbl-eww--select-hist-name (aplist engine-name)
  "Get hist-var-name from APLIST of ENGINE-NAME."
  (let ((hist-var-name (plist-get
                        (alist-get engine-name aplist)
                        'hist-var)))
    hist-var-name))

(defun gbl-eww--select-engine-names (aplist)
  "Return a list of search-engine names from APLIST.
Each value of the list is a string."
  (mapcar (lambda (x) (format "%s" (car x)))
          aplist))

(defun gbl-eww--select-s-string (aplist engine-name)
  "Return the search-string for specified ENGINE-NAME from APLIST."
  (plist-get
   (alist-get engine-name aplist)
   engine-name))
;; Selector definitions end here.

(defun gbl-eww--define-hist-var (aplist)
  "Initialize APLIST hist-variables to empty list; return nil."
  (let ((engine-names
         (gbl-eww--select-engine-names aplist)))
    (dolist (engine engine-names)
      (let ((hist-var-name
             (gbl-eww--select-hist-name aplist
                                         (intern engine))))
        (set hist-var-name '())))))

(gbl-eww--define-hist-var gbl-eww-search-engines)

;;;###autoload
(defun gbl-eww-search-engine (engine s-term &optional arg)
  "Search S-TERM using ENGINE.
ENGINE is an assossiation defined in `gbl-eww-search-engines'.

With optional prefix ARG (\\[universal-argument]) open the search
result in a new buffer."
  (interactive
   (let* ((engine-list (gbl-eww--select-engine-names
                        gbl-eww-search-engines))
          (engine-name (completing-read
                        "Search with: " engine-list nil t nil
                        'gbl-eww--engine-hist))
          (history-list (gbl-eww--select-hist-name
                         gbl-eww-search-engines
                         (intern engine-name)))
          (search-term (read-string
                        "Search for: " nil history-list)))
     (list engine-name search-term
           (prefix-numeric-value current-prefix-arg))))
  (let* ((s-string
          (gbl-eww--select-s-string gbl-eww-search-engines
                                     (intern engine)))
         (eww-pass (format "%s%s" s-string s-term))
         (history-list (gbl-eww--select-hist-name
                        gbl-eww-search-engines
                        (intern engine))))
    (add-to-history history-list s-term)
    (eww eww-pass arg)))

;;;###autoload
(defun gbl-eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

;;;###autoload
(defun gbl-eww-readable ()
  "Use more opinionated `eww-readable'.

Set width is set to `current-fill-column'.  Adjust size of
images."
  (interactive)
  (let ((shr-width (current-fill-column))
        (shr-max-image-proportion 0.35))
    (eww-readable)))

;; NOTE 2021-09-08: This uses the EWW-specific bookmarks, NOT those of
;; bookmark.el.  Further below I provide integration with the latter,
;; meaning that we must either make this obsolete or make it work with
;; the new system.
;;;###autoload
(defun gbl-eww-bookmark-page (title)
  "Add eww bookmark named with TITLE."
  (interactive
   (list
    (read-string "Set bookmark title: " (plist-get eww-data :title))))
  (plist-put eww-data :title title)
  (eww-add-bookmark))

(defvar gbl-eww--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

(defun gbl-eww--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string gbl-eww--punctuation-regexp "" str))

(defun gbl-eww--slug-hyphenate (str)
  "Replace spaces with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
   "-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun gbl-eww--sluggify (str)
  "Make STR an appropriate file name slug."
  (downcase (gbl-eww--slug-hyphenate (gbl-eww--slug-no-punct str))))

;;;###autoload
(defun gbl-eww-download-html (name)
  "Download web page and call the file with NAME."
  (interactive
   (list
    (gbl-eww--sluggify
     (read-string "Set downloaded file name: " (plist-get eww-data :title)))))
  (let* ((path (thread-last eww-download-directory
                 (expand-file-name
                  (concat (format-time-string "%Y%m%d_%H%M%S") "--" name ".html"))))
         (out (gbl-common-shell-command-with-exit-code-and-output
               "wget" "-q" (format "%s" (plist-get eww-data :url))
               "-O" (format "%s" (shell-quote-argument path)))))
    (if (= (car out) 0)
        (message "Downloaded page at %s" path)
      (message "Error downloading page: %s" (cdr out)))))

(defun gbl-eww--kill-buffers-when (predicate)
  "Kill buffers when PREDICATE is non-nil.

Loop through the buffer list, calling PREDICATE with each buffer.
When calling PREDICATE with a buffer returns non-nil, kill that
buffer.

PREDICATE must be function that takes buffer-object as the one
and only argument.  It should return nil or non-nil."
  (let ((list-buffers (buffer-list)))
    (dolist (buffer list-buffers)
      (when (funcall predicate buffer)
        (kill-buffer buffer)))))

(defun gbl-eww--kill-eww-buffers-p (buffer)
  "Predicate function.  Return nil or non-nil.

Take BUFFER, make it current, check if it has 'eww-mode' as the
`major-mode' or if its major mode is derived from `special-mode'
and has \"eww\" in the buffer-name. Then return non-nil."
  (let ((case-fold-search t))  ; ignore case
    (with-current-buffer buffer
      (or (eq major-mode 'eww-mode)
          (and (derived-mode-p 'special-mode)
               (string-match "\\*.*eww.*\\*" (buffer-name)))))))

(defun gbl-eww-kill-eww-buffers ()
  "Kill all EWW buffers.
Also kill special buffers made by EWW for example buffers like
\"*eww-bookmarks*\", \"*eww-history*\" etc."
  (gbl-eww--kill-buffers-when 'gbl-eww--kill-eww-buffers-p))

(defcustom gbl-eww-delete-cookies t
  "If non-nil delete cookies when `gbl-eww-quit' is called."
  :type 'boolean
  :group 'gbl-eww)

(defun gbl-eww-delete-cookies ()
  "Delete cookies from the cookie file."
  (when gbl-eww-delete-cookies
    (url-cookie-delete-cookies)))

;; TODO: Make it defcustom
(defvar gbl-eww-quit-hook nil
  "Run this hook when `gbl-eww-quit' is called.")

;; Populate the hook with these functions.
(dolist (func '(gbl-eww-delete-cookies
                gbl-eww-kill-eww-buffers
                gbl-eww--save-visited-history))
  (add-hook 'gbl-eww-quit-hook func))

;;;###autoload
(defun gbl-eww-quit ()
  "Quit eww, kill all its buffers, delete all cookies.
As a final step, save `gbl-eww-visited-history' to a file (see
`gbl-eww-save-history-file')."
  (interactive)
  (if gbl-eww-save-visited-history
      (when (y-or-n-p "Are you sure you want to quit eww? ")
        (run-hooks 'gbl-eww-quit-hook))
    ;;
    ;; Now users have full control what `gbl-eww-quit' does, by
    ;; modifying `gbl-eww-quit-hook'.
    (when (yes-or-no-p "Are you sure you want to quit eww?")
      (run-hooks 'gbl-eww-quit-hook))))

;;;;; Bookmarks with bookmark.el
;; The following is adapted from vc-dir.el.

;; TODO 2021-09-08: Review all legacy bookmark functions defined herein.

(defcustom gbl-eww-bookmark-link nil
  "Control the behaviour of bookmarking inside EWW buffers.

If non-nil bookmark the button at point, else the current page's
URL.  Otherwise only target the current page.

This concerns the standard bookmark.el framework, so it applies
to commands `bookmark-set' and `bookmark-set-no-overwrite'."
  :type 'boolean
  :group 'gbl-eww)

(declare-function bookmark-make-record-default "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(defun gbl-eww--bookmark-make-record ()
  "Return a bookmark record.
If `gbl-eww-bookmark-link' is non-nil and point is on a link button,
return a bookmark record for that link.  Otherwise, return a bookmark
record for the current EWW page."
  (let* ((button (and gbl-eww-bookmark-link
                      (button-at (point))))
         (url (if button
                  (button-get button 'shr-url)
                (plist-get eww-data :url))))
    (unless url
      (error "No link found; cannot bookmark this"))
    (let* ((title (if button
                      url
                    (concat "(EWW) " (plist-get eww-data :title))))
           (pos (if button nil (point)))
           (defaults (delq nil (list title url))))
      `(,title
        ,@(bookmark-make-record-default 'no-file)
        (eww-url . ,url)
        (filename . ,url) ; This is a hack to get Marginalia annotations
        (position . ,pos)
        (handler . gbl-eww-bookmark-jump)
        (defaults . ,defaults)))))

(defun gbl-eww--set-bookmark-handler ()
  "Set appropriate `bookmark-make-record-function'.
Intended for use with `eww-mode-hook'."
  (setq-local bookmark-make-record-function #'gbl-eww--bookmark-make-record))

(add-hook 'eww-mode-hook #'gbl-eww--set-bookmark-handler)

(defun gbl-eww--pop-to-buffer (buffer &rest _args)
  "Set BUFFER and ignore ARGS.
Just a temporary advice to override `pop-to-buffer'."
  (set-buffer buffer))

(declare-function bookmark-get-handler "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-front-context-string "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-rear-context-string "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-position "bookmark" (bookmark-name-or-record))
(declare-function bookmark-name-from-full-record "bookmark" (bookmark-record))
(declare-function bookmark-get-bookmark "bookmark" (bookmark-name-or-record &optional noerror))

;; Copied from the `eww-conf.el' of JSDurand on 2021-09-17 10:19 +0300:
;; <https://git.jsdurand.xyz/emacsd.git/tree/eww-conf.el>.  My previous
;; version would not work properly when trying to open the bookmark in
;; the other window from inside the Bookmarks' list view.

;;;###autoload
(defun gbl-eww-bookmark-jump (bookmark)
  "Jump to BOOKMARK in EWW.
This is intended to be the handler for bookmark records created
by `gbl-eww--bookmark-make-record'.

If there is already a buffer visiting the URL of the bookmark,
simply jump to that buffer and try to restore the point there.
Otherwise, fetch URL and afterwards try to restore the point."
  (let ((handler (bookmark-get-handler bookmark))
        (location (bookmark-prop-get bookmark 'eww-url))
        (front (cons 'front-context-string
                     (bookmark-get-front-context-string bookmark)))
        (rear (cons 'rear-context-string
                    (bookmark-get-rear-context-string bookmark)))
        (position (cons 'position (bookmark-get-position bookmark)))
        (eww-buffers
         (delq
          nil
          (mapcar
           (lambda (buffer)
             (cond
              ((provided-mode-derived-p
                (buffer-local-value
                 'major-mode buffer)
                'eww-mode)
               buffer)))
           (buffer-list))))
        buffer)
    (cond
     ((and (stringp location)
           (not (string= location ""))
           (eq handler #'gbl-eww-bookmark-jump))
      (let (reuse-p)
        (mapc
         (lambda (temp-buffer)
           (cond
            ((string=
              (plist-get
               (buffer-local-value 'eww-data temp-buffer)
               :url)
              location)
             (setq reuse-p temp-buffer)
             (setq buffer temp-buffer))))
         eww-buffers)
        ;; Don't switch to that buffer, otherwise it will cause
        ;; problems if we want to open the bookmark in another window.
        (cond
         (reuse-p
          (set-buffer reuse-p)
          ;; we may use the default handler to restore the position here
          (with-current-buffer reuse-p
            (goto-char (cdr position))
            (cond
             ((search-forward (cdr front) nil t)
              (goto-char (match-beginning 0))))
            (cond
             ((search-forward (cdr rear) nil t)
              (goto-char (match-end 0))))))
         (t
          ;; HACK, GIANT HACK!
          
          (advice-add #'pop-to-buffer :override
                      #'gbl-eww--pop-to-buffer)
          (eww location 4)
          ;; after the `set-buffer' in `eww', the current buffer is
          ;; the buffer we want
          (setq buffer (current-buffer))
          ;; restore the definition of pop-to-buffer...
          (advice-remove
           #'pop-to-buffer #'gbl-eww--pop-to-buffer)
          ;; add a hook to restore the position

          ;; make sure each hook function is unique, so that different
          ;; hooks don't interfere with each other.
          (let ((function-symbol
                 (intern
                  (format
                   "eww-render-hook-%s"
                   (bookmark-name-from-full-record
                    (bookmark-get-bookmark bookmark))))))
            (fset function-symbol
                  (lambda ()
                    (remove-hook
                     'eww-after-render-hook function-symbol)
                    (bookmark-default-handler
                     (list
                      "" (cons 'buffer buffer)
                      front rear position))))
            (add-hook 'eww-after-render-hook function-symbol))))))
     ((user-error "Cannot jump to this bookmark")))))


;;; lynx dump

(defcustom gbl-eww-post-lynx-dump-function nil
  "Function to run on lynx dumped buffer for post-processing.
Function is called with the URL of the page the buffer is
visiting.

Specifying nil turns off this variable, meaning that no
post-processing takes place."
  :group 'gbl-eww
  :type '(choice (const :tag "Unspecified" nil)
                 function))

(defcustom gbl-eww-lynx-dump-dir
  (if (stringp eww-download-directory)
      eww-download-directory
    (funcall eww-download-directory))
  "Directory to save lynx dumped files.
It should be an existing directory or a sexp that evaluates to an
existing directory."
  :group 'gbl-eww
  :type '(choice directory sexp))

(defun gbl-eww--lynx-available-p ()
  "Check if `lynx' is available in PATH."
  (executable-find "lynx"))

(defun gbl-eww--get-text-property-string (prop)
  "Return string that has text property PROP at (point).
The string is from (point) to end of PROP.  If there is no text
property PROP at (point), return nil."
  (let* ((match (text-property-search-forward prop))
         (start-point-prop (prop-match-beginning match))
         (end-point-prop (prop-match-end match)))
    (and
     (<= start-point-prop (point) end-point-prop)
     (replace-regexp-in-string
      "\n" " "
      (buffer-substring-no-properties
       start-point-prop end-point-prop)))))

(defun gbl-eww--current-page-title ()
  "Return title of the Web page EWW buffer is visiting."
  (plist-get eww-data :title))

(defun gbl-eww-lynx-dump (url filename)
  "Run lynx -dump on URL and save output as FILENAME.
When run interactively in a eww buffer visiting a web page, run
lynx dump on the web page's URL.  If point is on a link, then run
lynx dump on that link instead."
  (interactive
   (let* ((default-url (or (get-text-property (point) 'shr-url)
                           (eww-current-url)))
          (dir gbl-eww-lynx-dump-dir)
          (title (or
                  (gbl-eww--get-text-property-string 'shr-url)
                  (gbl-eww--current-page-title)))
          (def-file-name
            (file-name-concat dir
                              (concat (gbl-eww--sluggify title) ".txt"))))
     (list
      (read-string (format "URL [%s]: " default-url) nil nil default-url)
      (read-file-name (format "File Name [%s]: " def-file-name) dir def-file-name))))
  (if (gbl-eww--lynx-available-p)
      (progn
        (access-file gbl-eww-lynx-dump-dir "Non existing directory specified")
        (with-temp-file filename
          (with-temp-message
              (format "Running `lynx --dump %s'" url)
            (let ((coding-system-for-read 'prefer-utf-8))
              (call-process "lynx" nil t nil "-dump" url)))
          (with-temp-message "Processing lynx dumped buffer..."
            (and
             (functionp gbl-eww-post-lynx-dump-function)
             (funcall gbl-eww-post-lynx-dump-function url)))))
    (error "`lynx' executable not found in PATH")))

(provide 'gbl-eww)
;;; gbl-eww.el ends here

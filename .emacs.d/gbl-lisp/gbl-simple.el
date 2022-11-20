;;; gbl-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

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
;; Common commands for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'gbl-common)

(defgroup gbl-simple ()
  "Generic utilities for my dotemacs."
  :group 'editing)

;; Got those numbers from `string-to-char'
(defcustom gbl-simple-insert-pair-alist
  '(("' Single quote"        . (39 39))     ; ' '
    ("\" Double quotes"      . (34 34))     ; " "
    ("` Elisp quote"         . (96 39))     ; ` '
    ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
    ("“ Double apostrophes"  . (8220 8221)) ; “ ”
    ("( Parentheses"         . (40 41))     ; ( )
    ("{ Curly brackets"      . (123 125))   ; { }
    ("[ Square brackets"     . (91 93))     ; [ ]
    ("< Angled brackets"     . (60 62))     ; < >
    ("« Εισαγωγικά Gr quote" . (171 187))   ; « »
    ("= Equals signs"        . (61 61))     ; = =
    ("~ Tilde"               . (126 126))   ; ~ ~
    ("* Asterisks"           . (42 42))     ; * *
    ("/ Forward Slash"       . (47 47))     ; / /
    ("_ underscores"         . (95 95)))    ; _ _
  "Alist of pairs for use with `gbl-simple-insert-pair-completion'."
  :type 'alist
  :group 'gbl-simple)

(defcustom gbl-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `gbl-simple-inset-date'."
  :type 'string
  :group 'gbl-simple)

(defcustom gbl-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `gbl-simple-inset-date'."
  :type 'string
  :group 'gbl-simple)

(defcustom gbl-simple-focusable-help-commands
  '( describe-symbol describe-function
     describe-variable describe-key
     view-lossage)
  "Commands whose buffers should be focused when displayed.
This makes it easier to dismiss them at once.

Also see `gbl-simple-focus-help-buffers'."
  :type '(repeat symbol)
  :group 'gbl-simple)

(defcustom gbl-simple-scratch-buffer-default-mode 'markdown-mode
  "Default major mode for `gbl-simple-scratch-buffer'."
  :type 'symbol
  :group 'gbl-simple)

;;; Generic setup

;;;; Scratch buffers
;; The idea is based on the `scratch.el' package by Ian Eure:
;; <https://github.com/ieure/scratch-el>.

(defun gbl-simple--scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           when (and (functionp sym)
                     (or (provided-mode-derived-p sym 'text-mode)
                         (provided-mode-derived-p sym 'prog-mode)))
           collect sym))

(defun gbl-simple--scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (string (format "Scratch buffer for: %s\n\n" major))
         (text (concat string region))
         (buf (format "*%s scratch*" major)))
    (with-current-buffer (pop-to-buffer buf)
      (funcall major)
      (if (gbl-common-empty-buffer-p)
          ;; We could use `save-restriction' for narrowed buffers, but
          ;; it is overkill.
          (progn
            (insert text)
            (goto-char (point-min))
            (comment-region (point-at-bol) (point-at-eol))
            (goto-char (point-max)))
        (goto-char (point-max))
        (when (gbl-common-line-regexp-p 'non-empty)
          (insert "\n\n"))
        (insert region)))))

;;;###autoload
(defun gbl-simple-scratch-buffer (&optional arg)
  "Produce a scratch buffer matching the current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `gbl-simple-scratch-buffer-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.  Candidates are derivatives of `text-mode' or
`prog-mode'.

If region is active, copy its contents to the new scratch
buffer.

Buffers are named as *MAJOR-MODE scratch*.  If one already exists
for the given MAJOR-MODE, any text is appended to it."
  (interactive "P")
  (let* ((default-mode gbl-simple-scratch-buffer-default-mode)
         (modes (gbl-simple--scratch-list-modes))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))
         mode)
    (pcase (prefix-numeric-value arg)
      (16 (progn
            (setq mode (intern (completing-read "Select major mode: " modes nil t)))
            (gbl-simple--scratch-buffer-setup region mode)))
      (4 (gbl-simple--scratch-buffer-setup region default-mode))
      (_ (gbl-simple--scratch-buffer-setup region)))))

;;; Commands

;;;; General commands

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defun gbl-simple-describe-symbol ()
  "Run `describe-symbol' for the `symbol-at-point'."
  (interactive)
  (describe-symbol (symbol-at-point)))

;;;; Commands for lines

;;;###autoload
(defun gbl-simple-new-line-below (&optional arg)
  "Create an empty line below the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument]).  Also see
`gbl-simple-new-line-above'."
  (interactive "P")
  (end-of-line)
  (if arg
      (newline-and-indent)
    (newline)))

;;;###autoload
(defun gbl-simple-new-line-above (&optional arg)
  "Create an empty line above the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument])."
  (interactive "P")
  (let ((indent (or arg nil)))
    (if (or (bobp)
            (line-number-at-pos (point-min)))
        (progn
          (beginning-of-line)
          (newline)
          (forward-line -1))
      (forward-line -1)
      (gbl-simple-new-line-below indent))))

;;;###autoload
(defun gbl-simple-copy-line-or-region (&optional arg)
  "Kill-save the current line or active region.
With optional ARG (\\[universal-argument]) duplicate the target
instead.  When region is active, also apply context-aware
indentation while duplicating."
  (interactive "P")
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (pbol (point-at-bol))
         (peol (point-at-eol))
         (indent (if (eq (or rbeg rend) pbol) nil arg)))
    (cond
     ((use-region-p)
      (if arg
          (let ((text (buffer-substring rbeg rend)))
            (when (eq (point) rbeg)
              (exchange-point-and-mark))
            (gbl-simple-new-line-below indent)
            (insert text))
        (copy-region-as-kill rbeg rend)
        (message "Current region copied")))
     (t
      (if arg
          (let ((text (buffer-substring pbol peol)))
            (goto-char (point-at-eol))
            (newline)
            (insert text))
        (copy-region-as-kill pbol peol)
        (message "Current line copied"))))))

;;;###autoload
(defun gbl-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point-at-bol) (point-at-eol)))
  (yank))

;;;###autoload
(defun gbl-simple-multi-line-next ()
  "Move point 15 lines down."
  (interactive)
  (forward-line 15))

;;;###autoload
(defun gbl-simple-multi-line-prev ()
  "Move point 15 lines up."
  (interactive)
  (forward-line -15))

;;;###autoload
(defun gbl-simple-kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))

;;;; Commands for text insertion or manipulation

(defvar gbl-simple--character-hist '()
  "History of inputs for `gbl-simple-insert-pair-completion'.")

(defun gbl-simple--character-prompt (chars)
  "Helper of `gbl-simple-insert-pair-completion' to read CHARS."
  (let ((def (car gbl-simple--character-hist)))
    (completing-read
     (format "Select character [%s]: " def)
     chars nil t nil 'gbl-simple--character-hist def)))

(define-obsolete-function-alias
  'gbl-simple-insert-pair-completion
  'gbl-simple-insert-pair "2021-07-30")

;;;###autoload
(defun gbl-simple-insert-pair (pair &optional count)
  "Insert PAIR from `gbl-simple-insert-pair-alist'.
Operate on the symbol at point.  If the region is active, use it
instead.

With optional COUNT (either as a natural number from Lisp or a
universal prefix argument (\\[universal-argument]) when used
interactively) prompt for the number of delimiters to insert."
  (interactive
   (list
    (gbl-simple--character-prompt gbl-simple-insert-pair-alist)
    current-prefix-arg))
  (let* ((data gbl-simple-insert-pair-alist)
         (left (cadr (assoc pair data)))
         (right (caddr (assoc pair data)))
         (n (cond
             ((and count (natnump count))
              count)
             (count
              (read-number "How many delimiters?" 2))
             (1)))
         (beg)
         (end))
    (cond
     ((region-active-p)
      (setq beg (region-beginning)
            end (region-end)))
     ((when (thing-at-point 'symbol)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (setq beg (car bounds)
                end (cdr bounds)))))
     (t (setq beg (point)
              end (point))))
    (save-excursion
      (goto-char end)
      (dotimes (_ n)
        (insert right))
      (goto-char beg)
      (dotimes (_ n)
        (insert left)))))

;;;###autoload
(defun gbl-simple-delete-pair-dwim ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is
controlled by `delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

;;;###autoload
(defun gbl-simple-insert-date (&optional arg)
  "Insert the current date as `gbl-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `gbl-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date gbl-simple-date-specifier)
         (time gbl-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

(autoload 'ffap-url-at-point "ffap")
(defvar ffap-string-at-point-region)

;;;###autoload
(defun gbl-simple-escape-url ()
  "Wrap URL (or email address) in angled brackets."
  (interactive)
  (when-let ((url (ffap-url-at-point)))
    (let* ((reg ffap-string-at-point-region)
           (beg (car reg))
           (end (cadr reg))
           (string (if (string-match-p "^mailto:" url)
                       (substring url 7)
                     url)))
      (delete-region beg end)
      (insert (format "<%s>" string)))))

(defun gbl-simple-zap-to-char-backward (char &optional arg)
  "Backward `zap-to-char' for CHAR.
Optional ARG is a numeric prefix to match ARGth occurence of
CHAR."
  (interactive
   (list
    (read-char-from-minibuffer "Zap to char: " nil 'read-char-history)
    (prefix-numeric-value current-prefix-arg)))
  (zap-to-char (- arg) char t))

;;;; Commands for object transposition

(defmacro gbl-simple-transpose (name scope &optional doc)
  "Macro to produce transposition functions.
NAME is the function's symbol.  SCOPE is the text object to
operate on.  Optional DOC is the function's docstring.

Transposition over an active region will swap the object at
mark (region beginning) with the one at point (region end)"
  `(defun ,name (arg)
     ,doc
     (interactive "p")
     (let ((x (format "%s-%s" "transpose" ,scope)))
       (if (use-region-p)
           (funcall (intern x) 0)
         (funcall (intern x) arg)))))

(gbl-simple-transpose
 gbl-simple-transpose-lines
 "lines"
 "Transpose lines or swap over active region.")

(gbl-simple-transpose
 gbl-simple-transpose-paragraphs
 "paragraphs"
 "Transpose paragraphs or swap over active region.")

(gbl-simple-transpose
 gbl-simple-transpose-sentences
 "sentences"
 "Transpose sentences or swap over active region.")

(gbl-simple-transpose
 gbl-simple-transpose-sexps
 "sexps"
 "Transpose balanced expressions or swap over active region.")

;;;###autoload
(defun gbl-simple-transpose-chars ()
  "Always transposes the two characters before point.
There is no 'dragging' the character forward.  This is the
behaviour of `transpose-chars' when point is at the end of the
line."
  (interactive)
  (transpose-chars -1)
  (forward-char))

;;;###autoload
(defun gbl-simple-transpose-words (arg)
  "Transpose ARG words.

If region is active, swap the word at mark (region beginning)
with the one at point (region end).

Otherwise, and while inside a sentence, this behaves as the
built-in `transpose-words', dragging forward the word behind the
point.  The difference lies in its behaviour at the end or
beginnning of a line, where it will always transpose the word at
point with the one behind or ahead of it (effectively the
last/first two words)."
  (interactive "p")
  (cond
   ((use-region-p)
    (transpose-words 0))
   ((eq (point) (point-at-eol))
    (transpose-words -1))
   ((eq (point) (point-at-bol))
    (forward-word 1)
    (transpose-words 1))
   (t
    (transpose-words arg))))

;;;; Commands for marking syntactic constructs

(defmacro gbl-simple-mark (name object &optional docstring)
  "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
  `(defun ,name (&optional arg allow-extend)
     ,docstring
     (interactive "P\np")
     (let ((x (format "%s-%s" "forward" ,object)))
       (cond ((and allow-extend
                   (or (and (eq last-command this-command) (mark t))
                       (region-active-p)))
              (setq arg (if arg (prefix-numeric-value arg)
                          (if (< (mark) (point)) -1 1)))
              (set-mark
               (save-excursion
                 (goto-char (mark))
                 (funcall (intern x) arg)
                 (point))))
             (t
              (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                (unless (consp bounds)
                  (user-error "No %s at point" ,object))
                (if (>= (prefix-numeric-value arg) 0)
                    (goto-char (car bounds))
                  (goto-char (cdr bounds)))
                (push-mark
                 (save-excursion
                   (funcall (intern x) (prefix-numeric-value arg))
                   (point)))
                (activate-mark)))))))

(gbl-simple-mark
 gbl-simple-mark-word
 "word"
 "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `gbl-simple-mark-symbol'
instead.")

(gbl-simple-mark
 gbl-simple-mark-symbol
 "symbol"
 "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

;;;###autoload
(defun gbl-simple-mark-sexp-backward (&optional arg)
  "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
  (interactive "P")
  (if arg
      (mark-sexp (- arg) t)
    (mark-sexp (- 1) t)))

;;;###autoload
(defun gbl-simple-mark-construct-dwim (&optional arg)
  "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `gbl-simple-mark-sexp-backward',
`mark-sexp', and `gbl-simple-mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `gbl-simple-mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
  (interactive "P")
  (cond
   ((symbol-at-point)
    (gbl-simple-mark-symbol arg t))
   ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
    (gbl-simple-mark-sexp-backward arg))
   (t
    (mark-sexp arg t))))

;;;; Commands for code navigation (work in progress)

;;;###autoload
(defun gbl-simple-downward-list (&optional arg)
  "Like `backward-up-list' but defaults to a forward motion.
With optional ARG, move that many times in the given
direction (negative is forward due to this being a
'backward'-facing command)."
  (interactive "P")
  (backward-up-list (or arg -1)))

;;;; Commands for paragraphs

;;;###autoload
(defun gbl-simple-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active,
while respecting any empty lines (so multiple paragraphs are not
joined, just unfilled).  If no region is active, operate on the
paragraph.  The idea is to produce the opposite effect of both
`fill-paragraph' and `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))

;;;; Commands for windows and pages

;;;###autoload
(defun gbl-simple-narrow-visible-window ()
  "Narrow buffer to wisible window area.
Also check `gbl-simple-narrow-dwim'."
  (interactive)
  (let* ((bounds (gbl-common-window-bounds))
         (window-area (- (cadr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cadr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun gbl-simple-narrow-dwim ()
  "Do-what-I-mean narrowing.
If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `gbl-common-page-p', narrow to
the current page boundaries.

If no region is active and no pages exist, narrow to the visible
portion of the window.

If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (narrow-to-region (region-beginning) (region-end)))
   ((gbl-common-page-p)
    (narrow-to-page))
   ((null (buffer-narrowed-p))
    (gbl-simple-narrow-visible-window))
   ((widen))))

(defun gbl-simple--narrow-to-page (count &optional back)
  "Narrow to COUNTth page with optional BACK motion."
  (if back
      (narrow-to-page (or (- count) -1))
    (narrow-to-page (or (abs count) 1)))
  ;; Avoids the problem of skipping pages while cycling back and forth.
  (goto-char (point-min)))

;;;###autoload
(defun gbl-simple-forward-page-dwim (&optional count)
  "Move to next or COUNTth page forward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (gbl-simple--narrow-to-page count)
    (forward-page count)
    (setq this-command 'forward-page)))

;;;###autoload
(defun gbl-simple-backward-page-dwim (&optional count)
  "Move to previous or COUNTth page backward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (gbl-simple--narrow-to-page count t)
    (backward-page count)
    (setq this-command 'backward-page)))

;;;###autoload
(defun gbl-simple-delete-page-delimiters (&optional beg end)
  "Delete lines with just page delimiters in the current buffer.
When region is active, only operate on the region between BEG and
END, representing the point and mark."
  (interactive "r")
  (let (b e)
    (if (use-region-p)
        (setq b beg
              e end)
      (setq b (point-min)
            e (point-max)))
  (widen)
  (flush-lines (format "%s$" page-delimiter) b e)
  (setq this-command 'flush-lines)))

;; Inspired by Pierre Neidhardt's windower:
;; https://gitlab.com/ambrevar/emacs-windower/-/blob/master/windower.el
(defvar gbl-simple--windows-current nil
  "Current window configuration.")

;;;###autoload
(define-minor-mode gbl-simple-monocle
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " -M-"
  :global nil
  (let ((win gbl-simple--windows-current))
    (if (one-window-p)
        (when win
          (set-window-configuration win))
      (setq gbl-simple--windows-current (current-window-configuration))
      (delete-other-windows))))

(defun gbl-simple--monocle-disable ()
  "Set variable `gbl-simple-monocle' to nil, when appropriate.
To be hooked to `window-configuration-change-hook'."
  (when (and gbl-simple-monocle (not (one-window-p)))
    (delete-other-windows)
    (gbl-simple-monocle -1)
    (set-window-configuration gbl-simple--windows-current)))

(add-hook 'window-configuration-change-hook #'gbl-simple--monocle-disable)

;;;; Commands for buffers

;;;###autoload
(defun gbl-simple-kill-buffer-current (&optional arg)
  "Kill current buffer or abort recursion when in minibuffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well."
  (interactive "P")
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer)))
  (when (and arg
             (not (one-window-p)))
    (delete-window)))

;;;###autoload
(defun gbl-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(defun gbl-simple--buffer-major-mode-prompt ()
  "Prompt of `gbl-simple-buffers-major-mode'."
  (let ((major major-mode))
    (read-buffer
     (format "Buffer for %s: " major)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (derived-mode-p major))))))

;;;###autoload
(defun gbl-simple-buffers-major-mode (buffer)
  "Select BUFFER matching the current one's major mode."
  (interactive
   (list (gbl-simple--buffer-major-mode-prompt)))
  (switch-to-buffer buffer))

(defun gbl-simple--buffer-vc-root-prompt ()
  "Prompt of `gbl-simple-buffers-vc-root'."
  (let ((root (or (vc-root-dir)
                   (locate-dominating-file "." ".git"))))
    (read-buffer
     (format "Buffers in %s: " root)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (string-match-p root default-directory))))))

;;;###autoload
(defun gbl-simple-buffers-vc-root (buffer)
  "Select BUFFER matching the current one's VC root."
  (interactive
   (list (gbl-simple--buffer-vc-root-prompt)))
  (switch-to-buffer buffer))

(provide 'gbl-simple)
;;; gbl-simple.el ends here

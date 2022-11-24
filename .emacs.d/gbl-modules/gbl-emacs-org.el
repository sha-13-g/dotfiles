;;; Org-mode (personal information manager)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(use-package org-modern)

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

    (setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "/home/gbl13/Documents/org/gtd.org" "Things To Get Done")
             "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :ensure nil
  :hook (org-mode . org-indent-mode)
  :config

  (define-key org-mode-map (kbd "H") #'org-shiftleft)
  (define-key org-mode-map (kbd "L") #'org-shiftright)
  (define-key org-mode-map (kbd "J") #'org-shiftup)
  (define-key org-mode-map (kbd "K") #'org-shiftdown)  

    (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  
  (setq org-directory "~/Documents/org/")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s@)" "NEXT(n@)" "WAIT(w@)" "|" "DONE(d)")
          (sequence "READ(r@)" "|" "READED(R)")
          (sequence "PROJECT(p@)" "|" "FINISH(f)")
          (sequence "|" "CANCELED(c)" "DEFERRED(D)")))
  
  (setq org-global-properties
        '(("Effort_ALL" . "0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00")))

  (setq org-columns-default-format
        "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")

  (setq org-priority-default ?A)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?J)
  (setq org-agenda-files
        '("~/Documents/org/"))
  

  ;; Emacs ask what to do for the time spend over doing something else during a cloking after 5 min
  (setq org-clock-idle-time 5)
    ;; To save the clock history accross sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-tag-alist ; I don't really use those, but whatever
        '(("@book" . ?b)
          ("@vim" . ?v)
          ("software" . ?s)
          ("@work" . ?w)
          ("@home" . ?h)
          ("@email" . ?e)
          ("@emacs" . ?E)
          ("@website" . ?W)))

  (add-to-list
   'org-src-lang-modes '(("plantuml" . plantuml)
                         ("python" . python)
                         ("javascript" . js)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((plantuml . t)
                               (js . t)
                               (python . t)))
  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("NEXT" . "purple")
          ("DONE" . org-done)
          ("READ" . "yellow")
          ("WAIT" . "red")
          ("PROJECT" . "blue")
          ("CANCELED" . org-done)))

  ;; Enforce all the sub tasks to be DONE for the parent to be
  ;; (setq org-enforce-todo-dependencies nil)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)

  (use-package org-habit
    :ensure nil
    :config (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)))

;; (use-package org
;;   :ensure nil
;;   :config
;;   (setq org-directory (convert-standard-filename "~/Documents/org"))
;;   (setq org-imenu-depth 7)
;; ;;;; general settings
;;   (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
;;   (setq org-special-ctrl-a/e nil)
;;   (setq org-special-ctrl-k nil)
;;   (setq org-M-RET-may-split-line '((default . nil)))
;;   (setq org-hide-emphasis-markers nil)
;;   (setq org-hide-macro-markers nil)
;;   (setq org-hide-leading-stars nil)
;;   (setq org-cycle-separator-lines 0)
;;   (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
;;         '(("s" . "src")
;;           ("E" . "src emacs-lisp")
;;           ("e" . "example")
;;           ("q" . "quote")
;;           ("v" . "verse")
;;           ("V" . "verbatim")
;;           ("c" . "center")
;;           ("C" . "comment")))
;;   (setq org-catch-invisible-edits 'show)
;;   (setq org-return-follows-link nil)
;;   (setq org-loop-over-headlines-in-active-region 'start-level)
;;   (setq org-modules '(ol-info ol-eww))
;;   (setq org-use-sub-superscripts '{})
;;   (setq org-insert-heading-respect-content t)

;; ;;;; refile, todo
;;   (setq org-refile-targets
;;         '((org-agenda-files . (:maxlevel . 2))
;;           (nil . (:maxlevel . 2))))
;;   (setq org-refile-use-outline-path t)
;;   (setq org-refile-allow-creating-parent-nodes 'confirm)
;;   (setq org-refile-use-cache t)
;;   (setq org-reverse-note-order nil)
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
;;   (setq org-todo-keyword-faces
;;         '(("WAIT" . '(bold org-todo))
;;           ("MAYBE" . '(bold shadow))
;;           ("CANCEL" . '(bold org-done))))
;;   (setq org-use-fast-todo-selection 'expert)
;;   (setq org-priority-faces
;;         '((?A . '(bold org-priority))
;;           (?B . org-priority)
;;           (?C . '(shadow org-priority))))
;;   (setq org-fontify-done-headline nil)
;;   (setq org-fontify-todo-headline nil)
;;   (setq org-fontify-quote-and-verse-blocks t)
;;   (setq org-fontify-whole-heading-line nil)
;;   (setq org-fontify-whole-block-delimiter-line nil)
;;   (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
;;   (setq org-enforce-todo-dependencies t)
;;   (setq org-enforce-todo-checkbox-dependencies t)
;;   (setq org-track-ordered-property-with-tag t)
;;   (setq org-highest-priority ?A)
;;   (setq org-lowest-priority ?C)
;;   (setq org-default-priority ?A)

;; ;;;; tags
  

;;   (setq org-auto-align-tags nil)
;;   (setq org-tags-column 0)

;; ;;;; log
;;   (setq org-log-done 'time)
;;   (setq org-log-into-drawer t)
;;   (setq org-log-note-clock-out nil)
;;   (setq org-log-redeadline 'time)
;;   (setq org-log-reschedule 'time)
;;   (setq org-read-date-prefer-future 'time)

;; ;;;; links
;;   (setq org-link-keep-stored-after-insertion nil)
;;   ;; TODO 2021-10-15 org-link-make-description-function

;; ;;;; capture
;;   (setq org-capture-templates
;;         `(("b" "Basic task for future review" entry
;;            (file+headline "tasks.org" "Tasks to be reviewed")
;;            ,(concat "* %^{Title}\n"
;;                     ":PROPERTIES:\n"
;;                     ":CAPTURED: %U\n"
;;                     ":END:\n\n"
;;                     "%i%l")
;;            :empty-lines-after 1)
;;           ("c" "Clock in to a task" entry
;;            (file+headline "tasks.org" "Clocked tasks")
;;            ,(concat "* TODO %^{Title}\n"
;;                     "SCHEDULED: %T\n"
;;                     ":PROPERTIES:\n"
;;                     ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
;;                     ":END:\n\n"
;;                     "%a\n")
;;            :prepend t
;;            :clock-in t
;;            :clock-keep t
;;            :immediate-finish t
;;            :empty-lines-after 1)
;;           ("m" "Memorandum of conversation" entry
;;            (file+headline "tasks.org" "Tasks to be reviewed")
;;            ,(concat "* Memorandum of conversation with %^{Person}\n"
;;                     ":PROPERTIES:\n"
;;                     ":CAPTURED: %U\n"
;;                     ":END:\n\n"
;;                     "%i%?")
;;            :empty-lines-after 1)
;;           ("t" "Task with a due date" entry
;;            (file+headline "tasks.org" "Tasks with a date")
;;            ,(concat "* TODO %^{Title} %^g\n"
;;                     "SCHEDULED: %^t\n"
;;                     ":PROPERTIES:\n"
;;                     ":CAPTURED: %U\n"
;;                     ":END:\n\n"
;;                     "%a\n%i%?")
;;            :empty-lines-after 1)
;;           ("e" "Email note" entry
;;            (file+headline "tasks.org" "Tasks to be reviewed")
;;            ,(concat "* MAYBE %:subject :mail:\n"
;;                     ":PROPERTIES:\n"
;;                     ":CAPTURED: %U\n"
;;                     ":END:\n\n"
;;                     "%a\n%i%?")
;;            :empty-lines-after 1)))

;;   (setq org-capture-templates-contexts
;;         '(("e" ((in-mode . "notmuch-search-mode")
;;                 (in-mode . "notmuch-show-mode")
;;                 (in-mode . "notmuch-tree-mode")))))

;; ;;;; agenda
;; ;;;;; Basic agenda setup
;;   (setq org-default-notes-file (thread-last org-directory (expand-file-name "notes.org")))
;;   (setq org-agenda-files `(,org-directory "~/Documents"))
;;   (setq org-agenda-span 'week)
;;   (setq org-agenda-start-on-weekday 1)  ; Monday
;;   (setq org-agenda-confirm-kill t)
;;   (setq org-agenda-show-all-dates t)
;;   (setq org-agenda-show-outline-path nil)
;;   (setq org-agenda-window-setup 'current-window)
;;   (setq org-agenda-skip-comment-trees t)
;;   (setq org-agenda-menu-show-matcher t)
;;   (setq org-agenda-menu-two-columns nil)
;;   (setq org-agenda-sticky nil)
;;   (setq org-agenda-custom-commands-contexts nil)
;;   (setq org-agenda-max-entries nil)
;;   (setq org-agenda-max-todos nil)
;;   (setq org-agenda-max-tags nil)
;;   (setq org-agenda-max-effort nil)

;;   ;; NOTE 2021-12-07: In my `gbl-org.el' (see further below), I add
;;   ;; `org-agenda-to-appt' to various relevant hooks.
;;   ;;
;;   ;; Create reminders for tasks with a due date when this file is read.
;;   (run-at-time (* 60 5) nil #'org-agenda-to-appt)

;; ;;;;; General agenda view options
;;   ;; NOTE 2021-12-07: Check further below my `org-agenda-custom-commands'
;;   (setq org-agenda-prefix-format
;;         '((agenda . " %i %-12:c%?-12t% s")
;;           (todo . " %i %-12:c")
;;           (tags . " %i %-12:c")
;;           (search . " %i %-12:c")))
;;   (setq org-agenda-sorting-strategy
;;         '(((agenda habit-down time-up priority-down category-keep)
;;            (todo priority-down category-keep)
;;            (tags priority-down category-keep)
;;            (search category-keep))))
;;   (setq org-agenda-breadcrumbs-separator "->")
;;   (setq org-agenda-todo-keyword-format "%-1s")
;;   (setq org-agenda-fontify-priorities 'cookies)
;;   (setq org-agenda-category-icon-alist nil)
;;   (setq org-agenda-remove-times-when-in-prefix nil)
;;   (setq org-agenda-remove-timeranges-from-blocks nil)
;;   (setq org-agenda-compact-blocks nil)
;;   (setq org-agenda-block-separator ?—)

;; ;;;;; Agenda marks
;;   (setq org-agenda-bulk-mark-char "#")
;;   (setq org-agenda-persistent-marks nil)

;; ;;;;; Agenda diary entries
;;   (setq org-agenda-insert-diary-strategy 'date-tree)
;;   (setq org-agenda-insert-diary-extract-time nil)
;;   (setq org-agenda-include-diary nil)

;; ;;;;; Agenda follow mode
;;   (setq org-agenda-start-with-follow-mode nil)
;;   (setq org-agenda-follow-indirect t)

;; ;;;;; Agenda multi-item tasks
;;   (setq org-agenda-dim-blocked-tasks t)
;;   (setq org-agenda-todo-list-sublevels t)

;; ;;;;; Agenda filters and restricted views
;;   (setq org-agenda-persistent-filter nil)
;;   (setq org-agenda-restriction-lock-highlight-subtree t)

;; ;;;;; Agenda items with deadline and scheduled timestamps
;;   (setq org-agenda-include-deadlines t)
;;   (setq org-deadline-warning-days 5)
;;   (setq org-agenda-skip-scheduled-if-done nil)
;;   (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;   (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
;;   (setq org-agenda-skip-deadline-if-done nil)
;;   (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
;;   (setq org-agenda-skip-scheduled-delay-if-deadline nil)
;;   (setq org-agenda-skip-additional-timestamps-same-entry nil)
;;   (setq org-agenda-skip-timestamp-if-done nil)
;;   (setq org-agenda-search-headline-for-time nil)
;;   (setq org-scheduled-past-days 365)
;;   (setq org-deadline-past-days 365)
;;   (setq org-agenda-move-date-from-past-immediately-to-today t)
;;   (setq org-agenda-show-future-repeats t)
;;   (setq org-agenda-prefer-last-repeat nil)
;;   (setq org-agenda-timerange-leaders
;;         '("" "(%d/%d): "))
;;   (setq org-agenda-scheduled-leaders
;;         '("Scheduled: " "Sched.%2dx: "))
;;   (setq org-agenda-inactive-leader "[")
;;   (setq org-agenda-deadline-leaders
;;         '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
;;   ;; Time grid
;;   (setq org-agenda-time-leading-zero t)
;;   (setq org-agenda-timegrid-use-ampm nil)
;;   (setq org-agenda-use-time-grid t)
;;   (setq org-agenda-show-current-time-in-grid t)
;;   (setq org-agenda-current-time-string
;;         (concat "Now " (make-string 70 ?-)))
;;   (setq org-agenda-time-grid
;;         '((daily today require-timed)
;;           (0600 0700 0800 0900 1000 1100
;;                 1200 1300 1400 1500 1600
;;                 1700 1800 1900 2000 2100)
;;           " ....." "-----------------"))
;;   (setq org-agenda-default-appointment-duration nil)

;; ;;;;; Agenda global to-do list
;;   (setq org-agenda-todo-ignore-with-date t)
;;   (setq org-agenda-todo-ignore-timestamp t)
;;   (setq org-agenda-todo-ignore-scheduled t)
;;   (setq org-agenda-todo-ignore-deadlines t)
;;   (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
;;   (setq org-agenda-tags-todo-honor-ignore-options nil)

;; ;;;;; Agenda tagged items
;;   (setq org-agenda-show-inherited-tags t)
;;   (setq org-agenda-use-tag-inheritance
;;         '(todo search agenda))
;;   (setq org-agenda-hide-tags-regexp nil)
;;   (setq org-agenda-remove-tags nil)
;;   (setq org-agenda-tags-column -100)

;; ;;;;; Agenda entry
;;   ;; NOTE: I do not use this right now.  Leaving everything to its
;;   ;; default value.
;;   (setq org-agenda-start-with-entry-text-mode nil)
;;   (setq org-agenda-entry-text-maxlines 5)
;;   (setq org-agenda-entry-text-exclude-regexps nil)
;;   (setq org-agenda-entry-text-leaders "    > ")

;; ;;;;; Agenda logging and clocking
;;   ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
;;   ;; to its default value for the time being.
;;   (setq org-agenda-log-mode-items '(closed clock))
;;   (setq org-agenda-clock-consistency-checks
;;         '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
;;                          ("4:00")
;;                          :default-face ; This should definitely be reviewed
;;                          ((:background "DarkRed")
;;                           (:foreground "white"))
;;                          :overlap-face nil :gap-face nil :no-end-time-face nil
;;                          :long-face nil :short-face nil)))
;;   (setq org-agenda-log-mode-add-notes t)
;;   (setq org-agenda-start-with-log-mode nil)
;;   (setq org-agenda-start-with-clockreport-mode nil)
;;   (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
;;   (setq org-agenda-search-view-always-boolean nil)
;;   (setq org-agenda-search-view-force-full-words nil)
;;   (setq org-agenda-search-view-max-outline-level 0)
;;   (setq org-agenda-search-headline-for-time t)
;;   (setq org-agenda-use-time-grid t)
;;   (setq org-agenda-cmp-user-defined nil)
;;   (setq org-agenda-sort-notime-is-late t) ; Org 9.4
;;   (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;; ;;;;; Agenda column view
;;   ;; NOTE I do not use these, but may need them in the future.
;;   (setq org-agenda-view-columns-initially nil)
;;   (setq org-agenda-columns-show-summaries t)
;;   (setq org-agenda-columns-compute-summary-properties t)
;;   (setq org-agenda-columns-add-appointments-to-effort-sum nil)
;;   (setq org-agenda-auto-exclude-function nil)
;;   (setq org-agenda-bulk-custom-functions nil)

;; ;;;;; Agenda habits
;;   (require 'org-habit)
;;   (setq org-habit-graph-column 50)
;;   (setq org-habit-preceding-days 9)

;; ;;;; code blocks
;;   (setq org-confirm-babel-evaluate nil)
;;   (setq org-src-window-setup 'current-window)
;;   (setq org-edit-src-persistent-message nil)
;;   (setq org-src-fontify-natively t)
;;   (setq org-src-preserve-indentation t)
;;   (setq org-src-tab-acts-natively t)
;;   (setq org-edit-src-content-indentation 0)

;; ;;;; export
;;   (setq org-export-with-toc t)
;;   (setq org-export-headline-levels 8)
;;   (setq org-export-dispatch-use-expert-ui nil)
;;   (setq org-html-htmlize-output-type nil)
;;   (setq org-html-head-include-default-style nil)
;;   (setq org-html-head-include-scripts nil)
;;   (require 'ox-texinfo)
;;   (require 'ox-md)
;;   ;; FIXME: how to remove everything else?
;;   (setq org-export-backends '(html texinfo md))

;; ;;;; IDs
;;   (setq org-id-link-to-org-use-id
;;         'create-if-interactive-and-no-custom-id)

;; ;;;; Hooks and key bindings

;;   ;; See my `pulsar' package, which is declared further above (otherwise
;;   ;; I would wrap this in `with-eval-after-load'):
;;   ;; <https://protesilaos.com/emacs/pulsar>
;;   (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
;;     (add-hook hook #'pulsar-recenter-middle)
;;     (add-hook hook #'pulsar-reveal-entry))

;;   (let ((map global-map))
;;     (define-key map (kbd "C-c a") #'org-agenda)
;;     (define-key map (kbd "C-c c") #'org-capture)
;;     (define-key map (kbd "C-c l") #'org-store-link)
;;     (define-key map (kbd "C-c o") #'org-open-at-point-global))
;;   (let ((map org-mode-map))
;;     (define-key map (kbd "C-'") nil)
;;     (define-key map (kbd "C-,") nil)
;;     (define-key map (kbd "<C-return>") nil)
;;     (define-key map (kbd "<C-S-return>") nil)
;;     (define-key map (kbd "C-M-S-<right>") nil)
;;     (define-key map (kbd "C-M-S-<left>") nil)
;;     (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
;;     (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display)))

;; ;;; Custom extensions (gbl-org.el)
;; (use-package gbl-org
;;   :ensure nil
;;   :config
;;   (setq org-agenda-format-date #'gbl-org-agenda-format-date-aligned)

;;   ;; Check the variable `gbl-org-custom-daily-agenda' in gbl-org.el
;;   (setq org-agenda-custom-commands
;;         `(("A" "Daily agenda and top priority tasks"
;;            ,gbl-org-custom-daily-agenda
;;            ((org-agenda-fontify-priorities nil)
;;             (org-agenda-dim-blocked-tasks nil)))
;;           ("P" "Plain text daily agenda and top priorities"
;;            ,gbl-org-custom-daily-agenda
;;            ((org-agenda-with-colors nil)
;;             (org-agenda-prefix-format "%t %s")
;;             (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
;;             (org-agenda-fontify-priorities nil)
;;             (org-agenda-remove-tags t))
;;            ("agenda.txt"))))

;;   ;; I bind `org-agenda' to C-c a, so this one puts me straight into my
;;   ;; custom block agenda.
;;   (define-key global-map (kbd "C-c A") (lambda () (interactive) (org-agenda nil "A")))

;;   (add-to-list 'org-capture-templates
;;                '("j" "Music suggestion (jukebox)" entry
;;                  (file+headline "tasks.org" "Music suggestions")
;;                  #'gbl-org-capture-jukebox
;;                  :empty-lines-after 1
;;                  :immediate-finish t)))

;; ;;; org-modern
;; (use-package org-modern
;;   :config
;;   (setq org-modern-label-border 1)
;;   (setq org-modern-variable-pitch nil)
;;   (setq org-modern-timestamp t)
;;   (setq org-modern-table t)
;;   (setq org-modern-table-vertical 1)
;;   (setq org-modern-table-horizontal 0)
;;   (setq org-modern-list ; I swap the defaults for + and *
;;         '((?+ . "•")
;;           (?- . "–")
;;           (?* . "◦")))
;;   ;; I don't use those in documents anyway, and if I ever do I need to
;;   ;; remember what their standard looks are.
;;   (setq org-modern-internal-target nil)
;;   (setq org-modern-radio-target nil)

;;   ;; NOTE 2022-03-05: The variables that are commented out are the
;;   ;; defaults.

;;   ;; (setq org-modern-star ["◉""○""◈""◇""⁕"])
;;   ;; (setq org-modern-hide-stars 'leading)
;;   ;; (setq org-modern-checkbox
;;   ;;       '((?X . #("▢✓" 0 2 (composition ((2)))))
;;   ;;         (?- . #("▢–" 0 2 (composition ((2)))))
;;   ;;         (?\s . #("▢" 0 1 (composition ((1)))))))
;;   ;; (setq org-modern-horizontal-rule t)
;;   ;; (setq org-modern-priority t)
;;   ;; (setq org-modern-todo t)
;;   ;; (setq org-modern-tag t)
;;   ;; (setq org-modern-block t)
;;   ;; (setq org-modern-keyword t)
;;   ;; (setq org-modern-statistics t)
;;   ;; (setq org-modern-progress ["○""◔""◐""◕""●"])

;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;   ;; NOTE 2022-03-06: I am experimenting with various styles here.  DO NOT COPY.
;;   ;;
;;   ;; Also note that I do not wrap this in `with-eval-after-load' because
;;   ;; the `modus-themes' are always loaded first in my config.
;;   (defun prot/org-modern-face-tweaks ()
;;     (modus-themes-with-colors
;;       (custom-set-faces
;;        `(org-modern-label ((,class :box (:line-width (-1 . ,org-modern-label-border) :color ,bg-main))))
;;        `(org-modern-done ((,class :inherit org-modern-label :background ,bg-special-faint-mild :foreground ,green-alt-other)))
;;        `(org-modern-priority ((,class :inherit org-modern-label :background ,bg-special-faint-calm :foreground ,magenta-alt-other)))
;;        `(org-modern-statistics ((,class :inherit org-modern-label :background ,bg-special-faint-warm :foreground ,yellow-alt-other)))
;;        `(org-modern-tag ((,class :inherit org-modern-label :background ,bg-special-faint-calm :foreground ,magenta)))
;;        `(org-modern-todo ((,class :inherit org-modern-label :background ,bg-special-faint-warm :foreground ,red-alt-other)))
;;        `(org-modern-date-active ((,class :inherit org-modern-label :background ,bg-alt :foreground ,fg-main)))
;;        `(org-modern-date-inactive ((,class :inherit org-modern-date-active :foreground ,fg-dim)))
;;        `(org-modern-time-active ((,class :inherit org-modern-label :background ,bg-active :foreground ,fg-main)))
;;        `(org-modern-time-inactive ((,class :inherit org-modern-date-inactive))))))

;;   (add-hook 'modus-themes-after-load-theme-hook #'prot/org-modern-face-tweaks))

;; ;;; Calendar
;; (use-package calendar
;;   :ensure nil
;;   :config
;;   (setq calendar-mark-diary-entries-flag t)
;;   (setq calendar-mark-holidays-flag t)
;;   (setq calendar-mode-line-format nil)
;;   (setq calendar-time-display-form
;;         '(24-hours ":" minutes
;;                    (when time-zone
;;                      (format "(%s)" time-zone))))
;;   (setq calendar-week-start-day 1)      ; Monday
;;   (setq calendar-date-style 'iso)
;;   (setq calendar-date-display-form calendar-iso-date-display-form)
;;   (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

;;   (require 'solar)
;;   (setq calendar-latitude 35.17         ; Not my actual coordinates
;;         calendar-longitude 33.36)

;;   (require 'cal-dst)
;;   (setq calendar-standard-time-zone-name "+0200")
;;   (setq calendar-daylight-time-zone-name "+0300"))

;; ;;; Appt (appointment reminders which also integrate with Org agenda)
;; (use-package appt
;;   :ensure nil
;;   :config
;;   (setq appt-display-diary nil)
;;   (setq appt-disp-window-function #'appt-disp-window)
;;   (setq appt-display-mode-line t)
;;   (setq appt-display-interval 3)
;;   (setq appt-audible nil)
;;   (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
;;   (setq appt-message-warning-time 6)

;;   (run-at-time 10 nil #'appt-activate 1))

(provide 'gbl-emacs-org)

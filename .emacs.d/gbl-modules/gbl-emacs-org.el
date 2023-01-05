;;; Org-mode (personal information manager)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(use-package org-modern)

(use-package org
  :ensure nil
  :hook (org-mode . org-indent-mode)
  :config

  ;; (evil-collection-define-key 'normal 'org-mode-map
  ;;   (kbd "H") #'org-shiftleft
  ;;   (kbd "L") #'org-shiftright
  ;;   (kbd "J") #'org-shiftup
  ;;   (kbd "K") #'org-shiftdown)

  (setq org-refile-targets
        '(("~/org/archive.org" :maxlevel . 1)
          ("~/org/gtd.org" :maxlevel . 1)))

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
    (set-face-attribute (car face) nil :font "Hack Nerd Font" :weight 'regular :height (cdr face)))

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

  
  (setq org-directory "~/org/")
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
        '("~/org/"))
  

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

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Getting Things Done")
           "** TODO %?\n %i\n %a")
          ("b" "Birthday" entry (file+headline "~/org/birthdays.org" "Birthdays")
              "** %?\n %^{Birthday}t %i\n %a")))
  ;; Enforce all the sub tasks to be DONE for the parent to be
  ;; (setq org-enforce-todo-dependencies nil)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)

  (use-package org-habit
    :ensure nil
    :config (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)))

(provide 'gbl-emacs-org)

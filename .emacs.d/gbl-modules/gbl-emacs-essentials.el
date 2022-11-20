;;; Read environment variables
;; TODO 2022-07-19: Do I still need this on GNOME Wayland?
(gbl-emacs-elpa-package 'exec-path-from-shell
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

;;; Common auxiliary functions (gbl-common.el)
(gbl-emacs-builtin-package 'gbl-common)
                                        ;
;;; Common custom functions (gbl-simple.el)
;; (gbl-emacs-builtin-package 'gbl-simple ;
;;   (setq gbl-simple-insert-pair-alist
;;         '(("' Single quote"        . (39 39))     ; ' '
;;           ("\" Double quotes"      . (34 34))     ; " "
;;           ("` Elisp quote"         . (96 39))     ; ` '
;;           ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
;;           ("“ Double apostrophes"  . (8220 8221)) ; “ ”
;;           ("( Parentheses"         . (40 41))     ; ( )
;;           ("{ Curly brackets"      . (123 125))   ; { }
;;           ("[ Square brackets"     . (91 93))     ; [ ]
;;           ("< Angled brackets"     . (60 62))     ; < >
;;           ("« Εισαγωγικά Gr quote" . (171 187))   ; « »
;;           ("= Equals signs"        . (61 61))     ; = =
;;           ("~ Tilde"               . (126 126))   ; ~ ~
;;           ("* Asterisks"           . (42 42))     ; * *
;;           ("/ Forward Slash"       . (47 47))     ; / /
;;           ("_ underscores"         . (95 95))))   ; _ _
;;   (setq gbl-simple-date-specifier "%F")
;;   (setq gbl-simple-time-specifier "%R %z")
;;   (setq delete-pair-blink-delay 0.15) ; Emacs28 -- see `gbl-simple-delete-pair-dwim'
;;   (setq gbl-simple-scratch-buffer-default-mode 'markdown-mode)
;;   (setq help-window-select t)
;; 
;;   ;; ;; DEPRECATED 2021-10-15: set `help-window-select' to non-nil.
;;   ;; (setq gbl-simple-focusable-help-commands
;;   ;;       '( describe-symbol
;;   ;;          describe-function
;;   ;;          describe-mode
;;   ;;          describe-variable
;;   ;;          describe-key
;;   ;;          describe-char
;;   ;;          what-cursor-position
;;   ;;          describe-package
;;   ;;          view-lossage))
;;   ;; (gbl-simple-focus-help-buffers 1)
;; 
;;   ;; ;; NOTE 2022-01-20: The idea is good, but the implementation needs
;;   ;; ;; to be refined.
;;   ;; (gbl-simple-rename-help-buffers 1)
;; 
;;   ;; General commands
;;   (let ((map global-map))
;;     (define-key map (kbd "<insert>") nil)
;;     (define-key map (kbd "C-z") nil)
;;     (define-key map (kbd "C-x C-z") nil)
;;     (define-key map (kbd "C-h h") nil)
;;     (define-key map (kbd "M-`") nil)
;;     (define-key map (kbd "C-h .") #'gbl-simple-describe-symbol) ; overrides `display-local-help'
;;     (define-key map (kbd "C-h K") #'describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
;;     (define-key map (kbd "C-h c") #'describe-char) ; overrides `describe-key-briefly'
;;     (define-key map (kbd "C-c s") #'gbl-simple-scratch-buffer)
;;     ;; Commands for lines
;;     (define-key map (kbd "C-S-w") #'gbl-simple-copy-line-or-region)
;;     (define-key map (kbd "C-S-y") #'gbl-simple-yank-replace-line-or-region)
;;     (define-key map (kbd "M-SPC") #'cycle-spacing)
;;     (define-key map (kbd "M-o") #'delete-blank-lines)   ; alias for C-x C-o
;;     (define-key map (kbd "M-k") #'gbl-simple-kill-line-backward)
;;     (define-key map (kbd "C-S-n") #'gbl-simple-multi-line-next)
;;     (define-key map (kbd "C-S-p") #'gbl-simple-multi-line-prev)
;;     (define-key map (kbd "<C-return>") #'gbl-simple-new-line-below)
;;     (define-key map (kbd "<C-S-return>") #'gbl-simple-new-line-above)
;;     ;; Commands for text insertion or manipulation
;;     (define-key map (kbd "C-=") #'gbl-simple-insert-date)
;;     (define-key map (kbd "C-<") #'gbl-simple-escape-url)
;;     (define-key map (kbd "C-'") #'gbl-simple-insert-pair)
;;     (define-key map (kbd "M-'") #'gbl-simple-insert-pair)
;;     (define-key map (kbd "M-\\") #'gbl-simple-delete-pair-dwim)
;;     (define-key map (kbd "M-z") #'zap-up-to-char) ; NOT `zap-to-char'
;;     (define-key map (kbd "M-Z") #'gbl-simple-zap-to-char-backward)
;;     ;; NOTE 2022-05-01: I deprecated those commands.  I don't use them
;;     ;; and they need to be reworked.
;;     ;;
;;     ;; (define-key map (kbd "C-M-;") #'gbl-simple-cite-region)
;;     ;; (define-key map (kbd "C-M-^") #'gbl-simple-insert-undercaret)
;;     (define-key map (kbd "<C-M-backspace>") #'backward-kill-sexp)
;;     (define-key map (kbd "M-c") #'capitalize-dwim)
;;     (define-key map (kbd "M-l") #'downcase-dwim)        ; "lower" case
;;     (define-key map (kbd "M-u") #'upcase-dwim)
;;     ;; Commands for object transposition
;;     (define-key map (kbd "C-t") #'gbl-simple-transpose-chars)
;;     (define-key map (kbd "C-x C-t") #'gbl-simple-transpose-lines)
;;     (define-key map (kbd "C-S-t") #'gbl-simple-transpose-paragraphs)
;;     (define-key map (kbd "C-x M-t") #'gbl-simple-transpose-sentences)
;;     (define-key map (kbd "C-M-t") #'gbl-simple-transpose-sexps)
;;     (define-key map (kbd "M-t") #'gbl-simple-transpose-words)
;;     ;; Commands for marking objects
;;     (define-key map (kbd "M-@") #'gbl-simple-mark-word)       ; replaces `mark-word'
;;     (define-key map (kbd "C-M-SPC") #'gbl-simple-mark-construct-dwim)
;;     (define-key map (kbd "C-M-d") #'gbl-simple-downward-list)
;;     ;; Commands for paragraphs
;;     (define-key map (kbd "M-Q") #'gbl-simple-unfill-region-or-paragraph)
;;     ;; Commands for windows and pages
;;     (define-key map (kbd "C-x n k") #'gbl-simple-delete-page-delimiters)
;;     (define-key map (kbd "C-x M") #'gbl-simple-monocle)
;;     ;; NOTE 2022-03-02: Elsewhere I provide my `logos.el' package which
;;     ;; has the functionality of these three commands.
;;     ;;
;;     ;; (define-key map [remap narrow-to-region] #'gbl-simple-narrow-dwim)
;;     ;; (define-key map [remap forward-page] #'gbl-simple-forward-page-dwim)
;;     ;; (define-key map [remap backward-page] #'gbl-simple-backward-page-dwim)
;;     ;;
;;     ;; Commands for buffers
;;     (define-key map (kbd "M-=") #'count-words)
;;     (define-key map (kbd "<C-f2>") #'gbl-simple-rename-file-and-buffer)
;;     (define-key map (kbd "C-x K") #'gbl-simple-kill-buffer-current)
;;     (define-key map (kbd "M-s b") #'gbl-simple-buffers-major-mode)
;;     (define-key map (kbd "M-s v") #'gbl-simple-buffers-vc-root)))
                                        ;

(provide 'gbl-emacs-essentials)

;;; Plain text (text-mode with prot-text.el)
(prot-emacs-builtin-package 'text-mode)

(prot-emacs-builtin-package 'prot-text
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))
  (define-key text-mode-map (kbd "<M-return>") #'prot-text-insert-heading)
  (define-key org-mode-map (kbd "<M-return>") #'org-meta-return) ; don't override M-RET here
  (define-key org-mode-map (kbd "M-;") nil))

;;; Markdown (markdown-mode)
(prot-emacs-elpa-package 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))

(prot-emacs-elpa-package 'systemd)

;;; YAML (yaml-mode)
(prot-emacs-elpa-package 'yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;;; CSS (css-mode)
(prot-emacs-builtin-package 'css-mode
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-fontify-colors nil))

;;; Shell scripts (sh-mode)
(prot-emacs-builtin-package 'sh-script
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

;;; Paragraphs and fill-mode (prot-fill.el)
(prot-emacs-builtin-package 'prot-fill
  (setq prot-fill-default-column 72)
  (setq prot-fill-prog-mode-column 72)  ; Set this to another value if you want
  ;; Those variables come from various sources, though they feel part of the
  ;; same conceptual framework.
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)
  (prot-fill-fill-mode 1)
  (add-hook 'after-init-hook #'column-number-mode))

;;; Comments (newcomment.el and prot-comment.el)
(prot-emacs-builtin-package 'newcomment
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (let ((map global-map))
    (define-key map (kbd "C-:") #'comment-kill)         ; C-S-;
    (define-key map (kbd "M-;") #'comment-indent)))

(prot-emacs-builtin-package 'prot-comment
  (setq prot-comment-comment-keywords
        '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")
  (let ((map global-map))
    (define-key map (kbd "C-;") #'prot-comment-comment-dwim)
    (define-key map (kbd "C-x C-;") #'prot-comment-timestamp-keyword)))

;;; Configure 'electric' behaviour
(prot-emacs-builtin-package 'electric
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

;;; Parentheses (show-paren-mode)
(prot-emacs-builtin-package 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'child-frame) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode))

;;; Tabs, indentation, and the TAB key
(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Flyspell and prot-spell.el (spell check)
(prot-emacs-builtin-package 'flyspell
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil))

(prot-emacs-builtin-package 'prot-spell
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))
  (let ((map global-map))
    (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
    (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)))

;;; Flymake
(prot-emacs-builtin-package 'flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))

  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c ! s") #'flymake-start)
    (define-key map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics) ; Emacs28
    (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error)))

;;; Flymake + Shellcheck
(prot-emacs-elpa-package 'flymake-shellcheck
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;; Flymake + Proselint
(prot-emacs-elpa-package 'flymake-proselint
  (add-hook 'text-mode-hook #'flymake-proselint-setup))

;;; Elisp packaging requirements
(prot-emacs-elpa-package 'package-lint-flymake
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; Eldoc (elisp live documentation feedback)
(prot-emacs-builtin-package 'eldoc
  (global-eldoc-mode 1))

;;; Handle performance for very long lines (so-long.el)
(prot-emacs-builtin-package 'so-long
  (global-so-long-mode 1))

(provide 'gbl-emacs-langs)

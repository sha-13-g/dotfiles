;;; Plain text (text-mode with prot-text.el)

;;(use-package text-mode)

;; (use-package prot-text
;;   :ensure nil
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))
;;   (define-key text-mode-map (kbd "<M-return>") #'prot-text-insert-heading)
;;   (define-key org-mode-map (kbd "<M-return>") #'org-meta-return) ; don't override M-RET here
;;   (define-key org-mode-map (kbd "M-;") nil))

;;; Markdown (markdown-mode)
;; (use-package markdown-mode
;;   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;   (setq markdown-fontify-code-blocks-natively t))

;; (use-package systemd)

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :after yasnippet)

;;; YAML (yaml-mode)
;; (use-package yaml-mode
;;   (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;;; CSS (css-mode)
(use-package css-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-fontify-colors nil))

;;; Shell scripts (sh-mode)
(use-package sh-script
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

;;; Paragraphs and fill-mode (prot-fill.el)
;; (use-package prot-fill
;;   :ensure nil
;;   :config
;;   (setq prot-fill-default-column 72)
;;   (setq prot-fill-prog-mode-column 72)  ; Set this to another value if you want
;;   ;; Those variables come from various sources, though they feel part of the
;;   ;; same conceptual framework.
;;   (setq sentence-end-double-space t)
;;   (setq sentence-end-without-period nil)
;;   (setq colon-double-space nil)
;;   (setq use-hard-newlines nil)
;;   (setq adaptive-fill-mode t)
;;   (prot-fill-fill-mode 1)
;;   (add-hook 'after-init-hook #'column-number-mode))

;;; Comments (newcomment.el and prot-comment.el)
;; (use-package newcomment                 ;
;;   :ensure nil
;;   :config
;;   (setq comment-empty-lines t)
;;   (setq comment-fill-column nil)
;;   (setq comment-multi-line t)
;;   (setq comment-style 'multi-line)
;;   (let ((map global-map))
;;     (define-key map (kbd "C-:") #'comment-kill)         ; C-S-;
    ;; (define-key map (kbd "M-;") #'comment-indent)))

;; (use-package prot-comment
;;   :ensure nil
;;   :config
;;   (setq prot-comment-comment-keywords
;;         '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
;;   (setq prot-comment-timestamp-format-concise "%F")
;;   (setq prot-comment-timestamp-format-verbose "%F %T %z")
;;   (let ((map global-map))
;;     (define-key map (kbd "C-;") #'prot-comment-comment-dwim)
;;     (define-key map (kbd "C-x C-;") #'prot-comment-timestamp-keyword)))

;;; Configure 'electric' behaviour
(use-package electric
  :ensure nil
  :config
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
(use-package paren
  :ensure nil
  :config
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
(use-package flyspell
  :ensure nil
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil))

(use-package prot-spell
  :ensure nil
  :config
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))
  (let ((map global-map))
    (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
    (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)))

;;; Flymake
(use-package flymake
  :ensure nil
  :config
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
;;(use-package flymake-shellcheck                      ;;
;;  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)) ;;
                                        ;
;;; Flymake + Proselint
;; (use-package flymake-proselint
  ;; (add-hook 'text-mode-hook #'flymake-proselint-setup))

;;; Elisp packaging requirements
;; (use-package package-lint-flymake
  ;; (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; Eldoc (elisp live documentation feedback)
(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode 1))

;;; Handle performance for very long lines (so-long.el)
(use-package so-long
  :ensure nil
  :config
  (global-so-long-mode 1))

(use-package python-pytest
  :bind (:map python-mode-map
			  ("C-c C-y" . python-pytest))
  :custom
  (python-pytest-confirm t))

(use-package emmet-mode
  :diminish)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
		 ("\\.phtml\\'" . web-mode)
		 ("\\.tpl\\.php\\'" . web-mode)
		 ("\\.ejs?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist
		'(("django" . "focus/.*\\.html\\'")
		  ("ejs" . "\\.ejs\\."))))

(use-package web-beautify
  :bind
  (:map web-mode-map
		("C-c b" . web-beautify-html))
  (:map js2-mode-map
		("C-c b" . web-beautify-js)))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Debugging with Dap-Mode
(use-package dap-mode
  :diminish)

(use-package js2-mode
  :hook ((js2-mode . js2-imenu-extras-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.js[ms]\\'" . js2-mode)))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (js2-mode . eglot-ensure)))


;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (python-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)


;; (use-package lsp-jedi
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))
;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode
;;   :diminish)

(provide 'gbl-emacs-langs)

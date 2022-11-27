;;; Plain text (text-mode with gbl-text.el)

;;(use-package text-mode)

;; (use-package gbl-text
;;   :ensure nil
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))
;;   (define-key text-mode-map (kbd "<M-return>") #'gbl-text-insert-heading)
;;   (define-key org-mode-map (kbd "<M-return>") #'org-meta-return) ; don't override M-RET here
;;   (define-key org-mode-map (kbd "M-;") nil))

;;; Markdown (markdown-mode)
;; (use-package markdown-mode
;;   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;   (setq markdown-fontify-code-blocks-natively t))

;; (use-package systemd)

(use-package lorem-ipsum
  :config
  (lorem-ipsum-use-default-bindings))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :bind (:map yas-minor-mode-map
              ("C-j" . yas-expand)))

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

;;; Paragraphs and fill-mode (gbl-fill.el)
;; (use-package gbl-fill
;;   :ensure nil
;;   :config
;;   (setq gbl-fill-default-column 72)
;;   (setq gbl-fill-prog-mode-column 72)  ; Set this to another value if you want
;;   ;; Those variables come from various sources, though they feel part of the
;;   ;; same conceptual framework.
;;   (setq sentence-end-double-space t)
;;   (setq sentence-end-without-period nil)
;;   (setq colon-double-space nil)
;;   (setq use-hard-newlines nil)
;;   (setq adaptive-fill-mode t)
;;   (gbl-fill-fill-mode 1)
;;   (add-hook 'after-init-hook #'column-number-mode))

;;; Comments (newcomment.el and gbl-comment.el)
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

;; (use-package gbl-comment
;;   :ensure nil
;;   :config
;;   (setq gbl-comment-comment-keywords
;;         '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
;;   (setq gbl-comment-timestamp-format-concise "%F")
;;   (setq gbl-comment-timestamp-format-verbose "%F %T %z")
;;   (let ((map global-map))
;;     (define-key map (kbd "C-;") #'gbl-comment-comment-dwim)
;;     (define-key map (kbd "C-x C-;") #'gbl-comment-timestamp-keyword)))

;;; Configure 'electric' behaviour

(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/downloads/plantuml.jar")))

;; (use-package electric
;;   :ensure nil
;;   :config
;;   (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
;;   (setq electric-pair-preserve-balance t)
;;   (setq electric-pair-pairs
;;         '((8216 . 8217)
;;           (8220 . 8221)
;;           (171 . 187)))
;;   (setq electric-pair-skip-self 'electric-pair-default-skip-self)
;;   (setq electric-pair-skip-whitespace nil)
;;   (setq electric-pair-skip-whitespace-chars '(9 10 32))
;;   (setq electric-quote-context-sensitive t)
;;   (setq electric-quote-paragraph t)
;;   (setq electric-quote-string nil)
;;   (setq electric-quote-replace-double t)
;;   (electric-pair-mode -1)
;;   (electric-quote-mode -1)
;;   ;; I don't like auto indents in Org and related.  They are okay for
;;   ;; programming.
;;   (electric-indent-mode -1)
;;   (add-hook 'prog-mode-hook #'electric-indent-local-mode))
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

;;; Flyspell and gbl-spell.el (spell check)
;; (use-package flyspell
;;   :ensure nil
;;   :config
;;   (setq flyspell-issue-message-flag nil)
;;   (setq flyspell-issue-welcome-flag nil)
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-dictionary "en_GB")
;;   (define-key flyspell-mode-map (kbd "C-;") nil))

;; (use-package gbl-spell
;;   :ensure nil
;;   :config
;;   (setq gbl-spell-dictionaries
;;         '(("EN English" . "en")
;;           ("EL Ελληνικά" . "el")
;;           ("FR Français" . "fr")
;;           ("ES Espanõl" . "es")))
;;   (let ((map global-map))
;;     (define-key map (kbd "M-$") #'gbl-spell-spell-dwim)
;;     (define-key map (kbd "C-M-$") #'gbl-spell-change-dictionary)))

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
  :diminish
  :bind (:map emmet-mode-keymap
              ("C-," . emmet-expand-line)))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(use-package web-mode
  :hook '((web-mode . eglot-ensure))
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
  :hook ((js2-mode . js2-imenu-extras-mode)))

(add-to-list 'auto-mode-alist '("\\.js[ms]?\\'" . js2-mode))

(add-hook 'js2-mode-hook
       (lambda ()
	 (unless (or (file-exists-p "makefile")
		     (file-exists-p "Makefile"))
           (setq-local compile-command
		(concat "node "
			(if buffer-file-name
			  (shell-quote-argument (buffer-file-name))))))))

(add-hook 'python-mode-hook
       (lambda ()
	 (unless (or (file-exists-p "makefile")
		     (file-exists-p "Makefile"))
           (setq-local compile-command
		(concat "python "
			(if buffer-file-name
			  (shell-quote-argument (buffer-file-name))))))))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (js2-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(web-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(css-mode . ("vscode-css-language-server" "--stdio"))))

(provide 'gbl-emacs-langs)

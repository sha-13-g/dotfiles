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
  :hook '((css-mode . eglot-ensure))
  :config
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-fontify-colors nil))

;;; Shell scripts (sh-mode)
(use-package sh-script
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))


(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/downloads/plantuml.jar")))

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

;;; Flymake
(use-package flymake
  :ensure nil
  :hook ((prog-mode . flymake-mode)
         (flymake-mode . flymake-start)) 
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

(use-package typescript-mode
  :hook ((typescript-mode . eglot-ensure))
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

(use-package tide
 :ensure t
 :after (typescript-mode company flycheck)
 :hook ((typescript-mode . tide-setup)
        (typescript-mode . tide-hl-identifier-mode)
        (before-save . tide-format-before-save)))

(use-package js2-mode
  :hook ((js2-mode . eglot-ensure)
         (js2-mode . js2-imenu-extras-mode)
         (js2-mode . (lambda ()
                       (unless (or (file-exists-p "makefile")
                                   (file-exists-p "Makefile"))
                         (setq-local compile-command
                                     (concat "node "
                                             (if buffer-file-name
                                                 (shell-quote-argument (buffer-file-name)))))))))
  :config
  (add-to-list 'auto-mode-alist '("\\.js[ms]?\\'" . js2-mode)))

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

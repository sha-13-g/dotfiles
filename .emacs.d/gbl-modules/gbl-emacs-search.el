;;; Isearch, occur, grep, and extras (gbl-search.el)
(use-package isearch
  :ensure nil
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; These variables are from Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

(use-package replace
  :ensure nil
  :config
  (setq list-matching-lines-jump-to-current-line nil)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'gbl-common-truncate-lines-silently) ; from `gbl-common.el'
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(use-package grep)

(use-package gbl-search
  :ensure nil
  :config
  (setq gbl-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq gbl-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (let ((map global-map))
    (define-key map (kbd "M-s %") #'gbl-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-%") #'gbl-search-replace-markup) ; see `gbl-search-markup-replacements'
    (define-key map (kbd "M-s M-<") #'gbl-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s M->") #'gbl-search-isearch-end-of-buffer)
    (define-key map (kbd "M-s g") #'gbl-search-grep)
    (define-key map (kbd "M-s u") #'gbl-search-occur-urls)
    (define-key map (kbd "M-s t") #'gbl-search-occur-todo-keywords)
    (define-key map (kbd "M-s M-t") #'gbl-search-grep-todo-keywords) ; With C-u it runs `gbl-search-git-grep-todo-keywords'
    (define-key map (kbd "M-s M-o") #'gbl-search-occur-outline)
    (define-key map (kbd "M-s M-u") #'gbl-search-occur-browse-url))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'gbl-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'gbl-search-isearch-repeat-forward)
    (define-key map (kbd "<backspace>") #'gbl-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'gbl-search-isearch-other-end)))

;;; Test regular expressions (re-builder)
(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'read))

;;; wgrep (writable grep)
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-c") #'wgrep-finish-edit)))

;;; Cross-references (xref.el)
(use-package xref
  :ensure nil
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'grep))

;;; Built-in bookmarking framework (bookmark.el and gbl-bookmark.el)
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode))

(use-package gbl-bookmark
  :ensure nil
  :config
  (gbl-bookmark-extra-keywords 1))

(provide 'gbl-emacs-search)

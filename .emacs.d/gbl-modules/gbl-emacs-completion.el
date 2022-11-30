;;; Orderless completion style (and gbl-orderless.el)
(use-package company
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection)
		("C-j" . company-select-next-or-abort)
		("C-k" . company-select-previous-or-abort))
  ;; (:map lsp-mode-map
  ;; 		("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-show-quick-access t)
  :config
  (global-company-mode))

;; Cleaner Aesthetic with Company-box
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . vertico-next))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :after vertico
  :init
  ;; configure a custom style dispatcher (see the consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
		orderless-component-separator "[ &]"))

(use-package consult
  :after vertico
  :bind (;; C-c bindings (mode-specifiC-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c M" . consult-minor-mode-menu)
         ("C-c o" . consult-outline)
         ("C-c i" . consult-imenu)
         ("C-c f" . consult-flymake)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; ("C-s" . consult-line)
         ("C-\\" . consult-line)

         :map isearch-mode-map
         ;; ("C-s" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s l" . consult-line-multi)            ;; needed by consult-line to detect isearch

         :map minibuffer-local-map
         ("s-s" . consult-history))                ;; orig. previous-matching-history-element

:hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

;; (use-package vertico-posframe
;;   :init (vertico-posframe-mode 1)
;;   :config (setq vertico-posframe-parameters
;;                 '((left . 0))))


(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-:" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (describe-function-function #'helpful-callable)
  (describe-variable-function #'helpful-variable)
  :bind
  ;; ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ;; ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;(use-package gbl-orderless
;;:ensure nil)
;;  (marginalia-mode 1))

;;; Minibuffer configurations and Vertico
(use-package minibuffer
  :ensure nil
  :config
  (setq completion-styles '(basic orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;;
  ;; From the `consult' package:
  ;;
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;;
  ;; From the `embark' package:
  ;;
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq enable-recursive-minibuffers t)
  ;; Allow Emacs to resize mini windows, otherwise this does not work:
  ;;   (setq org-use-fast-todo-selection 'expert)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Also adapted from Vertico.
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple' filter ARGS."
    ;; The `error' face just makes the text red.
    (cons (concat (propertize "[CRM] " 'face 'error) (car args)) (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(use-package embark-consult)

;;; Completion for recent files and directories (gbl-recentf.el)
(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

(use-package gbl-recentf
  :ensure nil
  :config
  (add-to-list 'recentf-keep 'gbl-recentf-keep-predicate)
  (let ((map global-map))
    (define-key map (kbd "C-x C-r") #'gbl-recentf-recent-files-or-dirs)))

;;; Corfu (in-buffer completion popup)
;; (use-package corfu
;;   :config
;;   ;; (dolist (mode '( message-mode-hook text-mode-hook prog-mode-hook
;;   ;;                  shell-mode-hook eshell-mode-hook))
;;   ;;   (add-hook mode #'corfu-mode))
;;   (global-corfu-mode 1)
;;   (define-key corfu-map (kbd "<tab>") #'corfu-complete)

;;   ;; Adapted from Corfu's manual.
;;   (defun contrib/corfu-enable-always-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if Vertico is not active.
;; Useful for prompts such as `eval-expression' and `shell-command'."
;;     (unless (bound-and-true-p vertico--input)
;;       (corfu-mode 1)))

;;   (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;;; CAPE (extra completion-at-point backends)
(use-package cape
  :config
  (setq cape-dabbrev-min-length 3)
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

;;; Template-based in-buffer completion (tempel.el)
(use-package tempel
  :config

  ;; Setup completion at point
  (defun contrib/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'contrib/tempel-setup-capf))

  (let ((map global-map))
    (define-key map (kbd "M-+") #'tempel-complete) ; Alternative: `tempel-expand'
    (define-key map (kbd "M-*") #'tempel-insert))
  (let ((map tempel-map))
    (define-key map (kbd "RET") #'tempel-done)
    (define-key map (kbd "C-p") #'tempel-previous)
    (define-key map (kbd "C-n") #'tempel-next)))

;;; Enhance command-line completion (pcmpl-args)
(use-package pcmpl-args)

;;; Dabbrev (dynamic word completion)
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (let ((map global-map))
    (define-key map (kbd "M-/") #'dabbrev-expand)
    (define-key map (kbd "C-x M-/") #'dabbrev-completion)))

;;; Abbreviations or Abbrevs
(use-package abbrev
  :ensure nil
  :config
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil)

  (let ((table global-abbrev-table))
    (define-abbrev table "meweb" "https://protesilaos.com")
    (define-abbrev table "megit" "https://git.sr.ht/~protesilaos")
    ;; to speed up technical writing about Emacs in Greek (I wrote the
    ;; Emacs TUTORIAL in Greek)
    (define-abbrev table "εμαψσ" "Emacs")
    (define-abbrev table "βθφφερ" "αποσβεστήρας")
    (define-abbrev table "μινιβθφφερ" "μικροαποσβεστήρας"))

  (let ((table text-mode-abbrev-table))
    (define-abbrev table "latex" "LaTeX")
    (define-abbrev table "javascript" "JavaScript")
    (define-abbrev table "github" "GitHub")
    (define-abbrev table "gitlab" "GitLab")
    (define-abbrev table "sourcehut" "SourceHut")
    (define-abbrev table "libreplanet" "LibrePlanet")
    (define-abbrev table "emacsconf" "EmacsConf")
    (define-abbrev table "auctex" "AUCTeX")
    (define-abbrev table "Emacs27" "Emacs 27")
    (define-abbrev table "Emacs28" "Emacs 28")
    (define-abbrev table "Emacs29" "Emacs 29")
    (define-abbrev table "asciidoc" "AsciiDoc"))

  (with-eval-after-load 'message
    (let ((table message-mode-abbrev-table)
          (name "Protesilaos (or simply \"Prot\")"))
      (define-abbrev table "bestregards" (format "Best regards,\n%s" name))
      (define-abbrev table "allthebest" (format "All the best,\n%s" name))
      (define-abbrev table "abest" "All the best,\nProt")
      (define-abbrev table "bregards" "Best regards,\nProt")))

  (let ((map global-map))
    (define-key map (kbd "C-x a e") #'expand-abbrev) ; default, just here for visibility
    (define-key map (kbd "C-x a u") #'unexpand-abbrev))

  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  (dolist (hook '(text-mode-hook git-commit-mode-hook))
    (add-hook hook #'abbrev-mode)))

(provide 'gbl-emacs-completion)

;; Setting up Package.el to work with MELP
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
		 				 ("elpa-devel" . "https://elpa.gnu.org/devel/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("gnu" . "https://elpa.gnu.org/packages/")))

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;;Variable definition
    (defvar gbl/leader "s-SPC")
    (defvar gbl/super-leader "s-a")
    (defvar gbl/frame-transparency-v '(90 . 90))


(set-frame-parameter (selected-frame) 'alpha gbl/frame-transparency-v)
(add-to-list 'default-frame-alist `(alpha . ,gbl/frame-transparency-v))


(dolist (c '( narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Installing and Configuring use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t) ; all use-package statements will include :ensure t by default

(dolist (mode '(term-mode-hook
				vterm-mode-hook
				treemacs-mode-hook
				;elfeed-search-mode-hook
				elfeed-show-mode-hook
				eww-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Defining some fonts
(set-face-attribute 'default nil
					:font "Source Code Pro 10" ; Monospaced coding font
					:weight 'medium)
(set-face-attribute 'variable-pitch nil ; Non tech font
					:font "Cantarell"
					:height 110)
(set-face-attribute 'fixed-pitch nil
					:font "Source Code Pro 10"
					:weight 'medium)


(set-face-attribute 'font-lock-comment-face nil
					:slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
					:weight 'medium)

;; Extra font settings
(setq-default line-spacing 0.03)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))

(add-hook 'prog-mode-hook #'electric-pair-mode)
(setq electric-pair-preserve-balance nil)

;; Setting Transparency
;; Enabling usefull modes
(delete-selection-mode 1) ; Delete existing text when inserting text
(global-subword-mode 1)

;; Keeping init.el clean
(setq custom-file (concat user-emacs-directory "/custom.el")) ; Add custom set variables to a seperate file

;; Creating smooth scrolling
(setq scroll-conservativley 1000
	  scroll-margin 4
	  scroll-step 1
	  mouse-wheel-scroll-amount '(6 ((shift) . 1)) ; Use shift for finer scroll
	  mouse-wheel-progressive-speed nil)

(setq redisplay-dont-pause t)
(setq fast-but-imprecise-scrolling nil ; Turn off, always
	  ;jit-lock-defer-time 0.01
	  jit-lock-defer-time 0)


;; (require 'eaf)

;; (use-package eaf
;;   :ensure nil
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :init
;;   (require 'eaf-browser)
;;   ;; (require 'eaf-pdf-viewer)
;;   ;; (require 'eaf-camera)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding))
;;;; unbind, see more in the Wiki
;; Hydra allows for good keybindings for repetitive tasks

;; (use-package keycast
;;   :config
;;   (keycast-mode))

;;;; load function file
(dolist (path '("gbl-lisp" "gbl-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)));;(load-file (concat user-emacs-directory "modules/epubmode.el"))

;; (require 'jsshell-bundle)
;; (require 'js-comint)

(require 'gbl-emacs-utils)
(require 'gbl-emacs-conveniences)
(require 'gbl-emacs-completion)
(require 'gbl-emacs-maps)
(require 'gbl-emacs-window)
(require 'gbl-emacs-desktop)
(require 'gbl-emacs-evil)
(require 'gbl-emacs-modeline)
(require 'gbl-emacs-dired)
(require 'gbl-emacs-langs)
(require 'gbl-emacs-org)
(require 'gbl-emacs-magit)
;; (require 'gbl-emacs-desktop)

;; setting up auto-package update so that packages are updated automatically
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7) ; Every seven days
  (auto-package-update-prompt-before-update t) ; Ask permission first
  (auto-package-update-hide-results t)) ; At 9:00AM

;; Real auto-save feature

(use-package real-auto-save
  :config (setq real-auto-save-interval 50)
  :hook (prog-mode . real-auto-save-mode)
  :hook (org-mode . real-auto-save-mode))

;; Using 'doom-themes' as a theme repository
(use-package doom-themes
  :config
  ;; Disable italics and bold
  (setq doom-themes-enable-bold nil
		doom-themes-enable-italic nil)

  ;; Function for switching to light theme:
  (defun gbl/load-dark-theme ()
	(interactive)
	
	(disable-theme 'doom-solarized-light)
	(load-theme 'doom-dracula t))

  ;; Function for switching to light theme
  (defun gbl/load-light-theme ()
	(interactive)

	(disable-theme 'doom-dracula)
	(load-theme 'doom-solarized-light t)
	(setq-default input-block "#F9F2D9"))
  (gbl/load-dark-theme))

;; Adding upport for emojis and icons
(use-package all-the-icons)

(use-package ef-themes)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package afternoon-theme
  :diminish t)

(use-package flatui-theme
  :diminish t)

(use-package vampyricdark-theme
  :diminish t)

(use-package catppuccin-theme
 :config
 (setq catppuccin-height-title1 1.5))
;;;; Completion

;; Installing vertico
;; vertico is an autocompletion engine for Emacs, vertico-rich allows suggestions to have descriptions

;; Using Winum to label open windows
(use-package winum
  :config (winum-mode))

(use-package diminish
  :diminish t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

(use-package minions
  :after doom-modeline)

(use-package rich-minority
  :diminish t)

;; File manager for emacs, incuded package within emacs 27
;; (use-package dired
;;   :ensure nil
;;   :hook ((dired-mode . dired-hide-details-mode)
;; 		 (dired-mode . hl-line-mode))
;;   :commands (dired dired-jumo)
;;   :bind (("C-x C-j" . dired-jump))
;;   :custom
;;   ((dired-listing-switched "-AGFhlv --group-directories-first --time-style=long-iso")
;;   (dired-recursive-copies 'always)
;;   (dired-recursive-deletes 'always)
;;   (delete-by-moving-to-trash t)
;;   (dired-dwim-target t)
;;   (dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
;;   (dired-make-directory-clickable t) ; Emacs 29.1
;;   (dired-free-space nil) ; Emacs 29.1
;;   (dired-mouse-drag-files t))
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map ; VIM keybindings for navigating directories
;; 	"h" 'dired-up-directory
;; 	"l" 'dired-find-file))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")) ; "powershell.exe" for windows

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


;; Function to configure eshell when an instance of it is brought into existance
(defun configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history) 
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshellistory-size 5000
		eshell-buffer-maxiumum-lines 5000
		eshell-scrollto-bottom-on-input t))

; Eshell has a git prompt, showing git infomation when in a git initialized directory
(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun gbl/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun gbl/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun gbl/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'gbl/map-line-to-status-char status-lines)))))

(defun gbl/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun gbl/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (gbl/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (gbl/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

;; (unless gbl/is-termux
;;   (add-hook 'eshell-banner-load-hook
;;             (lambda ()
;;                (setq eshell-banner-message
;;                      (concat "\n" (propertize " " 'display (create-image "~/.dotfiles/.emacs.d/images/flux_banner.png" 'png nil :scale 0.2 :align-to "center")) "\n\n")))))

(defun gbl/eshell-configure ()
  ;; Make sure magit is loaded
  (require 'magit)

  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  (use-package xterm-color
	:diminish t)

  ;; (push 'eshell-tramp eshell-modules-list)
  ;; (push 'xterm-color-filter eshell-preoutput-filter-functions)
  ;; (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'gbl/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

;(setup eshell
  ;(add-hook 'eshell-first-time-mode-hook #'gbl/eshell-configure)
  ;(setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        ;eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))


(use-package eshell-z
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
		 (eshell-z-change-dir-hook . (lambda () (eshell/pushd (eshell/pwd))))))

;; (setup (:pkg exec-path-from-shell)
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

(use-package eshell-git-prompt
  :diminish t)
(use-package eshell-toggle
  :diminish t)
(use-package eshell
  :hook (eshell-first-time-mode . configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline)) ; Powerline theme selected

(setq eshell-destroy-buffer-when-process-dies t
	  eshell-visual-commands '("bash" "htop" "pftech"))

;; Vterm provides the best terminal emulation within emacs
(use-package vterm
  :commands vterm
  :config
  (setq shell-file-name "/bin/bash" ; Bash by default
		vterm-max-scrollback 5000))

; Elfeed, RSS Newsreader for emacs
(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #fff :weight bold"
		elfeed-feeds (quote
					  (("https://www.reddit.com/r/linux.rss" reddit linux)
					   ("https://www.reddit.com/r/emacs.rss" reddit emacs)
					   ("https://opensource.com/feed" opensource linux)
					   ("https://linux.softpedia.com/backend.xml" softpedia linux)
					   ("https://www.computerword.com/index.rss" computerworld linux)
					   ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
					   ("https://betanews.com/feed" betanews linux)
					   ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))

(use-package elfeed-goodies ; Additional features for elfeed
  :init
  (elfeed-goodies/setup)
  :config (setq elfeed-goodies/entry-pane-size 0.5))

(add-hook 'elfeed-search-mode-hook 'toggle-truncate-lines) ; Article names often reach past 1 length/

;; Vim keybindings for elfeed
;; (evil-define-key 'normal elfeed-show-mode-map
;;   (kbd "J") 'elfeed-goodies/split-show-next
;;   (kbd "K") 'elfeed-goodies/split-show-prev)
;; (evil-define-key 'normal elfeed-search-mode-map
;;   (kbd "J") 'elfeed-goodies/split-show-next
;;   (kbd "K") 'elfeed-goodies/split-show-prev)

;; Configuring EWW, emacs web wowser
(setq
 browser-url-browser-function 'eww-browser-url
 shr-use-fonts nil
 shr-use-colors nil
 shr-indentation 2
 shr-width 70
 eww-search-prefix "https://duckduckgo.com/html/?q=") ; Wiby.me as the default search engine

;; General.el keybindings for opening applications

;; Breader Headcrumb
(defun gbl/lsp-mode-setup ()
  (setq lsp-headline-breadcrumb-segmments '(path-up-to-project file symbols))
  (lsp-headline-breadcrumb-mode))

;;;; Languages Servers

;; (use-package blacken
;; :diminish t)
;; (use-package py-autopep8
;; :diminish t)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; (use-package elpy
;;   :config
;;   (elpy-enable))
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (js-mode . lsp-deferred)
;;          (python-mode . lsp-deferred)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands (lsp lsp-deferred))



;; (use-package lsp-ui
;;    :ensure t
;;    :config
;;    (setq lsp-ui-sideline-ignore-duplicate t)
;;    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))
;; (require 'eglot)

;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'typescript-mode-hook 'eglot-ensure)
;; (add-hook 'js-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'html-mode-hook 'eglot-ensure)
;; (add-hook 'css-mode-hook 'eglot-ensure)

;;; Org Mode
;; (defun org-mode-setup ()
;;   (org-indent-mode)
;;   (variable-pitch-mode 1)
;;   (auto-fill-mode 0)
;;   (visual-line-mode 1)
;;   (setq evil-auto-indent nil))

;; (use-package org
;;   :hook (org-mode . org-mode-setup)
;;   :config
;;   (setq org-ellipsis " ▼ " 
;; 		org-hide-emphasis-markers t))

;; (setq org-directory "~/documents/org/"
;; 	  org-agenda-files '("~/documents/org/") ; NOTE: This is a list, more files can be added later
;; 	  org-default-notes-file (expand-file-name "Notes.org" org-directory))

;; ;; This overides GNU default
;; (setq org-link-abbrev-alist
;; 	  '(("google" . "https://www.google.com/search?q=")
;; 		("arch-wiki" . "https://wiki.archlinux.org/index.php")
;; 		("freebsd-forum" . "https://forums.freebsd.org") ; I don't know if this one works.
;; 		("duckduckgo" . "https://duckduckgo.com/?q=")
;; 		("wiby" . "https://wiby.me/?q=")
;; 		("wikipedia" . "https://en.wikipedia.org/wiki")
;; 		("reddit" . "https://old.reddit.com/r/")))

;; (setq org-todo-keywords
;; 	  '((sequence
;; 		 "TODO(t)"
;; 		 "NEXT(n)"
;; 		 "PROJ(p)"
;; 		 "HOWEWORK(h)"
;; 		 "CHOICE(C)"
;; 		 "BOOK(b)"
;; 		 "WAIT(w)"
;; 		 "|"
;; 		 "DONE(d)"
;; 		 "CANCELLED(c)" )))

;; (setq org-tag-alist
;;     '((:startgroup)
;;        ; put mutually exclusive tags here
;;        (:endgroup)
;;        ("@errand" . ?e)
;;        ("@home" . ?h)
;;        ("@work" . ?w)
;;        ("agenda" . ?a)
;;        ("planning" . ?p)
;;        ("publish" . ?p)
;;        ("batch" . ?b)
;;        ("note" . ?n)
;;        ("idea" . ?i)))

;; (use-package org-tempo
;;   :ensure nil)

;; ;; Make sure that babel code blocks evaluate code correctly
;; (setq org-src-fontify-natively t
;; 	  org-src-tab-acts-natively t
;; 	  org-confirm-babel-evaluate nil
;; 	  org-edit-src-content-indentation 0)

;; (setq org-blank-before-new-entry (quote ((heading . nil)
;; 										 (plain-list-item . nil))))

;; ;; Make sure that python code can be executed inside a babel code block
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)
;;    (plantuml . t)))

;; ;; Use cooler and more diffrenciated bullets
;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode))

;; ;; Configuration of Font faces and sizes within org documents
;; (with-eval-after-load 'org-faces ; Must be wrapped in =with-eval-after-load=
;;  ;; Diffrenciate headers based on size
;;  (dolist (face '((org-level-1 . 1.2)
;; 				  (org-level-2 . 1.1)
;; 				  (org-level-3 . 1.05)
;; 				  (org-level-4 . 1.0)
;; 				  (org-level-5 . 1.1) ; Back to normal
;; 				  (org-level-6 . 1.1)
;; 				  (org-level-7 . 1.1)
;; 				  (org-level-8 . 1.1)))
;; 	(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;;  ;; Needs fixing:
;;  ;; Choosing what elements of an org-document should be represented in what font face.
;;  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;;  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
;;  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitched-pitch))
;;  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;;  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;;  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;;  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; ;; Center org-mode documents in the center of the screen
;; (defun org-mode-visual-fill ()
;;   ; Proportions:
;;   (setq visual-fill-column-width 75
;; 		visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1)) ; Activate

;; ;; The centering of documents depends on the following package:
;; (use-package visual-fill-column
;;   :hook (org-mode . org-mode-visual-fill))

;; ;; 'Latex' from Org-mode
;; (use-package biblio
;;   :diminish) ; Quick BibTex refrences, sometimes.
;;  ; Quick BibTex refrences, sometimes.

;; (setq org-latex-listings 'minted
;; 	  org-latex-packages-alist '(("" "minted")))

;; (setq org-latex-pdf-process
;; 	  '("pdflatex -interaction nonstopmode -output-directory %o %f"))

;; Emacs Mail
;(use-package mu4e
;  :ensure nil
;  :load-path /usr/share/emacs/site-lisp/mu4e/
;  :defer 20
;  :config
;  (setq mu4e-change-filenames-when-moving t) ; Refresh mail using isync every 10 minutes
;
;  ;; Refresh mail using isync every 10 mins
;  (setq mu4e-update-interval (* 10 60)
;		mu4e-get-mail-command "mbsync 0a"
;		mu4e-maildir "~/Mail")
;  
;  (setq mu4e-drafts-folder "/[Gmail]/Drafts"
;		mu4e-sent-folder "/[Gmail]/Sent Mail"
;		mu4e-refile-folder "/[Gmail]/All Mail"
;		mu4e-trash-folder "/[Gmail]/Trash")
;
;  ;; Quick Access to the following folders:
;  (setq mu4e-maildir-shortcuts
;		'((:maildir "/Inbox"				:key ?i)
;		  (:maildir "/[Gmail]/Sent Mail"	:key ?s)
;		  (:maildir "/[Gmail]/Trash"		:key ?t)
;		  (:maildir "/[Gmail]/Drafts"		:key ?d)
;		  (:maildir "/[Gmail]/All Mail"		:key ?a)))
;
;  ;; You can create bookmarked queries:
;  (setq mu4e-bookmarks
;		'((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?i)
;		  (:name "Today's messages" :query "date:today..now" :key ?t)
;		  (:name "Last 7 days" :query "date:6d..now" :hide-unread t :key ?w)
;		  (:name "Messages with images" :query "mime:image/*" :key ?p)))
;
;  (mu4e t))
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

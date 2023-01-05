;; Setting up Package.el to work with MELP
(server-start)
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
(defvar gbl/leader-space "SPC")
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

(setq use-package-always-ensure t) ; use-package statements will include :ensure t by default
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


(use-package autothemer)

;; Extra font settings
(setq-default line-spacing 0.03)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))

(add-hook 'prog-mode-hook #'electric-pair-mode)
(setq electric-pair-preserve-balance t)

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

(require 'gbl-emacs-conveniences)
(require 'gbl-emacs-utils)
(require 'gbl-emacs-modeline)
 (require 'gbl-emacs-completion)
(require 'gbl-emacs-maps)
(require 'gbl-emacs-window)
(require 'gbl-emacs-dired)
(require 'gbl-emacs-langs)
(require 'gbl-emacs-desktop)
(require 'gbl-emacs-org)
(require 'gbl-emacs-magit)
;; (require 'gbl-emacs-write)
(require 'gbl-emacs-shell)

;; setting up auto-package update so that packages are updated automatically
;;(use-package auto-package-update
;;  :custom
;;  (auto-package-update-interval 7) ; Every seven days
;;  (auto-package-update-prompt-before-update t) ; Ask permission first
;;  (auto-package-update-hide-results t)) ; At 9:00AM

;; Real auto-save feature

;; (use-package real-auto-save
;;   :config (setq real-auto-save-interval 50)
;;   :hook (prog-mode . real-auto-save-mode)
;;   :hook (org-mode . real-auto-save-mode))

;; Using 'doom-themes' as a theme repository
;; (use-package doom-themes
;;   :config
;;   ;; Disable italics and bold
;;   (setq doom-themes-enable-bold nil
;; 		doom-themes-enable-italic nil)

  ;; Function for switching to light theme:
;;   (defun gbl/load-dark-theme ()
;; 	(interactive)
;; 	
;; 	(disable-theme 'doom-solarized-light)
;; 	(load-theme 'doom-dracula t))
;; 
;;   ;; Function for switching to light theme
;;   (defun gbl/load-light-theme ()
;; 	(interactive)
;; 
;; 	(disable-theme 'doom-dracula)
;; 	(load-theme 'doom-solarized-light t)
;; 	(setq-default input-block "#F9F2D9")))



;; Adding upport for emojis and icons
;; (use-package all-the-icons)

;;(use-package ef-themes)

;; (use-package emojify
;;  :hook (after-init . global-emojify-mode))

(use-package catppuccin-theme
  :config
  (setq catppuccin-height-title1 1.5)
  (load-theme 'catppuccin-macchiato t))
;;;; Completion

;; Installing vertico
;; vertico is an autocompletion engine for Emacs, vertico-rich allows suggestions to have descriptions

;; Using Winum to label open windows
;;(use-package winum
;;  :config (winum-mode))

(use-package diminish
  :diminish t)

;;(use-package doom-modeline
;;  :init (doom-modeline-mode 1)
;;  :custom ((doom-modeline-height 20)))

;;(use-package rich-minority
;;  :diminish t)

(use-package term
  :config
  (setq explicit-shell-file-name "bash")) ; "powershell.exe" for windows

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

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

;; Emacs Mail
;;(use-package mu4e
;;  :ensure nil
;;  :load-path /usr/share/emacs/site-lisp/mu4e/
;;  :defer 20
;;  :config
;;  (setq mu4e-change-filenames-when-moving t) ; Refresh mail using isync every 10 minutes
;;
;;  ;; Refresh mail using isync every 10 mins
;;  (setq mu4e-update-interval (* 10 60)
;;		mu4e-get-mail-command "mbsync 0a"
;;		mu4e-maildir "~/Mail")
;;  
;;  (setq mu4e-drafts-folder "/[Gmail]/Drafts"
;;		mu4e-sent-folder "/[Gmail]/Sent Mail"
;;		mu4e-refile-folder "/[Gmail]/All Mail"
;;		mu4e-trash-folder "/[Gmail]/Trash")
;;
;;  ;; Quick Access to the following folders:
;;  (setq mu4e-maildir-shortcuts
;;		'((:maildir "/Inbox"				:key ?i)
;;		  (:maildir "/[Gmail]/Sent Mail"	:key ?s)
;;		  (:maildir "/[Gmail]/Trash"		:key ?t)
;;		  (:maildir "/[Gmail]/Drafts"		:key ?d)
;;		  (:maildir "/[Gmail]/All Mail"		:key ?a)))
;;
;;  ;; You can create bookmarked queries:
;;  (setq mu4e-bookmarks
;;		'((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?i)
;;		  (:name "Today's messages" :query "date:today..now" :key ?t)
;;		  (:name "Last 7 days" :query "date:6d..now" :hide-unread t :key ?w)
;;		  (:name "Messages with images" :query "mime:image/*" :key ?p)))
;;
;;  (mu4e t))
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

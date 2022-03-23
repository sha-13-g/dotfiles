; -*- lexical-binding: t; -*-
;; (server-start)
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
 (unless (package-installed-p 'use-package)
   (package-install 'use-package))

(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)

(global-display-line-numbers-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(use-package doom-themes
 :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

;;(use-package app-launcher
;;  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ;; ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-alt-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

(use-package ivy-rich
  :init
  (ivy-rich-mode t))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-j" . persp-counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))
  
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command )
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer gbl/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer gbl/ctrl-c-keys
    :prefix "C-c"))


(defun glb/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circle-server-mode
                  circle-chat-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-respect-visual-line-mode t)
;;  (setq evil-undo-system 'undo-tree)
  :config
;;  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-p") 'package-install)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (delete 'lispy evil-collection-mode-list)
  (delete 'org-present evil-collection-mode-list)
  (evil-collection-init))


(use-package hydra)
(defhydra hydra-window-management (:timeout 4)
  "window management"
  ("c" evil-window-delete "d-c-w" )
  ("o" delete-other-windows "d-o-w")
  ("Q" evil-quit "Quit Emacs")
  
  ("h" evil-window-left "m-l")
  ("j" evil-window-down "m-d")
  ("k" evil-window-up "m-u")
  ("l" evil-window-right "m-r")

  ("H" evil-window-move-far-left "m-w-l")
  ("J" evil-window-move-very-bottom "m-w-d")
  ("K" evil-window-move-very-top "m-w-u")
  ("L" evil-window-move-far-right "m-w-r")

  ("[" evil-window-increase-height "sz-w-up")
  ("]" evil-window-decrease-height "sz-w-up")
  (">" evil-window-increase-width "sz-w-up")
  ("<" evil-window-decrease-width "sz-w-up")

  ("v" evil-window-vsplit "v-s")
  ("s" evil-window-split "h-s")
  ("q" nil  "quit" :exit t))
 
(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("a" text-scale-adjust "adjust")
  ("q" nil  "quit" :exit t))

(gbl/leader-key
  "t" '(:ignore t :which-key "toggles")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tt" '(counsel-load-theme :which-key "Choose theme")
  "w" '(hydra-window-management/body :which-key "windows"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/personal/lab")
    (setq projectile-project-search-path '("~/personal/lab")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

;; (defun load-directory (dir)
;;       (let ((load-it (lambda (f)
;; 		       (load-file (concat (file-name-as-directory dir) f)))
;; 		     ))
;; 	(mapc load-it (directory-files dir nil "\\.el$"))))
 ;; (require 'load-directory)
 ;; (load-directory "~/git_repo/evil-magit")

;; (required '("~/.emacs.d/evil-magit"))
;; (use-package evil-magit
;;   :after magit)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package paren
  :ensure t
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))


;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

;; Use nerd-commenter for comment
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)) 

(use-package perspective
  :demand t
  :bind (("C-k" . persp-switch)
         ("C-[" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

;; (setq large-file-warning-threshold nil)
;; (setq ad-redefinition-action 'accept)
;; (scroll-bar-mode -1)        ; Disable visible scrollbar
;; (tool-bar-mode -1)          ; Disable the toolbar
;; (set-fringe-mode 10)        ; Give some breathing room

;; (use-package spacegray-theme :defer t)
;; (use-package doom-themes :defer t)
;; (load-theme 'doom-palenight t)
;; (doom-themes-visual-bell-config)
;; ;; (menu-bar-mode -1)            ; Disable the menu bar
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX
;; ;; Set up the visible bell
;; ;;(setq visible-bell t)


;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; (defun gbl/replace-unicode-font-mapping (block-name old-font new-font)
;;   (let* ((block-idx (cl-position-if
;;                          (lambda (i) (string-equal (car i) block-name))
;;                          unicode-fonts-block-font-mapping))
;;          (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
;;          (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
;;     (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
;;           `(,updated-block))))

;; (use-package unicode-fonts
;;   :disabled
;;   :if (not dw/is-termux)
;;   :custom
;;   (unicode-fonts-skip-font-groups '(low-quality-glyphs))
;;   :config
;;   ;; Fix the font mappings to use the right emoji font
;;   (mapcar
;;     (lambda (block-name)
;;       (gbl/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
;;     '("Dingbats"
;;       "Emoticons"
;;       "Miscellaneous Symbols and Pictographs"
;;       "Transport and Map Symbols"))
;;   (unicode-fonts-setup))


;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))



;; (use-package ws-butler
;;   :hook ((text-mode . ws-butler-mode)
;;          (prog-mode . ws-butler-mode)))

;; (use-package parinfer
;;   :ensure t
;;   :disabled
;;   :hook ((clojure-mode . parinfer-mode)
;;          (emacs-lisp-mode . parinfer-mode)
;;          (common-lisp-mode . parinfer-mode)
;;          (scheme-mode . parinfer-mode)
;;          (lisp-mode . parinfer-mode))
;;   :config
;;   (setq parinfer-extensions
;;       '(defaults       ; should be included.
;;         pretty-parens  ; different paren styles for different modes.
;;         evil           ; If you use Evil.
;;         smart-tab ;;
;;         smart-yank)))  ; Yank behavior depend on mode.
;; ;; (use-package origami
;; ;;   :hook (yaml-mode . origami-mode))

;; ;; (use-package dotcrafter
;; ;;   :straight '(dotcrafter :host github
;; ;;                           :repo "daviwil/dotcrafter.el"
;; ;;                           :branch "main")
;; ;;   :custom
;; ;;   (dotcrafter-org-files '("Emacs.org"
;; ;;                           "Desktop.org"
;; ;;                           "Systems.org"
;; ;;                           "Mail.org"
;; ;;                           "Workflow.org"))
;; ;;   :init
;; ;;   (require 'dotcrafter) ; Not sure why I have to do this...
;; ;;   :config
;; ;;   (dotcrafter-mode))

;; (defun gbl/minibuffer-backward-kill (arg)
;;   "When minibuffer is completing a file name delete up to parent
;; folder, otherwise delete a word"
;;   (interactive "p")
;;   (if minibuffer-completing-file-name
;;       (if (string-match-p "/." (minibuffer-contents))
;;           (zap-up-to-char (- arg) ?/)
;;         (delete-minibuffer-contents))
;;       (delete-word (- arg))))

;; (use-package vertico
;;   ;; :straight '(vertico :host github
;;   ;;                     :repo "minad/vertico"
;;   ;;                     :branch "main")
;;   :ensure t
;;   :bind (:map vertico-map
;;          ("C-j" . vertico-next)
;;          ("C-k" . vertico-previous)
;;          ("C-f" . vertico-exit)
;;          :map minibuffer-local-map
;;          ("M-h" . gbl/minibuffer-backward-kill))
;;   :custom
;;   (vertico-cycle t)
;;   :custom-face
;;   (vertico-current ((t (:background "#3a3f5a"))))
;;   :init
;;   (vertico-mode))


;; (defun gbl/get-project-root ()
;;   (when (fboundp 'projectile-project-root)
;;     (projectile-project-root)))

;; (use-package consult
;;   :ensure t
;;   :demand t
;;   :bind (("C-s" . consult-line)
;;          ("C-M-l" . consult-imenu)

;;          ("C-M-j" . persp-switch-to-buffer*)
;;          :map minibuffer-local-map
;;          ("C-r" . consult-history))
;;   :custom
;;   (consult-project-root-function #'gbl/get-project-root)
;;   (completion-in-region-function #'consult-completion-in-region))


;; (use-package consult-dir
;;   :ensure t
;;   :bind (("C-x C-d" . consult-dir)
;;          :map vertico-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file))
;;   :custom
;;   (consult-dir-project-list-function nil))
;; (use-package avy
;;   :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

;; (use-package bufler
;;   :disabled
;;   :straight t
;;   :bind (("C-M-j" . bufler-switch-buffer)
;;          ("C-M-k" . bufler-workspace-frame-set))
;;   :config
;;   (evil-collection-define-key 'normal 'bufler-list-mode-map
;;     (kbd "RET")   'bufler-list-buffer-switch
;;     (kbd "M-RET") 'bufler-list-buffer-peek
;;     "D"           'bufler-list-buffer-kill)

;;   (setf bufler-groups
;;         (bufler-defgroups
;;           ;; Subgroup collecting all named workspaces.
;;           (group (auto-workspace))
;;           ;; Subgroup collecting buffers in a projectile project.
;;           (group (auto-projectile))
;;           ;; Grouping browser windows
;;           (group
;;            (group-or "Browsers"
;;                      (name-match "Vimb" (rx bos "vimb"))
;;                      (name-match "Qutebrowser" (rx bos "Qutebrowser"))
;;                      (name-match "Chromium" (rx bos "Chromium"))))
;;           (group
;;            (group-or "Chat"
;;                      (mode-match "Telega" (rx bos "telega-"))))
;;           (group
;;            ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
;;            (group-or "Help/Info"
;;                      (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
;;                      ;; (mode-match "*Helpful*" (rx bos "helpful-"))
;;                      (mode-match "*Info*" (rx bos "info-"))))
;;           (group
;;            ;; Subgroup collecting all special buffers (i.e. ones that are not
;;            ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
;;            ;; through to other groups, so they end up grouped with their project buffers).
;;            (group-and "*Special*"
;;                       (name-match "**Special**"
;;                                   (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
;;                       (lambda (buffer)
;;                         (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
;;                                              buffer)
;;                                     (funcall (mode-match "Dired" (rx bos "dired"))
;;                                              buffer)
;;                                     (funcall (auto-file) buffer))
;;                           "*Special*"))))
;;           ;; Group remaining buffers by major mode.
;;           (auto-mode))))

;; (use-package default-text-scale
;;   :defer 1
;;   :config
;;   (default-text-scale-mode))

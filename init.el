 ;; Max Masterton's Configuration for the GNU/Emacs elisp interpreter
;;                       __                            
;;   __ _ _ __  _   _   / /__ _ __ ___   __ _  ___ ___ 
;;  / _` | '_ \| | | | / / _ \ '_ ` _ \ / _` |/ __/ __|
; | (_| | | | | |_| |/ /  __/ | | | | | (_| | (__\__ \
;;  \__, |_| |_|\__,_/_/ \___|_| |_| |_|\__,_|\___|___/
;;  |___/
;;
;; Found at github.com/maxmasterton/emacs

;; Setting up Package.el to work with MELP
(server-start)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Installing and Configuring use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; all use-package statements will include :ensure t by default

;;Variable definition
(defvar gbl/leader "C-SPC")


;; Functions definition
(defun launcher (prog)
  (interactive)
  (start-process-shell-command prog nil prog))

;; System packages
(use-package system-packages)

;; Setting up auto-package update so that packages are updated automatically
;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7) ; Every seven days
;;   (auto-package-update-prompt-before-update t) ; Ask permission first
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00")) ; At 9:00AM

;; Real auto-save feature
(use-package real-auto-save
  :config (setq real-auto-save-interval 50)
  :hook (prog-mode . real-auto-save-mode)
  :hook (org-mode . real-auto-save-mode))

;; Startup Performace:
;; Garbage Collection
;; Using garbage collection magic hack (gcmh)
(use-package gcmh
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold (* 2 1000 1000)
	  gc-cons-percentage 0.6)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-complation)
	(setq comp-deferred-complation nil
		  native-comp-deferrede-complation nil))
;; In noninteractive situations, prioritze non-byte-compiled source files to prevent the use of stale byte-code
(setq load-prefer-newer noninteractive)

;; Setting some rudimentary settings
(setq-default
 delete-by-moving-to-trash t
 tab-width 4
 window-combination-resize t ; When a new window is created, take space from all existing windows
 x-stretch-cursor t) ; Stretch cursor to cover long characters

(setq undo-limit 80000000 ; Set undo limit to 8MB
	  evil-want-fine-undo t ; Granular changes in insert-mode
	  uniquify-buffer-name-style 'forward ; uniquify buffer names
	  truncate-lines nil
	  indent-tabs-mode t)

;; Setting up contact details, used by mu4e and magit
(setq user-full-name "Ganfina Brice-Loic"
	  user-mail-address "ganfinab@gmail.com")

;; Dashboard
(use-package dashboard										  
  :init
  (setq dashboard-center-content t
		dashboard-banner-logo-title "Welcome Brice Ganfina"
		dashbord-set-heading-items t
		dashboard-set-file-icons t
		dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  :config													  
  (dashboard-setup-startup-hook))

;; For emacsclient:
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Disable lockfiles
(setq create-lockfiles nil)

;; Installing 'general.el' for keybindings
(use-package general
  :config (general-evil-setup t))

;; Removing useless GUI elements
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 7)
(setq inhibit-startup-message t)
(menu-bar--display-line-numbers-mode-relative)
(tool-bar-mode -1)
(menu-bar-mode -1)

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

(use-package smooth-scrolling) ; The smooth-scrolling.el works somewhat, have switched it out for sublimity
(smooth-scrolling-mode 1)

;; Defining a function to insert a tab char when tab is pressed
(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

;; Put numbers on every buffer, unless specified otherwise in the dolist.
(column-number-mode)
(global-display-line-numbers-mode 1)
(dolist (mode '(org-mode-hook
				term-mode-hook
				vterm-mode-hook
				treemacs-mode-hook
				;elfeed-search-mode-hook
				elfeed-show-mode-hook
				eww-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))) ; Turn off line numbers

;; Truncated lines on by default
(global-visual-line-mode t)

;; Defining some fonts
(set-face-attribute 'default nil
					:font "JetBrains Mono 10" ; Monospaced coding font
					:weight 'medium)
(set-face-attribute 'variable-pitch nil ; Non tech font
					:font "Cantarell"
					:height 110)
(set-face-attribute 'fixed-pitch nil
					:font "JetBrains Mono 10"
					:weight 'medium)

;; Extra font settings
(setq-default line-spacing 0.03)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10")) ; Emacsclient does not accept fonts by default
(setq global-prettify-symbols-mode t) ; Glyph support

;; Keybindings for scaling text (although hydra function exists)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Dad-joke queries
;; (use-package dad-joke
;;   :config (defun dad-joke ()
;; 			(interactive)
;; 			(insert (dad-joke-get))))



;; Using 'doom-themes' as a theme repository
(use-package doom-themes
  :config
  ;; Disable italics and bold
  (setq doom-themes-enable-bold nil
		doom-themes-enable-italic nil)

  ;; Function for switching to light theme:
  (defun maxm/load-dark-theme ()
	(interactive)
	
	(disable-theme 'doom-solarized-light)
	(load-theme 'doom-dracula t))

  ;; Function for switching to light theme
  (defun maxm/load-light-theme ()
	(interactive)

	(disable-theme 'doom-dracula)
	(load-theme 'doom-solarized-light t)
	(setq-default input-block "#F9F2D9")))

(maxm/load-dark-theme) ; Load dark theme by default

(nvmap :prefix gbl/leader
  "SPC l" '(maxm/load-light-theme :which-key "Light theme")
  "SPC d" '(maxm/load-dark-theme :which-key "Dark theme"))

;; Setting Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(defun set-transparency())
;; Diming unused windows
(use-package dimmer
  :config (dimmer-mode))

;; Overriding theme-set font attributes, comments should be italic and keywords shoult not be bold.
(set-face-attribute 'font-lock-comment-face nil
					:slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
					:weight 'medium)

;; Adding upport for emojis and icons
(use-package all-the-icons)

(use-package emojify)

(use-package afternoon-theme)

(use-package flatui-theme)

(use-package vampyricdark-theme)

(use-package catppuccin-theme
 :config
 (setq catppuccin-height-title1 1.5))
;;;; Completion

;; Installing vertico
;; vertico is an autocompletion engine for Emacs, vertico-rich allows suggestions to have descriptions
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
         ("C-c f" . consult-flycheck)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-s" . consult-line)
         ("C-\\" . consult-line-multi)

         :map isearch-mode-map
         ("C-s" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s l" . consult-line-multi)            ;; needed by consult-line to detect isearch

         :map minibuffer-local-map
         ("s-s" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.1
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

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

;; (use-package mini-popup)
;; (use-package vertico-posframe
;;   :init
;;   (vertico-posframe-mode))

;; Using Winum to label open windows
(use-package winum
  :config (winum-mode))

;; (use-package telephone-line
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
;;       telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
;;       telephone-line-primary-right-separator 'telephone-line-cubed-right
;;       telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
;;   (setq telephone-line-height 24
;; 		telephone-line-evil-use-short-tag t)
;;   :init
;;  (telephone-line-mode 1))

(use-package diminish)

(dolist (minor-mode minor-mode-list)
  (diminish minor-mode))

(use-package minions
  :after doom-modeline)

(use-package rich-minority)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 25)
           (doom-modeline-bar-width 6)
           (doom-modeline-lsp t)
           (doom-modeline-github nil)
           (doom-modeline-mu4e nil)
           (doom-modeline-irc t)
           (doom-modeline-minor-modes t)
           (doom-modeline-persp-name nil)
           (doom-modeline-buffer-file-name-style 'truncate-except-project)
           (doom-modeline-major-mode-icon nil)))

;; Use syntax highlighting to match pairs of delimiters ({} or [] or ()).
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Electric Pair Mode
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; Better commenting of lines
;; M-; does comment by standard but the behaviour isn't exactly what you might expect
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Enabling which-key to help with long key strings.
(use-package which-key
  :diminish
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha ; order alphabetically
		which-key-sort-uppercase-first nil
		which-key-add-column-adding 1
		which-key-min-display-lines 4
		which-key-idle-delay 0.3 ; wait 0.3 seconds before showing suggestions
		which-key-allow-imprecise-window-fit nil
		which-key-seperator " -> ")) ; Seperator selection
(which-key-mode)

;; Buffer keybindings:
(nvmap :prefix gbl/leader
	   "b b" '(ibuffer :which-key "Open ibuffer")
	   "b k" '(kill-current-buffer :which-key "Kill buffer")
	   "b c" '(hydra-cycle-buffers/body :which-key "Cycle Buffers"))

;; File keybindings:
(nvmap :states '(normal visual) :keymaps 'override :prefix gbl/leader
	   "f f" '(find-file :which-key "Find file")
	   "f ." '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "open emacs init.el")
	   "f r" '(consult-recent-file :which-key "Recent files")
	   "f s" '(save-buffer :which-key "Save file")
	   "f u" '(sudo-edit :which-key "Sudo edit file")
	   "f R" '(rename-file :which-ke "Rename file")

	   ;; Elisp interpretation keybindings:
	   "e b" '(eval-buffer :which-key "Evaluate elisp in buffer")
	   "e d" '(eval-expression :which-key "Evaliate elisp expression")
	   "e r" '(eval-region :which-key "Evaluate highlighted reigon"))

; Dependencies for file keybindings:
(use-package recentf
  :config
  (recentf-mode))

(use-package sudo-edit)

; Standard keybindings:
(nvmap :keymaps 'override :prefix gbl/leader
	   gbl/leader	'(execute-extended-command :which-key "M-x")
	   "RET"	'(bookmark-jump :which-key "Bookmarks")
	   "c"	'(compile :which-key "Compile")
	   "C"	'(recompile :which-key "Recompile")
	   "l"	'((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload config")
	   ;; "t"	'(toggle-truncate-lines :which-key "Toggle truncated lines")
	   "s"	'(hydra-text-scale/body :which-key "Scale text")
	   "r"	'(writeroom-mode :which-key "Writeroom mode")
	   ","	'(switch-to-buffer :which-key "Switch to buffer")
	   ";"	'(find-file :which-key "Find file"))

(nvmap :prefix gbl/leader
  "g" 	'(:ignore t :which-key "Grep command")
  "g g" '(consult-grep :which-key "Grep across project")
  "g r" '(consult-ripgrep :which-key "Ripgrep across project"))

; Dependencies for standard keybindings:
(use-package writeroom-mode)


;;Hydea for window resize
(defhydra gbl/hydra-window-resizer(:timeout 4)
  "This hydra define a set on function that resize a window"
  ("k" (shrink-window 2) "Shrink window vertically")
  ("j" (enlarge-window 2) "Enlarge window vertically")
  ("h" (shrink-window-horizontally 2) "Shrink window horizontally")
  ("l" (enlarge-window-horizontally 2) "Enlarge window horizontally")
  ("q" nil "Quit" :exit t))

;; Window control keybindings:
(nvmap :prefix gbl/leader
	   ;; Window Splits
	   "w" '(:ignore t :which-key "Close window")
	   "w c" '(evil-window-delete :which-key "Close window")
	   "w n" '(evil-window-new :which-key "New window")
	   "w s" '(evil-window-split :which-key "Split window")
	   "w v" '(evil-window-vsplit :which-key "Vsplit window")
	   ;; Window selection
	   "w h" '(evil-window-left :which-key "Window left")
	   "w j" '(evil-window-down :which-key "Window down")
	   "w k" '(evil-window-up :which-key "Window up")
	   "w l" '(evil-window-right :which-key "Window right")
	   ;; Window movement
	   "w r" '(gbl/hydra-window-resizer/body :which-key "Resize window")
	   ;; Custom window layout functions
	   "w t" '(maxm/window-plit-toggle :which-key "Window split toggle"))

;; Custom window functions
(defun maxm/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
	  (error "Can't toggle with more than 2 windows!")
	(let ((func (if (window-full-height-p)
					#'split-window-vertically
				  #'split-window-horizontally)))
	  (delete-other-windows)
	  (funcall func)
	  (save-selected-window
		(other-window 1)
		(switch-to-buffer (other-buffer))))))

;; (defun eshell ()
;;   "Une function qui lance eshell hortizontallement."
;;   (interactive)
;;   (evil-window-new)
;;   (evil-window-decrease-height 10)
;;   (eshell-mode))

;; Setting up EVIL
;; Vim emulation within emacs. Limited functionality within EXWM.
(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t ; when v-splitting a window select right window
		evil-split-window-below t) ; when h-splitting a window select bottom window
  :config
  (evil-mode)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection ; Evil collection adds support for non-text edditing applications of EVIL
  :after evil
  :config
  (evil-collection-init))

;; Hydra allows for good keybindings for repetitive tasks
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4) ; Change the size of text
  "scale text"
  ("j" text-scale-increase "inc")
  ("k" text-scale-decrease "dec")
  ("q" nil "finished" :exit t))

(defhydra hydra-cycle-buffers (:timeout 4) ; Cycle through buffers, killing uneccessary ones
  "cycle buffers"
  ("j" next-buffer "next")
  ("k" previous-buffer "prev")
  ("SPC" kill-current-buffer "kill")
  ("q" nil "quit" :exit t))

;; Setting up projectile, emacs project managment tool.
;; Mainly used to ripgrep through projects
(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
	(setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(defun gbl/evil-hook ()
  (dolist (mode '(custom-mode
      eshell-mode
      git-rebase-mode
      erC-mode
      circle-server-mode
      circle-chat-mode
      sauron-mode
      term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package git)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit) 
;; note: make sure to configure a github token before using this package!
;; - https://magit.vc/manual/forge/token-creation.html#token-creation
;; - https://magit.vc/manual/ghub/getting-started.html#getting-started
(use-package forge
  :after magit)

;; Project sidebar tool for emacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum ; Load after winum
	(define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
	(setq treemacs-collpase-dirs (if treemacs-python-executable 3 0)
		  treemacs-show-cursor nil
		  treemacs-show-hidden-files t
		  treemacs-width 35)
	(treemacs-follow-mode t)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode 'always)
	(treemacs-git-mode 'simple))
  :bind
  (:map global-map
		("C-x t t" . treemacs) ;; Keybindings for treemacs
		("C-x t B" . treemacs-bookmark)))

;; Must be installed if evil is used otherwise treemacs will not work correctly
(use-package treemacs-evil
  :after (treemacs evil))

;; Treemacs theme for all-the-icons, nicer, github icons.
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

;; File manager for emacs, incuded package within emacs 27
(use-package dired
  :ensure nil
  :commands (dired dired-jumo)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switched "=-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map ; VIM keybindings for navigating directories
	"h" 'dired-up-directory
	"l" 'dired-find-file))

;; Dired Extentions: (dired plus is no longer maintained)
(use-package dired-single
  :commands (dired dired-jump))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions '(("png" . "feh")
								("mvk" . "mpv")
								("avi" . "mpv")
								("mp4" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package peep-dired)

;; Dired general.el keymaps, also opened through open keybindings
(nvmap :states '(normal visual) :keymaps 'override :prefix gbl/leader
  "d d" '(dired :which-key "Open dired")
  "d j" '(dired-jump :which-key "Dired jump to current")
  "d p" '(peep-dired :which-key "Peep dired"))

;; Further vim keybindings
(with-eval-after-load 'dired
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(add-hook 'peep-dired-hook 'evil-normalize-keymaps) ; Evil normalize keymap
(setq dired-open-extentions '(("gif" . "sxiv") ;; When a gif is selected, it must be opened within sxiv.
							  ("jpg" . "sxiv")
							  ("png" . "sxiv")
							  ("mkv" . "mpv")
							  ("mp4" . "mpv")))

;; Dired will have all-the-icons, same as in treemacs
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Term mode is less functional yet less resource intensive than vterm.
(use-package term
  :config
  (setq explicit-shell-file-name "bash")) ; "powershell.exe" for windows

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(nvmap :prefix gbl/leader ; Eshell general.el keybindings
  "E h" '(counsel-esh-history :which-key "Eshell history")
  "E s" '(eshell :which-key "Eshell"))

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

  (use-package xterm-color)

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

(setup eshell
  (add-hook 'eshell-first-time-mode-hook #'gbl/eshell-configure)
  (setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))

(use-package eshell-z
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
		 (eshell-z-change-dir-hook . (lambda () (eshell/pushd (eshell/pwd))))))

;; (setup (:pkg exec-path-from-shell)
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

(use-package eshell-git-prompt)
(use-package eshell-toggle)
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
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

;; Configuring EWW, emacs web wowser
(setq
 browser-url-browser-function 'eww-browser-url
 shr-use-fonts nil
 shr-use-colors nil
 shr-indentation 2
 shr-width 70
 eww-search-prefix "https://wiby.me/?q=") ; Wiby.me as the default search engine

;; General.el keybindings for opening applications
(nvmap :prefix gbl/leader
  "o e" '(elfeed :which-key "Open elfeed")
  "o v" '(vterm :which-key "Open vterm")
  "o s" '(eshell-toggle :which-key "Open eshell")
  "o t" '(term :which-key "Open term")
  "o d" '(dired-jump :which-key "Open dired")
  "o a" '(org-agenda :which-key "Open org-agenda")
  "o w" '(eww :which-key "Open eww")
  "o p" '(treemacs :which-key "Open project sidebar"))

;;; LSP-Mode
;; Initial Configuration
;; (use-package lsp-mode
;;   :hook (lsp-mode . maxm/lsp-mode-setup)
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l"))

;; Breader Headcrumb
(defun maxm/lsp-mode-setup ()
  (setq lsp-headline-breadcrumb-segmments '(path-up-to-project file symbols))
  (lsp-headline-breadcrumb-mode))


;;snippets
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :after yasnippet)

;; Better completions with company-mode
(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package consult-flycheck)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
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
  (company-show-quick-access t))

;; Cleaner Aesthetic with Company-box
(use-package company-box
  :hook (company-mode . company-box-mode))

(require 'eglot)

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)

(require 'emmet-mode)

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
		("C-c-b" . web-beautify-html))
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
(use-package dap-mode)

;; Python keybindings
(nvmap :prefix gbl/leader
  "p p" '(run-python :which-key "Run python")
  "p r" '(python-shell-send-reigon :which-key "Interpret Reigon") ; Selected Reigon
  "p b" '(python-shell-send-buffer :which-key "Interpret Buffer"))

;;; Org Mode
(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▼ " 
		org-hide-emphasis-markers t))

(setq org-directory "~/org/"
	  org-agenda-files '("~/org/agenda.org") ; NOTE: This is a list, more files can be added later
	  org-default-notes-file (expand-file-name "notes.org" org-directory))

;; This overides GNU default
(setq org-link-abbrev-alist
	  '(("google" . "https://www.google.com/search?q=")
		("arch-wiki" . "https://wiki.archlinux.org/index.php")
		("freebsd-forum" . "https://forums.freebsd.org") ; I don't know if this one works.
		("duckduckgo" . "https://duckduckgo.com/?q=")
		("wiby" . "https://wiby.me/?q=")
		("wikipedia" . "https://en.wikipedia.org/wiki")
		("reddit" . "https://old.reddit.com/r/")))

(setq org-todo-keywords
	  '((sequence
		 "TODO(t)"
		 "BOOK(b)"
		 "NEXT(n)"
		 "PROJ(p)"
		 "HOWEWORK(h)"
		 "WAIT(w)"
		 "|"
		 "DONE(d)"
		 "CANCELLED(c)" )))

(setq org-tag-alist
    '((:startgroup)
       ; put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?e)
       ("@home" . ?h)
       ("@work" . ?w)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?p)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

(use-package org-tempo
  :ensure nil)

;; Make sure that babel code blocks evaluate code correctly
(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
	  org-confirm-babel-evaluate nil
	  org-edit-src-content-indentation 0)

(setq org-blank-before-new-entry (quote ((heading . nil)
										 (plain-list-item . nil))))

;; Make sure that python code can be executed inside a babel code block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (plantuml . t)))

;; Use cooler and more diffrenciated bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; Configuration of Font faces and sizes within org documents
(with-eval-after-load 'org-faces ; Must be wrapped in =with-eval-after-load=
  ;; Diffrenciate headers based on size
  (dolist (face '((org-level-1 . 1.2)
				  (org-level-2 . 1.1)
				  (org-level-3 . 1.05)
				  (org-level-4 . 1.0)
				  (org-level-5 . 1.1) ; Back to normal
				  (org-level-6 . 1.1)
				  (org-level-7 . 1.1)
				  (org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Needs fixing:
  ;; Choosing what elements of an org-document should be represented in what font face.
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitched-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Center org-mode documents in the center of the screen
(defun org-mode-visual-fill ()
  ; Proportions:
  (setq visual-fill-column-width 75
		visual-fill-column-center-text t)
  (visual-fill-column-mode 1)) ; Activate

;; The centering of documents depends on the following package:
(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; 'Latex' from Org-mode
(use-package biblio) ; Quick BibTex refrences, sometimes.

(setq org-latex-listings 'minted
	  org-latex-packages-alist '(("" "minted")))

(setq org-latex-pdf-process
	  '("pdflatex -interaction nonstopmode -output-directory %o %f"))

;; Org keybindings
(nvmap :keymaps 'override :prefix gbl/leader
  "m m" '(org-mode :which-key "Restart org mode")
  "m h" '(org-toggle-heading :which-key "Toggle heading")
  "m i" '(org-toggle-item :which-key "Toggle item")
  "m ." '(counsel-org-goto :which-key "Counsel-org goto")
  "m b" '(org-babel-tangle :which-key "Org babel tangle")
  "m t" '(counsel-org-tag :which-key "Counsel org-tag")
  "m T" '(org-tags-view :which-key "Org tags view")
  "m w" '(org-todo-list :which-key "Org todo list"))

;; EXWM Functions:
(defun max/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun max/exwm-init-hook ()
  (exwm-workspace-switch-create 1)) ; Start on workspace 1, not 0

(defhydra exwm-window-resize (:timeout 4)
  ("h" (exwm-layout-shrink-window-horizontally 10) "shrink h")
  ("l" (exwm-layout-enlarge-window-horizontally 10) "enlarge h")
  ("k" (exwm-layout-shrink-window 10) "shrink v")
  ("j" (exwm-layout-enlarge-window 10) "enlarge v")
  ("q" nil "quit" :exit t))


(defun kill-buffer-slip-window ()
  "a function for delete slip window and kill the buffer."
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "TAB") 'my-insert-tab-char)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Use ESC to quit prompts
;(global-set-key (kbd "C-;") 'counsel-switch-buffer)

(global-set-key (kbd "C-c & l") 'consult-yasnippet)

(global-set-key (kbd "M-:") 'evil-ex)
(global-set-key (kbd "M-q") 'kill-buffer-slip-window)
(global-set-key (kbd "M-Q") 'delete-window)

(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-l") 'evil-window-right)
(global-set-key (kbd "M-k") 'evil-window-up)
(global-set-key (kbd "M-j") 'evil-window-down)

(global-set-key (kbd "M-J") 'windmove-swap-states-up)
(global-set-key (kbd "M-K") 'windmove-swap-states-down)
(global-set-key (kbd "M-L") 'windmove-swap-states-right)
(global-set-key (kbd "M-H") 'windmove-swap-states-left)

(global-set-key (kbd "s-k") 'switch-to-prev-buffer)
(global-set-key (kbd "s-j") 'switch-to-next-buffer)
(global-set-key (kbd "s-h") 'find-file)

(global-set-key (kbd "s-q") '(lambda () (interactive) (launcher "qutebrowser")))
(global-set-key (kbd "s-b") '(lambda () (interactive) (launcher "qutebrowser")))
(global-set-key (kbd "s-t") '(lambda () (interactive) (launcher "alacritty")))
(global-set-key (kbd "s-m") '(lambda () (interactive) (launcher "mpv")))
(global-set-key (kbd "s-p") 'package-install)


(global-set-key (kbd"C-M-j") 'evil-collection-unimpaired-move-text-down)
(global-set-key (kbd"C-M-k") 'evil-collection-unimpaired-move-text-up)
(global-set-key (kbd"C-M-h") 'scroll-other-window-down)
(global-set-key (kbd"C-M-l") 'scroll-other-window)


(global-set-key (kbd"C--") 'desktop-environment-volume-decrement)
(global-set-key (kbd"C-=") 'desktop-environment-volume-increment)

;; The desktop environment package allows for control of brightness and volume
(use-package desktop-environment
  :after exwm
  :bind ("s-=" . desktop-environment-lock-screen)
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")

  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; This hydra function allows for control of volume
(defhydra hydra-volume-up (:timeout 4)
  "Configure Volume"
  ("j" desktop-environment-volume-decrement "down")
  ("k" desktop-environment-volume-increment "up")
  ("q" nil "quit" :exit t))

;; This hydra function allows for control of brightness
(defhydra hydra-brightness-up (:timeout 4)
  "Configure Brightness"
  ("j" desktop-environment-brightness-increment "up")
  ("k" desktop-environment-brightness-decrement "down")
  ("q" nil "quit" :exit t))

;; Keybindings for deskop-environment.el
(nvmap :prefix gbl/leader
  "SPC v" '(hydra-volume-up/body :which-key "Change volume")
  "SPC t" '(consult-theme :which-key "Load theme")
  "SPC b" '(hydra-brightness-up/body :which-key "Change brightness")
  "SPC m" '(desktop-environment-toggle-mute :which-key "Toggle mute")
  "SPC M" '(desktop-environment-toggle-microphone-mute :which-key "Toggle microphone")
  "SPC s" '(desktop-environment-screenshot :which-key "Take screenshot"))

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


;; EXWM: Emacs Xorg Window Manager
;; (defun gbl/polybar-exwm-workspace
;; 	)

(defvar gbl/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun gbl/kill-panel ()
  (interactive)
  (when gbl/polybar-process
    (ignore-errors
      (kill-process gbl/polybar-process)))
  (setq gbl/polybar-process nil))

(defun gbl/start-panel ()
  (interactive)
  (gbl/kill-panel)
  (setq gbl/polybar-process (start-process-shell-command "~/.config/polybar/launch.sh" nil "~/.config/polybar/launch.sh")))

(defun gbl/run-in-bg (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))


(defun gbl/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun gbl/send-polybar-exwm-workspace ()
  (gbl/send-polybar-hook "exwm-workspace" 1))

(defun gbl/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun gbl/exwm-update-title ()
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-rename-buffer (format "QuteBrowser: %s" exwm-title)))
    ("Alacritty" (exwm-workspace-rename-buffer (format "QuteBrowser: %s" exwm-title)))
    ("mpv" (exwm-workspace-rename-buffer (format "MPV: %s" exwm-title)))))

(defun gbl/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
          (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

(defun gbl/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("Alacritty" (exwm-workspace-move-window 0))
    ("TelegramDesktop" (exwm-workspace-move-window 9))
    ("mpv" (exwm-workspace-move-window 4))))

;; Update panel indicator when workspace changes

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'max/exwm-update-class)

  (add-hook 'exwm-update-title-hook #'gbl/exwm-update-title)

  (add-hook 'exwm-manage-finish-hook #'gbl/configure-window-by-class)

  (add-hook 'exwm-workspace-switch-hook #'gbl/send-polybar-exwm-workspace)
  ;; When EXWM finishes initialization, do some extra setup:
  (add-hook 'exwm-init-hook #'max/exwm-init-hook)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-h

      ?\M-&
      ?\M-x
      ?\M-:

      ?\M-h
      ?\M-l
      ?\M-k
      ?\M-j

      ?\M-J
      ?\M-K
      ?\M-H
      ?\M-L

      ?\M-q
      ?\M-Q

      ?\s-p
      ?\s-h
      ?\s-j
      ?\s-k
      ?\s-b

      ?\C--
      ?\C-=

      ?\C-\M-h
      ?\C-\M-l
      ?\C-\M-k
      ?\C-\M-j

      ?\C-\ ))     ;; Ctrl+Spacev

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\M-r] . exwm-window-resize/body)

		  ;; Toggle floating windows
		  ([?\M-t] . exwm-floating-toggle-floating)

		  ;; Toggle fullscreen
		  ([?\M-f] . exwm-layout-toggle-fullscreen)

		  ;; Toggle modeline
		  ;; ([?\s-m] . exwm-layout-toggle-mode-line)

          ;; Launch applications via shell command
		  ([?\M-d] . counsel-linux-app)

          ([?\M-a] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; Switch workspace
          ([?\M-w] . exwm-workspace-move-window)

		  ;; Move the current window to the i (1-9) workspace
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-move-window ,i))))
                    (number-sequence 1 9))

          ([?\M-²] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
		  
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "M-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "<M-tab>") 'evil-window-next)
  (exwm-input-set-key (kbd "M-SPC") 'evil-window-vsplit)
  (exwm-input-set-key (kbd "<M-return>") 'evil-window-split)

  (gbl/start-panel)
  
  (gbl/run-in-bg "dunst")
  (gbl/run-in-bg "nm-applet")
  (gbl/run-in-bg "pasystray")
  (gbl/run-in-bg "blueman-applet")
  ;;(gbl/run-in-bg "qutebrowser")
  ;;(gbl/run-in-bg "mpv")
  ;;(gbl/run-in-bg "alacritty")

  (exwm-enable))

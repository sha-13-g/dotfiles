; you will most likely need to adjust this font size for your system!
(defvar gbl/default-font-size 90)
(defvar gbl/default-variable-font-size 90)
;; Make frame transparency overridable

;;(auto-complete-mode)
(defvar gbl/frame-transparency '(50 . 90))
(defvar gbl/frame-transparency-beta '(80 . 90))
;;(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;;(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; The defaultemacs. is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun gbl/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(add-hook 'emacs-startup-hook #'gbl/display-startup-time)


;; Initialize package sources
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

  ;; Initialize use-package on non-Linux platforms
(require 'use-package)

(setq straight-use-package-by-default t)

;; (use-package auto-complete
;;   :bind (:map ac-completing-map
;; 	      ("C-j" . ac-next)
;; 	      ("C-k" . ac-previous)
;; 	      ("C-l" . ac-complete))
;;   :custom
;;   (ac-delay 0)
;;   (ac-quick-help-delay 0)
;;   :init
;;  (ac-config-default))

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

;; (use-package no-littering)

(use-package perspective
  :bind
  ("C-x C-l" . persp-list-buffers)   ; or use a nicer switcher, see below
  :init
  (persp-mode))

;; (add-hook 'kill-emacs-hook #'persp-state-save)

(add-to-list 'load-path "~/.emacs.d/autopair/") ;; comment if autopair.el is in standard load path
;; (add-to-list 'load-path "~/.emacs.d/") ;; comment if autopair.el is in standard load path

(require 'autopair)
;; (setq visible-bell t)
(autopair-global-mode)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha gbl/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,gbl/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height gbl/default-font-size)

;; (add-hook 'javascript-mode-hook #'lsp)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height gbl/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height gbl/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package peep-dired)

(defun gbl/show-and-copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun gbl/show-buffer-path-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer gbl/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (gbl/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "tt" '(consult-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-fle-name "~/.emacs.d/emacs.org")))

    "o"   '(:ignore t :which-key "org mode")

    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")

    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

    "os"  '(dw/counsel-rg-org-files :which-key "search notes")

    "oa"  '(org-agenda :which-key "Status")
    "ot"  '(org-todo-list :which-key "Todos")
    "oc"  '(org-capture t :which-key "Capture")
    "ox"  '(org-export-dispatch t :which-key "Export")
    "w"   '(evil-window-map t :which-key "Window")
    "q"   '(delete-window  t :which-key "Quit")
    "C"   '(delete-frame  t :which-key "Quit Frame")
    "Q"   '(save-buffers-kill-terminal  t :which-key "Quit Emacs")
    ","   '(switch-to-buffer :which-key "switch buffer"))
;; (general-define-key
;;  "C-=" 'text-scale-increase
;;  "C--" 'text-scale-decrease
 ;; "M-p" 'package-install
 ;; "M-h" 'windmove-left
 ;; "M-l" 'windmove-right
 ;; "M-k" 'windmove-up
 ;; "M-j" 'windmove-down
 ;; "C-M-h" 'evil-window-move-far-left
 ;; "C-M-l" 'evil-window-move-far-right
 ;; "C-M-k" 'evil-window-move-very-top
 ;; "C-M-j" 'evil-window-move-very-bottom
 ;; "C->" 'evil-window-increase-width
 ;; "C-<" 'evil-window-decrease-width
 ;; "C-k" 'evil-window-increase-height
 ;; "C-j" 'evil-window-decrease-height)
  (general-define-key
   "C-M-k" '(scroll-other-window-down :which-key "Scroll other window down")
   "C-M-j" '(scroll-other-window :which-key "Scroll other window up"))

(gbl/leader-keys
  "b"  '(:ignore t :which-key "Buffer")
  "b b"   '(ibuffer :which-key "Ibuffer")
  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "b n"   '(next-buffer :which-key "Next buffer")
  "b p"   '(previous-buffer :which-key "Previous buffer")
  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
  "b s"   '(switch-to-buffer :which-key "Switch buffer")
  "b K"   '(kill-buffer :which-key "Kill buffer"))

(gbl/leader-keys
  "e"  '(:ignore t :which-key "Eval")
  "e h"   '(counsel-esh-history :which-key "Eshell history")
  "e s"   '(eshell :which-key "Eshell")
  "e b"   '(eval-buffer :which-key "Eval in buffer")
  "e d"   '(eval-defun :which-key "Eval defun")
  "e e"   '(eval-expression :which-key "Eval expression")
  "e l"   '(eval-last-sexp :which-key "Eval last sexression")
  "e r"   '(eval-region :which-key "Eval region"))
(gbl/leader-keys
  "d"  '(:ignore t :which-key "Dired")
  "d d" '(dired :which-key "Open dired")
  "d j" '(dired-jump :which-key "Dired jump to current")
  "d p" '(peep-dired :which-key "Peep-dired"))
(gbl/leader-keys
  "f"  '(:ignore t :which-key "File")
  "."     '(find-file :which-key "Find file")
  "f f"   '(find-file :which-key "Find file")
  "f r"   '(counsel-recentf :which-key "Recent files")
  "f s"   '(save-buffer :which-key "Save file")
  "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
  "f y"   '(gbl/show-and-copy-buffer-path :which-key "Yank file path")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  "f S"   '(write-file :which-key "Save file as...")
  "f U"   '(sudo-edit :which-key "Sudo edit file"))

(gbl/leader-keys
  "r" '(:ignore t :which-key "Register")
  "r c"   '(copy-to-register :which-key "Copy to register")
  "r f"   '(frameset-to-register :which-key "Frameset to register")
  "r i"   '(insert-register :which-key "Insert register")
  "r j"   '(jump-to-register :which-key "Jump to register")
  "r l"   '(list-registers :which-key "List registers")
  "r n"   '(number-to-register :which-key "Number to register")
  "r r"   '(counsel-register :which-key "Choose a register")
  "r v"   '(view-register :which-key "View a register")
  "r w"   '(window-configuration-to-register :which-key "Window configuration to register")
  "r +"   '(increment-register :which-key "Increment register")
  "r SPC" '(consult-register-store :which-key "Point to register"))

(gbl/leader-keys
  "s" '(:ignore t :which-key "Search")
  "s f"   '(consult-find :which-key "Find file in project")
  "s o"   '(consult-outline :which-key "Search to headers")
  "s m"   '(consult-mark :which-key "Goto mark")
  "s M"   '(consult-globabl-mark :which-key "Goto all marks")
  "s i"   '(consult-imenu :which-key "Imenu")
  "s I"   '(consult-imenu-multi :which-key "Imenu multi")
  "s l"   '(consult-locate :which-key "Find file")
  "s g"   '(consult-grep :which-key "Grep in project")
  "s G"   '(consult-git-grep :which-key "Grep in git")
  "s r"   '(consult-ripgrep :which-key "Ripgrep in project"))

(gbl/leader-keys
  "SPC"   '(execute-extended-command :which-key "M-x")
  "c c"   '(compile :which-key "Compile")
  "c f"   '(consult-flymake :which-key "show error")
  "c C"   '(recompile :which-key "Recompile")
  "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config"))

(gbl/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase))

(defun gbl/evil-hook ()
  (dolist (mode '(custom-mode
      eshell-mode
      git-rebase-mode
      erc-mode
      circle-server-mode
      circle-chat-mode
      sauron-mode
      term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; (use-package undo-tree)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-i-jump nil)
  ;; (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-tutor)

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                              (bookmarks . "book"))))
;; (use-package command-log-mode
;;   :commands command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-solarized-dark t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


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
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-/" . consult-line)
         ("C-\\" . consult-line-multi)

         :map isearch-mode-map
         ("C-/" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.1
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))


(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; (use-package ;; hydra
;; ;;   :defer t)

(defhydra hydra-window-management (:timeout 4)
  "window management"
  ;; ("c" evil-window-delete "Delete current window" )
  ;; ("o" delete-other-windows "Delete others window")
  ;; ("Q" evil-quit "Quit Emacs")

  ;; ("h" evil-window-left "Move left")
  ;; ("j" evil-window-down "Move down")
  ;; ("k" evil-window-up "Move up")
  ;; ("l" evil-window-right "Move right")

  ;; ("H" evil-window-move-far-left "Move window left")
  ;; ("J" evil-window-move-very-bottom "Move window down")
  ;; ("K" evil-window-move-very-top "Move window up")
  ;; ("L" evil-window-move-far-right "Move window right")

  ("k" evil-window-increase-height "Increase height")
  ("j" evil-window-decrease-height "Decrease height")
  ("l" evil-window-increase-width "Increase width")
  ("h" evil-window-decrease-width "Decrease height")

  ;; ("v" evil-window-vsplit "Vertical split")
  ;; ("s" evil-window-split "Horizontal split")
  ("q" nil  "quit" :exit t))

(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("a" text-scale-adjust "adjust")
  ("q" nil  "quit" :exit t))

(gbl/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "w" '(hydra-window-management/body :which-key "windows"))

(defun gbl/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun gbl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
;;  :straight nil
;  :pin org			
  :commands (org-capture org-agenda)
  :hook (org-mode . gbl/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/personal/organisation/org-files/Tasks.org"
          "~/personal/organisation/org-files/Habits.org"
          "~/personal/organisation/org-files/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/personal/organisation/org-files/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/personal/organisation/org-files/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/personal/organisation/org-files/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/personal/organisation/org-files/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/personal/organisation/org-files/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (gbl/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun gbl/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . gbl/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun gbl/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gbl/org-babel-tangle-config)))

(use-package company-mode
  :straight nil
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 0)
  (company-show-quick-access t)
  (company-show-doc-buffer 0)
  :bind (:map company-active-map
	      ("C-l" . company-complete-selection))
  :init
  (global-company-mode))

(use-package emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-current-column-highlight t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("ejs"  . "\\.ejs\\.")))

(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(use-package eglot)

(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "fish") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun gbl/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . gbl/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :straight nil
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :straight nil
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :straight nil
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :straight nil
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; ;;; Keymap for buffers (Emacs28)
;; (let ((map ctl-x-x-map))
;;   (define-key map "e" #'eval-buffer)
;;   (define-key map "f" #'follow-mode)  ; override `font-lock-update'
;;   (define-key map "r" #'rename-uniquely))

;; (with-eval-after-load 'org
;;   (define-key ctl-x-x-map "i" #'prot-org-id-headlines)
;;   (define-key ctl-x-x-map "h" #'prot-org-ox-html))

;; ;;; Mouse wheel behaviour
;; (use-package mouse
;;   :ensure nil
;;   :config
;;   ;; In Emacs 27+, use Control + mouse wheel to scale text.
;;   (setq mouse-wheel-scroll-amount
;;         '(1
;;           ((shift) . 5)
;;           ((meta) . 0.5)
;;           ((control) . text-scale)))
;;   (setq mouse-drag-copy-region nil)
;;   (setq make-pointer-invisible t)
;;   (setq mouse-wheel-progressive-speed t)
;;   (setq mouse-wheel-follow-mouse t)
;;   (add-hook 'after-init-hook #'mouse-wheel-mode)
;;   (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window))

;; ;;; Scrolling behaviour
;; ;; These four come from the C source code.
;; (setq-default scroll-preserve-screen-position t)
;; (setq-default scroll-conservatively 1) ; affects `scroll-step'
;; (setq-default scroll-margin 0)
;; (setq-default next-screen-context-lines 0)

;; ;;; Delete selection
;; (use-package delsel
;;   :ensure nil
;;   :config
;;   (add-hook 'after-init-hook #'delete-selection-mode))

;; ;;; Tooltips (tooltip-mode)
;; (use-package tooltip
;;   :ensure nil
;;   :config
;;   (setq tooltip-delay 0.5)
;;   (setq tooltip-short-delay 0.5)
;;   (setq x-gtk-use-system-tooltips nil)
;;   (setq tooltip-frame-parameters
;;         '((name . "tooltip")
;;           (internal-border-width . 6)
;;           (border-width . 0)
;;           (no-special-glyphs . t)))
;;   (add-hook 'after-init-hook #'tooltip-mode))

;; ;;; Auto revert mode
;; (use-package autorevert
;;   :ensure nil
;;   :config
;;   (setq auto-revert-verbose t)
;;   (add-hook 'after-init-hook #'global-auto-revert-mode))

;; ;;; Preserve contents of system clipboard
;; (setq save-interprogram-paste-before-kill t)

;; ;;; Newline characters for file ending
;; (setq mode-require-final-newline 'visit-save)

;; ;;; Go to last change
;; (use-package goto-last-change
;;   :config
;;   (define-key global-map (kbd "C-z") #'goto-last-change))

;; ;;; Repeatable key chords (repeat-mode)
;; (use-package repeat
;;   :ensure nil
;;   :config
;;   (setq repeat-on-final-keystroke t)
;;   (setq set-mark-command-repeat-pop t)

;;   (repeat-mode 1))

;; ;;; Make Custom UI code disposable
;; (use-package cus-edit
;;   :ensure nil
;;   :config
;;   ;; Disable the damn thing
;;   (setq custom-file (make-temp-file "emacs-custom-")))

;; ;;; Bongo music or media manager (and prot-bongo.el)
;; (use-package bongo
;;   :config
;;   (setq bongo-default-directory "~/Music/")
;;   (setq bongo-prefer-library-buffers nil)
;;   (setq bongo-insert-whole-directory-trees t)
;;   (setq bongo-logo nil)
;;   (setq bongo-display-track-icons nil)
;;   (setq bongo-display-track-lengths nil)
;;   (setq bongo-display-header-icons nil)
;;   (setq bongo-display-playback-mode-indicator t)
;;   (setq bongo-display-inline-playback-progress nil) ; t slows down the playlist buffer
;;   (setq bongo-join-inserted-tracks nil)
;;   (setq bongo-field-separator (propertize " · " 'face 'shadow))
;;   (setq bongo-mark-played-tracks t)
;;   (setq bongo-vlc-program-name "cvlc")
;;   (bongo-mode-line-indicator-mode -1)
;;   (bongo-header-line-mode -1)
;;   (let ((map global-map))
;;     (define-key map (kbd "C-c b") #'bongo)
;;     (define-key map (kbd "<C-XF86AudioPlay>") #'bongo-pause/resume)
;;     (define-key map (kbd "<C-XF86AudioNext>") #'bongo-next)
;;     (define-key map (kbd "<C-XF86AudioPrev>") #'bongo-previous)
;;     (define-key map (kbd "<C-M-XF86AudioPlay>") #'bongo-play-random)
;;     (define-key map (kbd "<M-XF86AudioPlay>") #'bongo-show)
;;     (define-key map (kbd "<S-XF86AudioNext>") #'bongo-seek-forward-10)
;;     (define-key map (kbd "<S-XF86AudioPrev>") #'bongo-seek-backward-10)
;;     ;; Same as above for the pgtk build of Emacs 29.  Only tested it
;;     ;; with SwayWM.  GNOME 42 may have its own bindings for the
;;     ;; multimedia keys that require changes at the level of the desktop
;;     ;; environment.
;;     (define-key map (kbd "C-<AudioPlay>") #'bongo-pause/resume)
;;     (define-key map (kbd "C-<AudioNext>") #'bongo-next)
;;     (define-key map (kbd "C-<AudioPrev>") #'bongo-previous)
;;     (define-key map (kbd "C-M-<AudioPlay>") #'bongo-play-random)
;;     (define-key map (kbd "M-<AudioPlay>") #'bongo-show)
;;     (define-key map (kbd "S-<AudioNext>") #'bongo-seek-forward-10)
;;     (define-key map (kbd "S-<AudioPrev>") #'bongo-seek-backward-10))
;;   (let ((map bongo-playlist-mode-map))
;;     (define-key map (kbd "n") #'bongo-next-object)
;;     (define-key map (kbd "p") #'bongo-previous-object)
;;     (define-key map (kbd "R") #'bongo-rename-line)
;;     (define-key map (kbd "j") #'bongo-dired-line)       ; Jump to dir of file at point
;;     (define-key map (kbd "J") #'dired-jump)             ; Jump to library buffer
;;     (define-key map (kbd "I") #'bongo-insert-special)))

;; (with-eval-after-load 'bongo
;;   (use-package prot-bongo
;; 	:ensure nil
;; 	:config
;;     (setq prot-bongo-enabled-backends '(mpv vlc))
;;     (setq prot-bongo-playlist-section-delimiter (make-string 30 ?*))
;;     (setq prot-bongo-playlist-heading-delimiter "§")
;;     (setq prot-bongo-playlist-directory
;;           (concat
;;            (file-name-as-directory bongo-default-directory)
;;            (file-name-as-directory "playlists")))
;;     ;; Those set up a few extras: read each function's doc string.  Pass
;;     ;; an argument to undo their effects.
;;     (prot-bongo-enabled-backends)
;;     (prot-bongo-remove-headers)
;;     (prot-bongo-imenu-setup)
;;     (add-hook 'dired-mode-hook #'prot-bongo-dired-library-enable)
;;     (add-hook 'wdired-mode-hook #'prot-bongo-dired-library-disable)
;;     (add-hook 'prot-bongo-playlist-change-track-hook #'prot-bongo-playlist-recenter)
;;     (let ((map bongo-playlist-mode-map))
;;       (define-key map (kbd "C-c C-n") #'prot-bongo-playlist-heading-next)
;;       (define-key map (kbd "C-c C-p") #'prot-bongo-playlist-heading-previous)
;;       (define-key map (kbd "M-n") #'prot-bongo-playlist-section-next)
;;       (define-key map (kbd "M-p") #'prot-bongo-playlist-section-previous)
;;       (define-key map (kbd "M-h") #'prot-bongo-playlist-mark-section)
;;       (define-key map (kbd "M-d") #'prot-bongo-playlist-kill-section)
;;       (define-key map (kbd "g") #'prot-bongo-playlist-reset)
;;       (define-key map (kbd "D") #'prot-bongo-playlist-terminate)
;;       (define-key map (kbd "r") #'prot-bongo-playlist-random-toggle)
;;       (define-key map (kbd "i") #'prot-bongo-playlist-insert-playlist-file))
;;     (let ((map bongo-dired-library-mode-map))
;;       (define-key map (kbd "<C-return>") #'prot-bongo-dired-insert)
;;       (define-key map (kbd "C-c SPC") #'prot-bongo-dired-insert)
;;       (define-key map (kbd "C-c +") #'prot-bongo-dired-make-playlist-file))))

;; (use-package osm
;;   :config
;;   (let ((map global-map))
;;     (define-key map (kbd "C-c O h") #'osm-home)
;;     (define-key map (kbd "C-c O s") #'osm-search)
;;     (define-key map (kbd "C-c O t") #'osm-server)
;;     (define-key map (kbd "C-c O g") #'osm-goto)
;;     (define-key map (kbd "C-c O j") #'osm-bookmark-jump))

;;   ;; Load Org link support
;;   (with-eval-after-load 'org
;;     (require 'osm-ol)))

;; ;;; TMR May Ring (tmr is used to set timers)
;; ;; Read the manual: <https://protesilaos.com/emacs/tmr>.
;; (use-package tmr
;;   :config
;;   (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
;;   (setq tmr-notification-urgency 'normal)
;;   (setq tmr-description-list 'tmr-description-history)

;;   ;; You do not need these if you install the package.
;;   (require 'tmr-notification)
;;   (require 'tmr-tabulated)

;;   (let ((map global-map))
;;     (define-key map (kbd "C-c t t") #'tmr)
;;     (define-key map (kbd "C-c t T") #'tmr-with-description)
;;     (define-key map (kbd "C-c t l") #'tmr-tabulated-view) ; "list timers" mnemonic
;;     (define-key map (kbd "C-c t c") #'tmr-clone)
;;     (define-key map (kbd "C-c t k") #'tmr-cancel)
;;     (define-key map (kbd "C-c t s") #'tmr-reschedule)
;;     (define-key map (kbd "C-c t e") #'tmr-edit-description)
;;     (define-key map (kbd "C-c t r") #'tmr-remove)
;;     (define-key map (kbd "C-c t R") #'tmr-remove-finished)))

(if (boundp 'comp-deferred-complation)
	(setq comp-deferred-complation nil
		  native-comp-deferrede-complation nil))

(use-package general
  :diminish t
  :config (general-evil-setup t))

(use-package which-key
  :diminish t
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha ; order alphabetically
		which-key-sort-uppercase-first nil
		which-key-add-column-adding 1
		which-key-min-display-lines 4
		which-key-idle-delay 0.3 ; wait 0.3 seconds before showing suggestions
		which-key-allow-imprecise-window-fit nil
		which-key-seperator " -> ")
  :config
 (which-key-mode))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
	(setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package smooth-scrolling
  :diminish t
  :config
 (smooth-scrolling-mode 1))

(use-package dimmer
  :config (dimmer-mode))

(use-package hydra
  :diminish t)


(use-package frame
  :diminish t
  :ensure nil
  :config
  (window-divider-mode))

(use-package exwm-edit
  :diminish t)

(use-package winner
  :ensure nil 
  :config
  (add-hook 'after-init-hook  #'winner-mode))


(use-package pdf-tools
  :init
  (pdf-loader-install))

(use-package recentf
  :config
  (recentf-mode))

(use-package sudo-edit
  :diminish t)

(use-package writeroom-mode
  :diminish t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package origami
  :config
  (global-origami-mode))

(use-package harpoon)

(use-package gcmh
  :config
  (gcmh-mode 1)
  (setq gc-cons-threshold (* 2 1000 1000)
	  gc-cons-percentage 0.6))

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

(defun my-nov-window-configuration-change-hook ()
  (my-nov-post-html-render-hook)
  (remove-hook 'window-configuration-change-hook
               'my-nov-window-configuration-change-hook
               t))
(defun my-nov-post-html-render-hook ()
  (if (get-buffer-window)
      (let ((max-width (pj-line-width))
            buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
    (add-hook 'window-configuration-change-hook
              'my-nov-window-configuration-change-hook
              nil t)))

(use-package nov
  :config (setq nov-unzip-program (executable-find "bsdtar")
				nov-unzip-args '("-xC" directory "-f" filename))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width t))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)))

(use-package forge
  :after magit)

(provide 'gbl-emacs-conveniences)

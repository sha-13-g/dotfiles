(if (boundp 'comp-deferred-complation)
    (setq comp-deferred-complation nil
          native-comp-deferrede-complation nil))

;; (setq debug-on-error t
;;            debug-on-signal nil
;;            debug-on-quit nil)

;; (use-package tab-bar
;;   :init (tab-bar-mode)
;;   :custom
;;   (tab-bar-select-tab-modifiers "<control>")) ; Every seven days

(use-package general
    :diminish t)

(use-package devdocs)

(use-package denote
  :config
  (setq denote-file-type "text"))

(use-package tab-bar
  :ensure nil
  :bind (("C-c t" . gbl/hydra-tab-bar/body))
  :config
  (setq tab-bar-tab-hints t
        tab-bar-show 1)
  (tab-bar-mode 1))
  


(use-package go-translate
    :config

  (setq gts-translate-list
        '(("en" "fr")))

  ;; (setq gts-default-translator (gts-translator :engines (gts-bing-engine)))
  ;; (setq gts-default-translator
  ;;       (gts-translator
  ;;        :picker
  ;;        (lambda ()
  ;;          (cond ((equal major-mode 'pdf-view-mode)
  ;;                 (gts-noprompt-picker :texter (gts-current-or-selection-texter)))
  ;;                (t (gts-prompt-picker))))
  ;;        :engines
  ;;        (lambda ()
  ;;          (cond ((equal major-mode 'pdf-view-mode)
  ;;                 (gts-bing-engine))
  ;;                (t (list
  ;;                    (gts-bing-engine)
  ;;                    (gts-google-engine :parser (gts-google-summary-parser))
  ;;                    (gts-google-rpc-engine)))))
  ;;        :render
  ;;        (lambda ()
  ;;          (cond ((equal major-mode 'pdf-view-mode)
  ;;                 (gts-posframe-pop-render))
  ;;                (t (gts-buffer-render))))))

  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine) (gts-google-engine))
         :render (gts-buffer-render))))

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
    :bind-keymap (("C-c p" . projectile-command-map))
    
    :init
    (when (file-directory-p "~/Documents/projects/")
      (setq projectile-project-search-path '("~/Documents/projects/")))
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

;; (use-package exwm-edit
;;   :diminish t)

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
        dashboard-startup-banner "~/Git_repos/dotfiles/bg/flat_icons/saturn-resize.png"
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
    (setq nov-text-width t)
    (setq nov-text-width 80)
    (setq visual-fill-column-center-text t)
    (add-hook 'nov-mode-hook 'visual-line-mode)
    (add-hook 'nov-mode-hook 'visual-fill-column-mode))

;; (use-package justify-kp
;;   :config
;;   (defun my-nov-window-configuration-change-hook ()
;;     (my-nov-post-html-render-hook)
;;     (remove-hook 'window-configuration-change-hook
;;                  'my-nov-window-configuration-change-hook
;;                  t))

;;   (defun my-nov-post-html-render-hook ()
;;     (if (get-buffer-window)
;;         (let ((max-width (pj-line-width))
;;               buffer-read-only)
;;           (save-excursion
;;             (goto-char (point-min))
;;             (while (not (eobp))
;;               (when (not (looking-at "^[[:space:]]*$"))
;;                 (goto-char (line-end-position))
;;                 (when (> (shr-pixel-column) max-width)
;;                   (goto-char (line-beginning-position))
;;                   (pj-justify)))
;;               (forward-line 1))))
;;       (add-hook 'window-configuration-change-hook
;;                 'my-nov-window-configuration-change-hook
;;                 nil t))))
;; (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook))


(use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this)))

(use-package forge
    :after magit)

(provide 'gbl-emacs-conveniences)

(defvar gbl/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun gbl/exwm-logout ()
  (interactive)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "lxsession-logout"))

(defun gbl/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun gbl/exwm-init-hook ()
  (exwm-workspace-switch-create 1))

(defun gbl/kill-panel ()
  (interactive)
  (when gbl/polybar-process
    (ignore-errors
      (kill-process gbl/polybar-process)))
  (setq gbl/polybar-process nil))

(defun gbl/start-panel ()
  (interactive)
  (gbl/kill-panel)
  (setq gbl/polybar-process (start-process-shell-command "polybar" nil "polybar")))

(defun gbl/run-in-bg (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun gbl/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

;; (defun gbl/send-polybar-exwm-workspace ()
  ;; (gbl/send-polybar-hook "exwm-workspace" 1))

(defun gbl/exwm-update-title ()
  (exwm-workspace-rename-buffer (gbl/capitalize-first-char (format "%s: %s"  exwm-class-name exwm-title))))

;; (defun gbl/exwm-update-title ()
;;   (pcase exwm-class-name
;;     ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
;;     ("qutebrowser" (exwm-workspace-rename-buffer (format "QuteBrowser: %s" exwm-title)))
;;     ("Google-chrome" (exwm-workspace-rename-buffer (format "Chrome: %s" exwm-title)))
;;     ("Gimp" (exwm-workspace-rename-buffer (format "Gimp: %s" exwm-title)))
;;     ("Spotify" (exwm-workspace-rename-buffer (format "Spotify: %s" exwm-title)))
;;     ("Alacritty" (exwm-workspace-rename-buffer (format "Alacritty: %s" exwm-title)))
;;     ("mpv" (exwm-workspace-rename-buffer (format "MPV: %s" exwm-title)))))

(defun gbl/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("Arandr" (exwm-floating-toggle-floating))
    ("Google-chrome" (exwm-workspace-move-window 2))
    ("discord" (exwm-workspace-move-window 6))
    ("Spotify" (exwm-workspace-move-window 4))
    ("spotify" (exwm-workspace-move-window 4))
    ("firefox" (exwm-workspace-move-window 2))
    ("Alacritty" (exwm-workspace-move-window 0))
    ("kitty" (exwm-workspace-move-window 0))
    ("TelegramDesktop" (exwm-workspace-move-window 8))
    ("Gimp" (exwm-workspace-move-window 3))
    ("figma-linux" (exwm-workspace-move-window 3))
    ("Pavucontrol" (exwm-floating-toggle-floating))
    ("mpv" (exwm-workspace-move-window 4))
    ("ktouch" (exwm-workspace-move-window 5))
    ("qBittorrent" (exwm-workspace-move-window 5))
    ("VirtualBox Manager" (exwm-workspace-move-window 5))
    ("vlc" (exwm-workspace-move-window 4))))

(use-package desktop-environment
  :after exwm
  :bind (:map desktop-environment-mode-map
			  ("s-l" . nil)
			  ("s-l" . windmove-right))
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")

  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 9)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'gbl/exwm-update-class)

  (add-hook 'exwm-update-title-hook #'gbl/exwm-update-title)

  (add-hook 'exwm-manage-finish-hook #'gbl/configure-window-by-class)

  ;; (add-hook 'exwm-workspace-switch-hook #'gbl/send-polybar-exwm-workspace)
  ;; When EXWM finishes initialization, do some extra setup:
  (add-hook 'exwm-init-hook #'gbl/exwm-init-hook)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-h
      ?\C-c
      ?\C-&

      ?\C--
      ?\C-=

      ?\C-w

      ?\M-&
      ?\M-x
      ?\M-:

      ?\s-h
      ?\s-l
      ?\s-k
      ?\s-j

      ?\s- 

      ?\s-J
      ?\s-K
      ?\s-H
      ?\s-L

      ;; (kbd "<f8>")

      ?\s-a

      ?\s-q
      ?\s-Q

      ?\s-P
      ?\s-N

      ?\s-p
      ?\s-n

      ?\s-e
	  
      ?\s-f
      ?\s-b
      ?\s-B


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
          ([?\M-r] . exwm-reset)

		  ;; Toggle floating windows
		  ([?\s-t] . exwm-floating-toggle-floating)

		  ;; Toggle fullscreen
		  ([?\s-F] . exwm-layout-toggle-fullscreen)

		  ;; ([?\s-w] . exwm-workspace-switch)

		  ;; Toggle modeline
		  ;; ([?\s-m] . exwm-layout-toggle-mode-line)

          ;; Launch applications via shell command
		  ;; ([?\s-d] . (lambda () (interactive) (gbl/launcher "launcher" "")))

          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; Switch workspace
          ([?\M-R] . exwm-input-release-keyboard)

		  ;; Move the current window to the i (1-9) workspace
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "M-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-move-window ,i))))
                    (number-sequence 0 9))

		  
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))

          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))))

  (exwm-input-set-key (kbd "<s-tab>") 'other-window)
  (exwm-input-set-key (kbd "s-v") 'split-window-right)
  (exwm-input-set-key (kbd "<s-return>") 'split-window-below)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'desktop-environment-volume-increment)
  ;; (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'split-window-below)
  ;; (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'split-window-below)

  (gbl/launcher "qutebrowser" "")
  (gbl/launcher "kitty" "")
  (exwm-enable))
  ;; (gbl/launcher "discord" "")
  ;; (gbl/run-in-bg "dunst")
  ;; (gbl/run-in-bg "nm-applet")
  ;; (gbl/run-in-bg "pasystray")
  ;; (gbl/run-in-bg "blueman-applet")
  ;; (gbl/launcher "mpv")
  

(provide 'gbl-emacs-desktop)

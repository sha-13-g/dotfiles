(defun exwm-setup ()
  "Default configuration of EXWM."
  ;; Set the initial workspace number.
  ;; Make class name the buffer name
  ;; Line-editing shortcuts
  (setq exwm-workspace-number 5)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook #'gbl/exwm-update-title)

  (add-hook 'exwm-manage-finish-hook #'gbl/configure-window-by-class)

  (add-hook 'exwm-init-hook #'gbl/exwm-init-hook) ;
  ;; Global keybindings.
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ;; ([?\s-r] . hydra-exwm-move-resize/body)
            ([?\s-i] . exwm-input-toggle-keyboard)
            ([?\s-f] . exwm-layout-toggle-fullscreen)
            ([?\s-F] . exwm-floating-toggle-floating)
            ([?\s-s] . exwm-workspace-switch-to-buffer)
            ([?\s-e] . dired-jump)
            ([?\s-E] . (lambda () (interactive) (dired "~")))
            ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
            ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
            ;; 's-w': Switch workspace.
	    ([?\s-=] . text-scale-increase)
	    ([?\s--] . text-scale-decrease)
            ([?\s-h] . windmove-left)
            ([?\s-m] . windmove-right)
            ([?\s-j] . windmove-down)
            ([?\s-k] . windmove-up)
            ([?\s-\C-h] . shrink-window-horizontally)
            ([?\s-\C-l] . enlarge-window-horizontally)
            ([?\s-\C-j] . shrink-window)
            ([?\s-\C-k] . enlarge-window)
            ([?\s-\C-w] . exwm-workspace-switch)
            ([?\s-H] . evil-window-move-far-left)
            ([?\s-M] . evil-window-move-far-right)
            ([?\s-J] . evil-window-move-very-top)
            ([?\s-K] . evil-window-move-very-bottom)
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-d] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

    (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-h
      ?\M-x
      ?\M-&
      ?\M-:				
      ?\C-j  ;; Next workspace
      ?\C-k  ;; Popper toggle
      ?\C-\     ;; Ctrl+Space
      ?\C-\,))
  
   (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Enable EXWM
  (exwm-enable)
  ;; Configure Ido
  (exwm-config-ido))

(defun exwm-config--fix/ido-buffer-window-other-frame ()
  "Fix `ido-buffer-window-other-frame'."
  (defalias 'exwm-config-ido-buffer-window-other-frame
    (symbol-function #'ido-buffer-window-other-frame))
  (defun ido-buffer-window-other-frame (buffer)
    "This is a version redefined by EXWM.
You can find the original one at `exwm-config-ido-buffer-window-other-frame'."
    (with-current-buffer (window-buffer (selected-window))
      (if (and (derived-mode-p 'exwm-mode)
               exwm--floating-frame)
          ;; Switch from a floating frame.
          (with-current-buffer buffer
            (if (and (derived-mode-p 'exwm-mode)
                     exwm--floating-frame
                     (eq exwm--frame exwm-workspace--current))
                ;; Switch to another floating frame.
                (frame-root-window exwm--floating-frame)
              ;; Do not switch if the buffer is not on the current workspace.
              (or (get-buffer-window buffer exwm-workspace--current)
                  (selected-window))))
        (with-current-buffer buffer
          (when (derived-mode-p 'exwm-mode)
            (if (eq exwm--frame exwm-workspace--current)
                (when exwm--floating-frame
                  ;; Switch to a floating frame on the current workspace.
                  (frame-selected-window exwm--floating-frame))
              ;; Do not switch to exwm-mode buffers on other workspace (which
              ;; won't work unless `exwm-layout-show-all-buffers' is set)
              (unless exwm-layout-show-all-buffers
                (selected-window)))))))))

(defun exwm-config-ido ()
  "Configure Ido to work with EXWM."
  (ido-mode 1)
  (add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame))

  (defun gbl/run-in-bg (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun gbl/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
    (with-eval-after-load 'perspective
	;; Set up perspective names on initial workspaces
	(exwm-workspace-switch-create 0)
	(persp-switch "Terminals")
	(persp-kill "main")

	(exwm-workspace-switch-create 1)

	(exwm-workspace-switch-create 2)
	(persp-switch "Browsers")
	(persp-kill "main")

	(exwm-workspace-switch-create 3)
	(persp-switch "Design")
	(persp-kill "main")

	(exwm-workspace-switch-create 4)
	(persp-switch "Media")
	(persp-kill "main")

	;; Make workspace 1 be the one where we land at startup
	(exwm-workspace-switch-create 1)
	(persp-rename "Editor"))

  ;; Open eshell by default
  ;;(eshell)

  ;; NOTE: The next two are disabled because we now use Polybar!

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Start the Polybar panel
  (gbl/start-panel)

  ;; Launch apps that will run in the background
  (gbl/run-in-bg "dunst")
  (gbl/run-in-bg "qutebrowser")
  (gbl/run-in-bg "vlc")
  (gbl/run-in-bg "alacritty")
  (gbl/run-in-bg "nm-applet")
  (gbl/run-in-bg "pasystray")
  (gbl/run-in-bg "blueman-applet"))

(use-package exwm
  :config
  (start-process-shell-command "setxkbmap" nil "setxkbmap -option 'grp:shifts_toggle, ctrl:swapcaps' -layout 'fr' -variant 'us-azerty' -model 'pc105'") 
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 35)
  ;; (exwm-systemtray-enable)
  (exwm-setup))

(require 'ido)

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

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
  (setq gbl/polybar-process (start-process-shell-command "polybar" nil "polybar panel --config=.config/polybar/config.ini")))

(defun gbl/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun gbl/send-polybar-exwm-workspace ()
  (gbl/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'gbl/send-polybar-exwm-workspace)

(defun gbl/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun gbl/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun gbl/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

(defun gbl/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun gbl/exwm-update-title ()
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-rename-buffer (format "QuteBrowser: %s" exwm-title)))))

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
    ("vlc" (exwm-workspace-move-window 4))))

;; This function should be used only after configuring autorandr!
(defun gbl/update-displays ()
  (gbl/run-in-background "autorandr --change --force")
  (gbl/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(provide 'desktop)

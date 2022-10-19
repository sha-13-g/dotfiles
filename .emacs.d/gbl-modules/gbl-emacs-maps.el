(global-set-key (kbd "C-c & l") 'consult-yasnippet)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "TAB") 'my-insert-tab-char)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Use ESC to quit prompts
(global-set-key (kbd "C-&") 'universal-argument)

(global-set-key (kbd "M-:") 'evil-ex)

(global-set-key (kbd "C-s-f") 'gbl/maximize-window)
(global-set-key (kbd "C-s-b") 'gbl/balance-window)
(global-set-key (kbd "C-s-t") '(lambda () (interactive) (gbl/toggle-transparency)))

(global-set-key (kbd "s-q") 'kill-buffer-slip-window)
(global-set-key (kbd "s-Q") 'delete-window)

(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)

(global-set-key (kbd "s-K") 'windmove-swap-states-up)
(global-set-key (kbd "s-J") 'windmove-swap-states-down)
(global-set-key (kbd "s-L") 'windmove-swap-states-right)
(global-set-key (kbd "s-H") 'windmove-swap-states-left)

(global-set-key (kbd "s-p") 'switch-to-prev-buffer)
(global-set-key (kbd "s-c") 'compile)
(global-set-key (kbd "s-n") 'switch-to-next-buffer)


(global-set-key (kbd "s-N") 'gbl/next-buffer-other-window)
(global-set-key (kbd "s-P") 'gbl/prev-buffer-other-window)

(global-set-key (kbd "s-e") 'dired-jump)

(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-b") 'consult-buffer)
(global-set-key (kbd "s-B") '(lambda () (interactive) (kill-buffer)))

;; (global-set-key (kbd "s-t") '(lambda () (interactive) (gbl/launcher "alacritty" "")))
(global-set-key (kbd "s-m") '(lambda () (interactive) (gbl/launcher "mpv" "")))

(global-set-key (kbd "C-M-j") 'evil-collection-unimpaired-move-text-down)
(global-set-key (kbd "C-M-k") 'evil-collection-unimpaired-move-text-up)
(global-set-key (kbd "C-M-h") 'scroll-other-window-down)
(global-set-key (kbd "C-M-l") 'scroll-other-window)

(global-set-key (kbd "C--") 'desktop-environment-volume-decrement)
(global-set-key (kbd "C-=") 'desktop-environment-volume-increment)
(global-set-key (kbd "C-G") '(lambda () (interactive) (dired "~/git_repo/")))

(with-eval-after-load 'desktop-environment
  (global-set-key (kbd "s-l") #'windmove-right))

(nvmap :prefix gbl/leader
  "SPC l" '(gbl/load-light-theme :which-key "Light theme")
  "SPC d" '(gbl/load-dark-theme :which-key "Dark theme"))

(nvmap :keymaps 'override :prefix gbl/leader
	   gbl/leader	'(execute-extended-command :which-key "M-x")
	   "RET"	'(bookmark-jump :which-key "Bookmarks")
	   ;; "t"	'(toggle-truncate-lines :which-key "Toggle truncated lines")
	   "s"	'(hydra-text-scale/body :which-key "Scale text")
	   "r"	'(writeroom-mode :which-key "Writeroom mode")
	   ","	'(switch-to-buffer :which-key "Switch to buffer")
	   ";"	'(find-file :which-key "Find file"))

(nvmap :prefix gbl/leader
  "g" 	'(:ignore t :which-key "Magit")
  "g c" '(magit-clone :which-key "Magit Clone")
  "g s" '(magit-status :which-key "Magit Status"))

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
	   "w k" '(evil-window-up :which-key "Window up")
	   "w l" '(evil-window-right :which-key "Window right")
	   ;; Window movement
	   "w r" '(gbl/hydra-window-resizer/body :which-key "Resize window")
	   ;; Custom window layout functions
	   "w t" '(gbl/window-split-toggle :which-key "Window split toggle"))

(nvmap :prefix gbl/leader
  "a"   '(:ignore t :which-key "Applications")

  "a b" '(:ignore t :which-key "Browser")
  "a b q" '((lambda () (interactive) (gbl/launcher "qutebrowser" "")) :which-key "Qutebrowser")
  "a b f" '((lambda () (interactive) (gbl/launcher "firefox" "")) :which-key "Firefox")
  "a b g" '((lambda () (interactive) (gbl/launcher "google-chrome-stable" "")) :which-key "Google Chrome")

  "a p" '((lambda () (interactive) (gbl/run-in-bg "polybar-launcher")) :which-key "Polybar")

  "a l" '(:ignore t :which-key "Launcher")
  "a l w" '((lambda () (interactive) (gbl/launcher "wifi-menu" "")) :which-key "Wifi Manager")
  "a l l" '((lambda () (interactive) (gbl/run-in-bg "launcher")) :which-key "App Launcher")
  "a l e" '((lambda () (interactive) (gbl/launcher "emoji" "")) :which-key "Emoji"))

(nvmap :states '(normal visual) :keymaps 'override :prefix gbl/leader
  "d d" '(dired :which-key "Open dired")
  "d D" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "Open dired")
  "d j" '(dired-jump :which-key "Dired jump to current")
  "d p" '(peep-dired :which-key "Peep dired"))

(nvmap :prefix gbl/leader ; Eshell general.el keybindings
  "E h" '(counsel-esh-history :which-key "Eshell history")
  "E s" '(eshell :which-key "Eshell"))

(nvmap :prefix gbl/leader
  "o e" '(elfeed :which-key "Open elfeed")
  "o v" '(vterm :which-key "Open vterm")
  "o s" '(eshell :which-key "Open eshell")
  "o t" '(term :which-key "Open term")
  "o d" '(dired-jump :which-key "Open dired")
  "o a" '(org-agenda :which-key "Open org-agenda")
  "o w" '(eww :which-key "Open eww")
  "o p" '(treemacs :which-key "Open project sidebar"))

(nvmap :prefix gbl/leader
  "p s" '(gbl/shutdown :which-key "Shutdown")
  "p r" '(gbl/restart :which-key "Restart"))

(nvmap :keymaps 'override :prefix gbl/leader
  "m m" '(org-mode :which-key "Restart org mode")
  "m h" '(org-toggle-heading :which-key "Toggle heading")
  "m i" '(org-toggle-item :which-key "Toggle item")
  "m ." '(counsel-org-goto :which-key "Counsel-org goto")
  "m b" '(org-babel-tangle :which-key "Org babel tangle")
  "m t" '(counsel-org-tag :which-key "Counsel org-tag")
  "m T" '(org-tags-view :which-key "Org tags view")
  "m w" '(org-todo-list :which-key "Org todo list"))

(nvmap :prefix gbl/leader
  "w m" '(gbl/hydra-window-move/body :wich-key "Move window"))

(nvmap :prefix gbl/leader
  "SPC v" '(hydra-volume-up/body :which-key "Change volume")
  "SPC t" '(consult-theme :which-key "Load theme")
  "SPC b" '(hydra-brightness-up/body :which-key "Change brightness")
  "SPC m" '(desktop-environment-toggle-mute :which-key "Toggle mute")
  "SPC M" '(desktop-environment-toggle-microphone-mute :which-key "Toggle microphone")
  "SPC s" '(desktop-environment-screenshot :which-key "Take screenshot"))


(nvmap :prefix gbl/leader
  "f" '(:ignore t :which-key "Files")
  "f d" '((lambda () (interactive) (dired "~/git-repos/dotfiles/")) :which-key "Dotfiles")
  "f q" '((lambda () (interactive) (dired "~/.config/qutebrowser/config.py")) :which-key "Qutebrowser File")
  "f f" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "Emacs init.el")
  "f g" '((lambda () (interactive) (find-file "~/git-repos/")) :which-key "Git repos")
  "f m" '((lambda () (interactive) (find-file "~/.emacs.d/gbl-modules/")) :which-key "Emacs Modules")
  "f F" '((lambda () (interactive) (dired "~/documents/files/")) :which-key "Personal Files")
  "f o" '((lambda () (interactive) (dired "~/documents/org/")) :which-key "Org Files")
  "f e" '((lambda () (interactive) (dired "~/.emacs.d/")) :which-key "Emacs Directory")
  "f p" '((lambda () (interactive) (dired "~/documents/personal/")) :which-key "Personal Directory"))

(defhydra gbl/hydra-window-resizer(:timeout 4)
  "This hydra define a set on function that resize a window"
  ("k" (shrink-window 2) "Shrink window vertically")
  ("j" (enlarge-window 2) "Enlarge window vertically")
  ("h" (shrink-window-horizontally 2) "Shrink window horizontally")
  ("l" (enlarge-window-horizontally 2) "Enlarge window horizontally")

  ("K" (exwm-layout-shrink-window 10) "Shrink window vertically")
  ("J" (exwm-layout-enlarge-window 10) "Enlarge window vertically")
  ("H" (exwm-layout-shrink-window-horizontally 10) "Shrink window horizontally")
  ("L" (exwm-layout-enlarge-window-horizontally 10) "Enlarge window horizontally")
  ("q" nil "Quit" :exit t))

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


;(global-set-key (kbd "C-;") 'counsel-switch-buffer)

(defhydra gbl/hydra-window-move (:timeout 5)
  "Hydra for window deplacement"
  ("h" (exwm-floating-move -50 0) "Move back")
  ("l" (exwm-floating-move +50 0) "Move forward")
  ("j" (exwm-floating-move 0 +50) "Move down")
  ("k" (exwm-floating-move 0 -50) "Move up")

  ("H" (exwm-floating-move -200 0) "Move back")
  ("L" (exwm-floating-move +200 0) "Move forward")
  ("J" (exwm-floating-move 0 +200) "Move down")
  ("K" (exwm-floating-move 0 -200) "Move up")
  ("q" nil "Quit" :exit t))

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


(provide 'gbl-emacs-maps)

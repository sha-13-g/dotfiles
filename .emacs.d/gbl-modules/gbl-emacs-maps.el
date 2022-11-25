;; Some convenient maps

(global-set-key (kbd "C-c y l") 'consult-yasnippet)
(global-set-key (kbd "C-c y d") 'yas-describe-tables)
(global-set-key (kbd "C-c y n") 'yas-new-snippet)
(global-set-key (kbd "C-c C-x r") 'org-clock-report)

;; (eval-after-load 'yassnippet
    ;; (define-key yas-minor-mode-map (kbd "C-j") 'yas-expand)) ; 

(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "TAB") 'my-insert-tab-char)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Use ESC to quit prompts

(global-set-key (kbd "s-u") 'universal-argument)
(define-key universal-argument-map (kbd "s-u") #'universal-argument-more)

(global-set-key (kbd "M-:") 'eval-expression)

(global-set-key (kbd "C-s-f") 'gbl/maximize-window)
(global-set-key (kbd "C-s-b") 'gbl/balance-window)
(global-set-key (kbd "C-s-t") #'(lambda () (interactive) (gbl/toggle-transparency)))

(global-set-key (kbd "s-q") 'gbl/kill-buffer-slipt-window)
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
(global-set-key (kbd "s-E") 'sudo-edit)
;; (global-set-key (kbd "s-s") 'gbl/scratch-buffer)

(global-set-key (kbd "C-s-g") 'gbl/get-token)

(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-b") 'consult-buffer)
(global-set-key (kbd "s-B") #'(lambda () (interactive) (kill-buffer)))

;; (global-set-key (kbd "s-t") '(lambda () (interactive) (gbl/launcher "alacritty" "")))
;; (global-set-key (kbd "s-m") '(lambda () (interactive) (gbl/launcher "mpv" "")))

(global-set-key (kbd "C-M-j") 'evil-collection-unimpaired-move-text-down)
(global-set-key (kbd "C-M-k") 'evil-collection-unimpaired-move-text-up)
(global-set-key (kbd "C-M-h") 'scroll-other-window-down)
(global-set-key (kbd "C-M-l") 'scroll-other-window)

(global-set-key (kbd "C--") 'desktop-environment-volume-decrement)
(global-set-key (kbd "C-=") 'desktop-environment-volume-increment)

(with-eval-after-load 'desktop-environment
  (global-set-key (kbd "s-l") #'windmove-right))

(general-define-key  :prefix gbl/leader
 "SPC l" '(gbl/load-light-theme :which-key "Light theme")
 "SPC d" '(gbl/load-dark-theme :which-key "Dark theme"))

(general-define-key  :keymaps 'override :prefix gbl/leader
	   gbl/leader	'(execute-extended-command :which-key "M-x")
	   "RET"	'(bookmark-jump :which-key "Bookmarks")
	   ;; "t"	'(toggle-truncate-lines :which-key "Toggle truncated lines")
	   "s"	'(hydra-text-scale/body :which-key "Scale text")
	   "r"	'(writeroom-mode :which-key "Writeroom mode")
	   ","	'(switch-to-buffer :which-key "Switch to buffer")
	   ";"	'(find-file :which-key "Find file"))

(general-define-key  :prefix gbl/leader
 "g" 	'(:ignore t :which-key "Magit")
 "g c" '(magit-clone :which-key "Magit Clone")
 "g i" '(magit-init :which-key "Magit Init")
 "g p" '(magit-push :which-key "Magit Push")
 "g F" '(magit-pull :which-key "Magit Pull")
 "g s" '(magit-status :which-key "Magit Status"))

(general-define-key  :prefix gbl/leader
	   ;; Window Splits
	   "w" '(:ignore t :which-key "Close window")
	   "w c" '(delete-window :which-key "Close window")
	   "w s" '(split-window-below :which-key "Split window")
	   "w v" '(spit-window-right :which-key "Vsplit window")
	   ;; Window selection
	   "w h" '(windmove-left :which-key "Window left")
	   "w j" '(windmove-down :which-key "Window down")
	   "w k" '(windmove-up :which-key "Window up")
	   "w l" '(windmove-right :which-key "Window right")
	   ;; Window movement
	   "w r" '(gbl/hydra-window-resizer/body :which-key "Resize window")
	   ;; Custom window layout functions
	   "w t" '(gbl/window-split-toggle :which-key "Window split toggle"))

(general-define-key  :prefix gbl/super-leader
 "l"   '(:ignore t :which-key "Applications")
 "l b"   '(:ignore t :which-key "Browsers")

 "l b q" '((lambda () (interactive) (gbl/launcher "qutebrowser" "")) :which-key "Qutebrowser")
 "l b f" '((lambda () (interactive) (gbl/launcher "firefox" "")) :which-key "Firefox")
 "l b c" '((lambda () (interactive) (gbl/launcher "google-chrome-stable" "")) :which-key "Google Chrome")
 "l d" '((lambda () (interactive) (gbl/launcher "discord" "")) :which-key "Discord")
 "l T" '((lambda () (interactive) (gbl/launcher "telegram-desktop" "")) :which-key "Telegram Desktop")
 "l t" '((lambda () (interactive) (gbl/launcher "alacritty" "")) :which-key "Alacritty")
 "l m" '((lambda () (interactive) (gbl/launcher "vlc" "")) :which-key "VLC")
 "l s" '((lambda () (interactive) (gbl/launcher "spotify" "")) :which-key "Spotify")
 "l f" '((lambda () (interactive) (gbl/launcher "figma-linux" "")) :which-key "Figma")
 "l g" '((lambda () (interactive) (gbl/launcher "gimp" "")) :which-key "Gimp")
 "l P" '((lambda () (interactive) (gbl/launcher "pavucontrol" "")) :which-key "Pavucontrol")

 "l p" '((lambda () (interactive) (gbl/start-panel)) :which-key "Start Polybar")
 "l k" '((lambda () (interactive) (gbl/kill-panel)) :which-key "Kill Polybar")

 "l w" '((lambda () (interactive) (gbl/launcher "wifi-menu" "")) :which-key "Wifi Manager")
 "l e" '((lambda () (interactive) (gbl/launcher "emoji" "")) :which-key "Emoji"))

(general-define-key  :states '(normal visual) :keymaps 'override :prefix gbl/leader
 "d d" '(dired :which-key "Open dired")
 "d D" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "Open dired")
 "d j" '(dired-jump :which-key "Dired jump to current")
 "d p" '(peep-dired :which-key "Peep dired"))
                     
(general-define-key  :prefix gbl/leader ; Eshell general.el keybindings
 "E h" '(counsel-esh-history :which-key "Eshell history")
 "E s" '(eshell :which-key "Eshell"))

(general-define-key  :prefix gbl/leader
 "o e" '(elfeed :which-key "Open elfeed")
 "o c" '(org-capture :which-key "Org Capture")
 "o l" '(org-store-link :which-key "Org Store Link")
 "o v" '(vterm :which-key "Open vterm")
 "o s" '(eshell :which-key "Open eshell")
 "o t" '(term :which-key "Open term")
 "o d" '(dired-jump :which-key "Open dired")
 "o a" '(org-agenda :which-key "Open org-agenda")
 "o w" '(eww :which-key "Open eww")
"o p" '(treemacs :which-key "Open project sidebar"))

(general-define-key  :prefix gbl/leader
 "p s" '(gbl/shutdown :which-key "Shutdown")
 "p l" '((lambda () (interactive) (gbl/exwm-logout)) :which-key "Log out")
 "p r" '(gbl/restart :which-key "Restart"))

(general-define-key  :keymaps 'override :prefix gbl/leader
 "m m" '(org-mode :which-key "Restart org mode")
 "m h" '(org-toggle-heading :which-key "Toggle heading")
 "m i" '(org-toggle-item :which-key "Toggle item")
 "m ." '(counsel-org-goto :which-key "Counsel-org goto")
 "m b" '(org-babel-tangle :which-key "Org babel tangle")
 "m t" '(counsel-org-tag :which-key "Counsel org-tag")
 "m T" '(org-tags-view :which-key "Org tags view")
 "m w" '(org-todo-list :which-key "Org todo list"))

(general-define-key  :prefix gbl/leader
 "w m" '(gbl/hydra-window-move/body :wich-key "Move window"))

(general-define-key  :prefix gbl/leader
 "SPC v" '(hydra-volume-up/body :which-key "Change volume")
 "SPC t" '(consult-theme :which-key "Load theme")
 "SPC b" '(hydra-brightness-up/body :which-key "Change brightness")
 "SPC m" '(desktop-environment-toggle-mute :which-key "Toggle mute")
 "SPC M" '(desktop-environment-toggle-microphone-mute :which-key "Toggle microphone")
 "SPC s" '(desktop-environment-screenshot :which-key "Take screenshot"))


(general-define-key  :prefix gbl/leader
 "f" '(:ignore t :which-key "Files")
 "f d" '((lambda () (interactive) (dired "~/Git_repos/dotfiles/")) :which-key "Dotfiles")
 "f T" '((lambda () (interactive) (gts-do-translate)) :which-key "Dotfiles")
 "f F" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-desktop.el")) :which-key "Desktop")
 "f s" '(gbl/scratch-buffer :wich-key "Scratch Buffer")
 "f /" '((lambda () (interactive) (dired "/")) :which-key "Root")
 "f c" '((lambda () (interactive) (dired "~/Git_repos/dotfiles/.config/")) :which-key "Configs")
 "f b" '((lambda () (interactive) (dired "~/Documents/books/")) :which-key "Books")
 "f q" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.config/qutebrowser/config.py")) :which-key "Qutebrowser File")
 "f f" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/init.el")) :which-key "Emacs init.el")
 "f g" '((lambda () (interactive) (dired "~/Git_repos/")) :which-key "Git repos")
 "f h" '((lambda () (interactive) (dired "~/")) :which-key "Home")
 "f o" '((lambda () (interactive) (dired "~/Documents/org/")) :which-key "Org files")

 "f m" '(:ignore t :which-key "Files")
 "f m m" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-maps.el")) :which-key "Maps")
 "f m d" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-dired.el")) :which-key "Dired Config")
 "f m M" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-magit.el")) :which-key "Magit Config")
 "f m D" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-desktop.el")) :which-key "Exwm Config")
 "f m u" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-utils.el")) :which-key "Usefull Command")
 "f m c" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-conveniences.el")) :which-key "Conveniences")
 "f m l" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-langs.el")) :which-key "Dev Config")
 "f m o" '((lambda () (interactive) (find-file "~/Git_repos/dotfiles/.emacs.d/gbl-modules/gbl-emacs-org.el")) :which-key "Org Config")
 
 "f e" '((lambda () (interactive) (dired "~/Git_repos/dotfiles/.emacs.d/")) :which-key "Emacs Directory")
 "f p" '((lambda () (interactive) (dired "~/Documents/personal/")) :which-key "Personal Directory"))

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

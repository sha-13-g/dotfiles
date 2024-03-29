
(global-set-key (kbd "C-c y l") 'consult-yasnippet)
(global-set-key (kbd "C-c y d") 'yas-describe-tables)
(global-set-key (kbd "C-c y n") 'yas-new-snippet)

(global-set-key (kbd "C-c w") 'gbl/search-web)

(global-set-key (kbd "C-c s r") 'replace-regexp)
(global-set-key (kbd "C-c s g") 'consult-grep)
(global-set-key (kbd "C-c s f") 'consult-find)

(global-set-key (kbd "C-c b") 'gbl/hydra-bongo/body)
(global-set-key (kbd "C-c S") 'gbl/scratch-buffer)

(global-set-key (kbd "C-c C-x r") 'org-clock-report)
(global-set-key (kbd "C-c C-x ;") 'org-timer-set-timer)
(global-set-key (kbd "C-c C-x _") 'org-timer-stop)

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c L") #'org-insert-link)

(global-set-key (kbd "C-S-j") #'gbl/scroll-half-page-down)
(global-set-key (kbd "C-S-k") #'gbl/scroll-half-page-up)

(global-set-key (kbd "C-c n n") #'denote)
(global-set-key (kbd "C-c n a") #'denote-keywords-add)
(global-set-key (kbd "C-c n N") #'denote-type)
(global-set-key (kbd "C-c n d") #'denote-date)
(global-set-key (kbd "C-c n s") #'denote-subdirectory)
(global-set-key (kbd "C-c n T") #'denote-template)
(global-set-key (kbd "C-c n r") #'denote-rename-file)

(define-key dired-mode-map (kbd "C-c M-m") #'gbl/dired-mark-regexp)
(define-key dired-mode-map (kbd "C-c C-<return>") #'(lambda () (interactive) (gbl/dired-choose-file qute-filename)))

(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "TAB") 'gbl/my-insert-tab-char)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Use ESC to quit prompts

(global-set-key (kbd "s-u") 'universal-argument)
(define-key universal-argument-map (kbd "s-u") #'universal-argument-more)

(global-set-key (kbd "M-:") 'eval-expression)
(global-set-key (kbd "s-[") 'winner-undo)
(global-set-key (kbd "s-]") 'winner-redo)

(global-set-key (kbd "C-s-f") 'gbl/maximize-window)
(global-set-key (kbd "C-s-b") 'gbl/balance-window)
(global-set-key (kbd "C-s-t") #'(lambda () (interactive) (gbl/toggle-transparency)))

(global-set-key (kbd "s-q") 'gbl/kill-buffer-window)
(global-set-key (kbd "s-Q") 'delete-window)
(global-set-key (kbd "s-o") 'delete-other-windows)


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

(global-set-key (kbd "s-w") 'find-file)
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


(defvar gbl/dictionary-leader "C-c d"
  "Dictionary leader key")

(general-define-key :prefix gbl/dictionary-leader
                    "s" '(dictionary-search :wich-key "Search word")
                    "t" '(gts-do-translate :wich-key "Translate word"))

()
(general-define-key  :prefix gbl/leader
                     "SPC l" '(gbl/load-light-theme :which-key "Light theme")
                     "SPC d" '(gbl/load-dark-theme :which-key "Dark theme")
                     "h"     '(gbl/hydra-harpoon/body :wich-key "Harpoon")) 

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
	                 "w"   '(:ignore t :which-key "Close window")
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
                     "b"   '(:ignore t :which-key "Browsers")

                     "b q" '((lambda () (interactive) (gbl/launcher "qutebrowser" "")) :which-key "Qutebrowser")
                     "b f" '((lambda () (interactive) (gbl/launcher "firefox" "")) :which-key "Firefox")
                     "b c" '((lambda () (interactive) (gbl/launcher "google-chrome-stable" "")) :which-key "Google Chrome")
                     "d"   '((lambda () (interactive) (gbl/launcher "discord" "")) :which-key "Discord")
                     "T"   '((lambda () (interactive) (gbl/launcher "telegram-desktop" "")) :which-key "Telegram Desktop")
                     "t"   '((lambda () (interactive) (gbl/launcher "kitty" "")) :which-key "Alacritty")
                     "v"   '((lambda () (interactive) (gbl/launcher "vlc" "")) :which-key "VLC")
                     "m"   '((lambda () (interactive) (gbl/launcher "hakuneko-desktop" "")) :which-key "MangaReader")
                     "s"   '((lambda () (interactive) (gbl/launcher "spotify-launcher" "")) :which-key "Spotify")
                     "f"   '((lambda () (interactive) (gbl/launcher "figma-linux" "")) :which-key "Figma")
                     "g"   '((lambda () (interactive) (gbl/launcher "gimp" "")) :which-key "Gimp")
                     "P"   '((lambda () (interactive) (gbl/launcher "pavucontrol" "")) :which-key "Pavucontrol")

                     "p"   '((lambda () (interactive) (gbl/start-panel)) :which-key "Start Polybar")
                     "k"   '((lambda () (interactive) (gbl/kill-panel)) :which-key "Kill Polybar")
                           
                     "w"   '((lambda () (interactive) (gbl/launcher "wifi-menu" "")) :which-key "Wifi Manager")
                     "e"   '((lambda () (interactive) (gbl/launcher "emoji" "")) :which-key "Emoji"))

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
                     "f" '(:ignore t :which-key "Go to files or directly")
                     "f d" '((lambda () (interactive) (dired "~/dotfiles/")) :which-key "Dotfiles")
                     "f n" '((lambda () (interactive) (dired "~/documents/notes/")) :which-key "Notes")
                     "f M" '((lambda () (interactive) (dired "~/music/")) :which-key "Music")
                     "f e" '((lambda () (interactive) (dired "~/dotfiles/.emacs.d/")) :which-key "Emacs Directory")
                     "f p" '((lambda () (interactive) (dired "~/personal/")) :which-key "Personal Directory")                     
                     "f /" '((lambda () (interactive) (dired "/")) :which-key "Root")
                     "f c" '((lambda () (interactive) (dired "~/dotfiles/.config/")) :which-key "Configs")
                     "f q" '((lambda () (interactive) (find-file "~/dotfiles/.config/qutebrowser/config.py")) :which-key "Qutebrowser File")
                     "f b" '(gbl/go-to-books :wich-key "Books")                     

                     "f g" '((lambda () (interactive) (dired "~/repos/")) :which-key "Git repos")
                     "f h" '((lambda () (interactive) (dired "~/")) :which-key "Home")
                     "f o" '((lambda () (interactive) (dired "~/org/")) :which-key "Org files")

                     "f f" '(:ignore t :which-key "Emacs Files")
                     "f f i" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/init.el")) :which-key "Emacs init.el")
                     "f f e" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-desktop.el")) :which-key "Emacs desktop.el")                     
                     "f f m" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-maps.el")) :which-key "Maps")
                     "f f d" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-dired.el")) :which-key "Dired Config")
                     "f f M" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-magit.el")) :which-key "Magit Config")
                     "f f u" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-utils.el")) :which-key "Emacs utils.el")
                     "f f c" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-conveniences.el")) :which-key "Emacs conveniences.el")
                     "f f C" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-completion.el")) :which-key "Emacs Completion")                     
                     "f f l" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-langs.el")) :which-key "Dev Config")
                     "f f o" '((lambda () (interactive) (find-file "~/dotfiles/.emacs.d/gbl-modules/gbl-emacs-org.el")) :which-key "Org Config"))

(provide 'gbl-emacs-maps)


;;; Dired file manager and prot-dired.el extras
(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
		 (dired-mode . hl-line-mode))
  :custom ((dired-recursive-copies 'always)
		   (dired-recursive-deletes 'always)
		   (delete-by-moving-to-trash t)
		   (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
		   (dired-dwim-target t)
		   (dired-auto-revert-buffer #'dired-directory-changed-p)
		   (dired-mouse-drag-files t))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map ; VIM keybindings for navigating directories
	"h" 'dired-single-up-directory
	"l" 'dired-find-alternate-file)) ; Emacs 29.1

(use-package peep-dired
  :diminish t)


(use-package dired-aux
  :ensure nil
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))) ; Emacs 28

(use-package dired-x
  :ensure nil
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(use-package prot-dired
  :ensure nil
  :config
  (setq prot-dired-image-viewers '("feh" "sxiv"))
  (setq prot-dired-media-players '("mpv" "vlc"))
  (setq prot-dired-media-extensions
        "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)")
  (setq prot-dired-image-extensions
        "\\.\\(png\\|jpe?g\\|tiff\\)")
  (setq dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
        `((,prot-dired-image-extensions (prot-dired-image-viewer))
          (,prot-dired-media-extensions (prot-dired-media-player))))

  (add-hook 'dired-mode-hook #'prot-dired-setup-imenu)) ; M-s g is `prot-search-grep'

(use-package dired-subtree
  :config
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))) ; S-TAB

(use-package wdired
  :ensure nil
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package image-dired
  :ensure nil
  :config
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))

;;; dired-like mode for the trash (trashed.el)
(use-package trashed
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Ibuffer (dired-like buffer list manager)
(use-package ibuffer
  :ensure nil
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (let ((map ibuffer-mode-map))
    (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
    (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
    (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
    (define-key map (kbd "/ g") #'ibuffer-filter-by-content)))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

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

(use-package dired-single)

(provide 'gbl-emacs-dired)

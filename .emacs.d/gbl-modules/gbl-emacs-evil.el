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
  (evil-mode 1)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'normal "gb" #'gbl/mark-buffer)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-nerd-commenter        ;
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package evil-collection ; Evil collection adds support for non-text edditing applications of EVIL
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org)

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(provide 'gbl-emacs-evil)

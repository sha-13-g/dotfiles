;;; Eshell and gbl-eshell.el
(use-package eshell
  :ensure nil
  :config
  (require 'esh-mode)
  (require 'esh-module)
  (setq eshell-modules-list             ; It works but may need review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix))
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

(use-package gbl-eshell
  :ensure nil
  :config
  (setq gbl-eshell-output-buffer "*Exported Eshell output*")
  (setq gbl-eshell-output-delimiter "* * *")
  (let ((map eshell-mode-map))
    (define-key map (kbd "M-k") #'eshell-kill-input)
    (define-key map (kbd "C-c C-f") #'gbl-eshell-ffap-find-file)
    (define-key map (kbd "C-c C-j") #'gbl-eshell-ffap-dired-jump)
    (define-key map (kbd "C-c C-w") #'gbl-eshell-ffap-kill-save)
    (define-key map (kbd "C-c C->") #'gbl-eshell-redirect-to-buffer)
    (define-key map (kbd "C-c C-e") #'gbl-eshell-export)
    (define-key map (kbd "C-c C-r") #'gbl-eshell-root-dir))
  (let ((map eshell-cmpl-mode-map))
    (define-key map (kbd "C-c TAB") #'gbl-eshell-ffap-insert) ; C-c C-i
    (define-key map (kbd "C-c C-h") #'gbl-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    (define-key map (kbd "M-r") #'gbl-eshell-complete-history)
    (define-key map (kbd "C-c C-d") #'gbl-eshell-complete-recent-dir)
    (define-key map (kbd "C-c C-s") #'gbl-eshell-find-subdirectory-recursive)))

;;; Shell (M-x shell)
(use-package shell
  :ensure nil
  :config
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t))

;;; Tools for manual pages (manpages)
(use-package man
  :ensure nil
  :config
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

;;; Proced (process monitor, similar to `top')
(use-package proced
  :ensure nil
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(use-package gbl-proced
  :ensure nil
  :config
  (gbl-proced-extra-keywords 1))

;;; Pass interface (password-store)
(use-package password-store
  :config
  (setq password-store-time-before-clipboard-restore 30)
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy))

(use-package pass)

(provide 'gbl-emacs-shell)

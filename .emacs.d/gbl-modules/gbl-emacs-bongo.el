(add-hook 'after-change-major-mode-hook #'gbl/bongo-playlist-mode)
(use-package bongo
  :config
  (setq bongo-default-directory "~/Music/")
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-dired-library-mode t))

(provide 'gbl-emacs-bongo)

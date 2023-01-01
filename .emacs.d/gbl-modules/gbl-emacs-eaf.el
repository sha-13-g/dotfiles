(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (require 'eaf-browser)
  (require 'eaf-file-manager)
  (require 'eaf-terminal)
  (require 'eaf-system-monitor)
  (require 'eaf-org-previewer)
  (require 'eaf-markdown-previewer)
  (require 'eaf-git)
  (require 'eaf-browser)
  (require 'eaf-airshare)
  (require 'eaf-video-player)
  (require 'eaf-camera)
  (require 'eaf-file-sender))

  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) 

(provide 'gbl-emacs-eaf)

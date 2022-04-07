;;; init.el -*- lexical-binding: t; -*-

(defvar gbl/default-font-size 90)
(defvar gbl/default-variable-font-size 90)
;; Make frame transparency overridable

;;(auto-complete-mode)

(defvar gbl/frame-transparency '(60 . 90))
(defvar gbl/frame-transparency-beta '(80 . 90))


(column-number-mode)
(global-display-line-numbers-mode t)

 (set-frame-parameter (selected-frame) 'alpha gbl/frame-trans
 (add-to-list 'default-frame-alist `(alpha . ,gbl/frame-trans
 (set-frame-parameter (selected-frame) 'fullscreen 'maximized
((add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :font "Fira Code Retina" :height gbl/default-font-size)




;;(use-package general
;;  :after evil
;;  :config
;;  (general-evil-setup t)
;;  (general-create-definer gbl/leader-keys
;;    :keymaps '(normal insert visual emacs)
;;    :prefix "SPC"
;;    :global-prefix "C-SPC")
;;
;;  (gbl/leader-keys
;;    "t"  '(:ignore t :which-key "Toggles")
;;    "tt" '(counsel-load-theme :which-key "choose theme")
;;    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/emacs.org")))
;;
;;    "o"   '(:ignore t :which-key "org mode")
;;
;;    "oi"  '(:ignore t :which-key "insert")
;;    "oil" '(org-insert-link :which-key "insert link")
;;
;;    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
;;
;;    "os"  '(dw/counsel-rg-org-files :which-key "search notes")
;;
;;    "oa"  '(org-agenda :which-key "Status")
;;    "ot"  '(org-todo-list :which-key "Todos")
;;    "oc"  '(org-capture t :which-key "Capture")
;;    "ox"  '(org-export-dispatch t :which-key "Export")
;;    "w"   '(evil-window-map t :which-key "Window")
;;    "q"   '(delete-window  t :which-key "Quit")
;;    "C"   '(delete-frame  t :which-key "Quit Frame")
;;    "Q"   '(save-buffers-kill-terminal  t :which-key "Quit Emacs")
;;    ","   '(counsel-switch-buffer :which-key "switch buffer"))
;;;; (general-define-key
;;;;  "C-=" 'text-scale-increase
;;;;  "C--" 'text-scale-decrease
;; ;; "M-p" 'package-install
;; ;; "M-h" 'windmove-left
;; ;; "M-l" 'windmove-right
;; ;; "M-k" 'windmove-up
;; ;; "M-j" 'windmove-down
;; ;; "C-M-h" 'evil-window-move-far-left
;; ;; "C-M-l" 'evil-window-move-far-right
;; ;; "C-M-k" 'evil-window-move-very-top
;; ;; "C-M-j" 'evil-window-move-very-bottom
;; ;; "C->" 'evil-window-increase-width
;; ;; "C-<" 'evil-window-decrease-width
;; ;; "C-k" 'evil-window-increase-height
;; ;; "C-j" 'evil-window-decrease-height)
;;
;;(gbl/leader-keys
;;  "b"  '(:ignore t :which-key "Buffer")
;;  "b b"   '(counsel-ibuffer :which-key "Ibuffer")
;;  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
;;  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
;;  "b n"   '(next-buffer :which-key "Next buffer")
;;  "b p"   '(previous-buffer :which-key "Previous buffer")
;;  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
;;  "b s"   '(counsel-switch-buffer :which-key "Switch buffer")
;;  "b K"   '(kill-buffer :which-key "Kill buffer"))
;;
;;(gbl/leader-keys
;;  "e"  '(:ignore t :which-key "Eval")
;;  "e h"   '(counsel-esh-history :which-key "Eshell history")
;;  "e s"   '(eshell :which-key "Eshell")
;;  "e b"   '(eval-buffer :which-key "Eval in buffer")
;;  "e d"   '(eval-defun :which-key "Eval defun")
;;  "e e"   '(eval-expression :which-key "Eval expression")
;;  "e l"   '(eval-last-sexp :which-key "Eval last sexression")
;;  "e r"   '(eval-region :which-key "Eval region"))
;;(gbl/leader-keys
;;  "d"  '(:ignore t :which-key "Dired")
;;  "d d" '(dired :which-key "Open dired")
;;  "d j" '(dired-jump :which-key "Dired jump to current")
;;  "d p" '(peep-dired :which-key "Peep-dired"))
;;(gbl/leader-keys
;;  "f"  '(:ignore t :which-key "File")
;;  "."     '(counsel-find-file :which-key "Find file")
;;  "f f"   '(counsel-find-file :which-key "Find file")
;;  "f r"   '(counsel-recentf :which-key "Recent files")
;;  "f s"   '(save-buffer :which-key "Save file")
;;  "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
;;  "f y"   '(gbl/show-and-copy-buffer-path :which-key "Yank file path")
;;  "f C"   '(copy-file :which-key "Copy file")
;;  "f D"   '(delete-file :which-key "Delete file")
;;  "f R"   '(rename-file :which-key "Rename file")
;;  "f S"   '(write-file :which-key "Save file as...")
;;  "f U"   '(sudo-edit :which-key "Sudo edit file"))
;;
;;(gbl/leader-keys
;;  "r" '(:ignore t :which-key "Register")
;;  "r c"   '(copy-to-register :which-key "Copy to register")
;;  "r f"   '(frameset-to-register :which-key "Frameset to register")
;;  "r i"   '(insert-register :which-key "Insert register")
;;  "r j"   '(jump-to-register :which-key "Jump to register")
;;  "r l"   '(list-registers :which-key "List registers")
;;  "r n"   '(number-to-register :which-key "Number to register")
;;  "r r"   '(counsel-register :which-key "Choose a register")
;;  "r v"   '(view-register :which-key "View a register")
;;  "r w"   '(window-configuration-to-register :which-key "Window configuration to register")
;;  "r +"   '(increment-register :which-key "Increment register")
;;  "r SPC" '(point-to-register :which-key "Point to register"))
;;
;;(gbl/leader-keys
;;  "SPC"   '(counsel-M-x :which-key "M-x")
;;  "c c"   '(compile :which-key "Compile")
;;  "c C"   '(recompile :which-key "Recompile")
;;  "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config"))
;;
;;(gbl/leader-keys
;;  "g"   '(:ignore t :which-key "git")
;;  "gs"  'magit-status
;;  "gd"  'magit-diff-unstaged
;;  "gc"  'magit-branch-or-checkout
;;  "gl"   '(:ignore t :which-key "log")
;;  "glc" 'magit-log-current
;;  "glf" 'magit-log-buffer-file
;;  "gb"  'magit-branch
;;  "gP"  'magit-push-current
;;  "gp"  'magit-pull-branch
;;  "gf"  'magit-fetch
;;  "gF"  'magit-fetch-all
;;  "gr"  'magit-rebase))


(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Rational Emacs loaded in %s."
                     (emacs-init-time))))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
(customize-set-variable 'visible-bell 1)  ; turn off beeps, make them flash!
(customize-set-variable 'large-file-warning-threshold 100000000) ;; change to ~100 MB


;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun rational-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, either via
straight.el or Guix depending on the value of
`rational-prefer-guix-packages'."
  (if rational-prefer-guix-packages
      (unless (featurep package)
        (message "Package '%s' does not appear to be installed by Guix!"))
    (straight-use-package package)))

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Find the user configuration file
;; (defvar rational-config-file (expand-file-name "config.el" rational-config-path)
  ;; "The user's configuration file.")

;; Defines the user configuration var and etc folders
;; and ensure they exist.
;; (defvar rational-config-etc-directory (expand-file-name "etc/" rational-config-path)
  ;; "The user's configuration etc/ folder.")
;; (defvar rational-config-var-directory (expand-file-name "var/" rational-config-path)
  ;; "The user's configuration var/ folder.")

;; (mkdir rational-config-etc-directory t)
;; (mkdir rational-config-var-directory t)

;; Load the user configuration file if it exists
;; (when (file-exists-p rational-config-file)
;;   (load rational-config-file nil 'nomessage))

;; When writing rational-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  (define-auto-insert
    (cons (concat (expand-file-name user-emacs-directory) "modules/rational-.*\\.el")
          "Rational Emacs Lisp Skeleton")
    '("Rational Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "

;; Copyright (C) " (format-time-string "%Y") "
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; " _ "

;;; Code:

(provide '"
      (file-name-base (buffer-file-name))
      ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

;;   The file used by the Customization UI to store value-setting
;; forms in a customization file, rather than at the end of the
;; `init.el' file, is called `custom.el' in Rational Emacs. The file
;; is loaded after this `init.el' file, and after the user `config.el'
;; file has been loaded. Any variable values set in the user
;; `config.el' will be overridden with the values set with the
;; Customization UI and saved in the custom file.
;; (customize-set-variable 'custom-file
;;   (expand-file-name "custom.el" rational-config-path))

;; The custom file will only be loaded if `rational-load-custom-file'
;; is set to a non-nil value in the user's `config.el'.
;; (when rational-load-custom-file
;;   (load custom-file t))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(require 'rational-defaults)
;; (require 'rational-screencast)
(require 'rational-ui)
(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
(require 'rational-windows)
(require 'rational-org)
;; (require 'rational-project)
(require 'rational-updates)

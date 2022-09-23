;;; Unique names for buffers
;; (use-package uniquify
;;   :ensure nil
;;   (setq uniquify-buffer-name-style 'forward)
;;   (setq uniquify-strip-common-suffix t)
;;   (setq uniquify-after-kill-buffer-p t))

;;; Window rules and basic tweaks (window.el)
(defun gbl/dired-left ()
  (interactive)
  (let ((dir (if (eq (vc-root-dir) nil)
				 (dired-noselect default-directory)
			   (dired-noselect (vc-root-dir)))))
	(display-buffer-in-side-window
	 dir `((side . left)
		   (slot . 0)
		   (window-width . 0.2)
		   (window-parameter . ((no-other-window . t)
								(no-delete-other-window . t)
								(mode-line-format . (" "
													 "%b"))))))
	(with-current-buffer dir
	  (rename-buffer "*Dired-side*"))))

(use-package window
  :ensure nil
  :bind ("<f8>" . window-toggle-side-windows)
  :config (setq display-buffer-alist
				`(;; no window
				  ("\\`\\*Async Shell Command\\*\\'"
				   (display-buffer-no-window))
				  ;; top side window

				  ((equal major-mode 'dired-mode)
				   (display-buffer-in-side-window)
				   (window-height . 0.30)
				   (side . left)
				   (slot . 0))

				  ("\\*pytest*"
				   (display-buffer-in-side-window)
				   (window-height . 0.25)
				   (side . bottom)
				   (slot . 1))

				  ("\\*Help*"
				   (display-buffer-in-side-window)
				   (window-width . 0.25)
				   (side . right)
				   (slot . 1))

				  ("\\*eshell*"
				   (display-buffer-in-side-window)
				   (window-height . 0.35)
				   (side . bottom)
				   (slot . 0))

				  ("\\*terminal*"
				   (display-buffer-in-side-window)
				   (window-height . 0.35)
				   (side . bottom)
				   (slot . 0))

				  ("\\*Python*"
				   (display-buffer-in-side-window)
				   (window-height . 0.25)
				   (side . bottom)
				   (slot . -1))

				  ("\\*world-clock.*"
				   (display-buffer-in-side-window)
				   (window-height . 0.16)
				   (side . top)
				   (slot . -1))
				  ((derived-mode . flymake-diagnostics-buffer-mode)
				   (display-buffer-in-side-window)
				   (window-height . 0.16)
				   (side . top)
				   (slot . 0))
				  ((derived-mode . eshell-mode)
				   (display-buffer-in-side-window)
				   (window-height . 0.25)
				   (side . bottom)
				   (slot . -1))
          ((derived-mode . messages-buffer-mode)
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1))
          ((or . ((derived-mode . backtrace-mode)
                  "\\*\\(Warnings\\|Compile-Log\\)\\*"))
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 2))

		  ("\\*compilation\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . -1)
           (window-parameters . ((mode-line-format . none))))
          ;; right side window
          ("\\*keycast\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (window-width . 0.25)
           (side . right)
           (slot . -1)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ;; bottom side window
          ("\\*Org Select\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom))
          ;; below current window
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
           ;; the height is not known in advance.
           (window-height . 0.2))
          ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ;; new frame
          (prot/display-buffer-shell-or-term-p ; see definition below
           (display-buffer-pop-up-frame)
           (pop-up-frame-parameters . ((width . (text-pixels . 640))
                                       (height . (text-pixels . 360))
                                       (tab-bar-lines . 0)
                                       ;; ;; Emacs 29 transparency, if you want:
                                       ;; (alpha-background . 90)
                                       ))
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ((or . ((derived-mode . Man-mode)
                  (derived-mode . woman-mode)
                  "\\*\\(Man\\|woman\\).*"))
           (display-buffer-reuse-window display-buffer-pop-up-frame)
           (pop-up-frame-parameters . ((width . (text-pixels . 640))
                                       (height . (text-pixels . 360))))))))
;; (use-package window
;;   :ensure nil
;;   :config (setq display-buffer-alist
;; 				`(;; no window
;; 				  ("\\`\\*Async Shell Command\\*\\'"
;; 				   (display-buffer-no-window))
;; 				  ;; top side window
;; 				  ("\\*world-clock.*"
;; 				   (display-buffer-in-side-window)
;; 				   (window-height . 0.16)
;; 				   (side . top)
;; 				   (slot . -1))
;;           ((derived-mode . flymake-diagnostics-buffer-mode)
;;            (display-buffer-in-side-window)
;;            (window-height . 0.16)
;;            (side . top)
;;            (slot . 0))
;;           ((derived-mode . messages-buffer-mode)
;;            (display-buffer-in-side-window)
;;            (window-height . 0.16)
;;            (side . top)
;;            (slot . 1))
;;           ((or . ((derived-mode . backtrace-mode)
;;                   "\\*\\(Warnings\\|Compile-Log\\)\\*"))
;;            (display-buffer-in-side-window)
;;            (window-height . 0.16)
;;            (side . top)
;;            (slot . 2))
;;           ;; left side window
;;           ((derived-mode . help-mode) ; See the hooks for `visual-line-mode'
;;            (display-buffer-reuse-mode-window display-buffer-in-side-window)
;;            (window-width . 0.25)
;;            (side . left)
;;            (slot . 0))
;;           ;; right side window
;;           ("\\*keycast\\*"
;;            (display-buffer-in-side-window)
;;            (dedicated . t)
;;            (window-width . 0.25)
;;            (side . right)
;;            (slot . -1)
;;            (window-parameters . ((no-other-window . t)
;;                                  (mode-line-format . none))))
;;           ;; bottom side window
;;           ("\\*Org Select\\*"
;;            (display-buffer-in-side-window)
;;            (dedicated . t)
;;            (side . bottom)
;;            (slot . 0)
;;            (window-parameters . ((mode-line-format . none))))
;;           ;; bottom buffer (NOT side window)
;;           ("\\*Embark Actions\\*"
;;            (display-buffer-reuse-mode-window display-buffer-at-bottom)
;;            (window-height . fit-window-to-buffer)
;;            (window-parameters . ((no-other-window . t)
;;                                  (mode-line-format . none))))
;;           ("\\*\\(Output\\|Register Preview\\).*"
;;            (display-buffer-reuse-mode-window display-buffer-at-bottom))
;;           ;; below current window
;;           ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
;;            (display-buffer-reuse-mode-window display-buffer-below-selected)
;;            ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
;;            ;; the height is not known in advance.
;;            (window-height . 0.2))
;;           ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
;;            (display-buffer-reuse-mode-window display-buffer-below-selected)
;;            (window-height . fit-window-to-buffer))
;;           ;; new frame
;;           (prot/display-buffer-shell-or-term-p ; see definition below
;;            (display-buffer-pop-up-frame)
;;            (pop-up-frame-parameters . ((width . (text-pixels . 640))
;;                                        (height . (text-pixels . 360))
;;                                        (tab-bar-lines . 0)
;;                                        ;; ;; Emacs 29 transparency, if you want:
;;                                        ;; (alpha-background . 90)
;;                                        ))
;;            (window-parameters . ((no-other-window . t)
;;                                  (mode-line-format . none))))
;;           ((or . ((derived-mode . Man-mode)
;;                   (derived-mode . woman-mode)
;;                   "\\*\\(Man\\|woman\\).*"))
;;            (display-buffer-reuse-window display-buffer-pop-up-frame)
;;            (pop-up-frame-parameters . ((width . (text-pixels . 640))
;;                                        (height . (text-pixels . 360)))))))

;;   (defun prot/display-buffer-shell-or-term-p (buffer &rest _)
;;     "Check if BUFFER is a shell or terminal.
;; This is a predicate function for `buffer-match-p', intended for
;; use in `display-buffer-alist'."
;;     (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name buffer))
;;       (with-current-buffer buffer
;;         ;; REVIEW 2022-07-14: Is this robust?
;;         (and (or (not (derived-mode-p 'message-mode))
;;                  (not (derived-mode-p 'text-mode)))
;;              (or (derived-mode-p 'eshell-mode)
;;                  (derived-mode-p 'shell-mode)
;;                  (derived-mode-p 'comint-mode)
;;                  (derived-mode-p 'fundamental-mode))))))

;;   (setq window-combination-resize t)
;;   (setq even-window-sizes 'height-only)
;;   (setq window-sides-vertical nil)
;;   (setq switch-to-buffer-in-dedicated-window 'pop)

;;   (add-hook 'help-mode-hook #'visual-line-mode)
;;   (add-hook 'custom-mode-hook #'visual-line-mode)

;;   (let ((map global-map))
;;     (define-key map (kbd "C-x <down>") #'next-buffer)
;;     (define-key map (kbd "C-x <up>") #'previous-buffer)
;;     (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
;;     (define-key map (kbd "C-x C-p") #'previous-buffer) ; override `mark-page'
;;     (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
;;     (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
;;     (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
;;     (define-key map (kbd "C-x +") #'balance-windows-area)
;;     (define-key map (kbd "C-x }") #'enlarge-window)
;;     (define-key map (kbd "C-x {") #'shrink-window)
;;     (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
;;     (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'
;;   (let ((map resize-window-repeat-map))
;;     (define-key map ">" #'enlarge-window-horizontally)
;;     (define-key map "<" #'shrink-window-horizontally)))

;;; Window history (winner-mode)
;; (use-package winner
;;   :ensure nil
;;   (add-hook 'after-init-hook #'winner-mode)

  ;; ;; NOTE 2021-07-31: Those are superseded by the commands
  ;; ;; `prot-tab-winner-undo' and `prot-tab-winner-redo' in prot-tab.el
  ;; ;; (search this document).
  ;; (let ((map global-map))
  ;;   (define-key map (kbd "C-x <right>") #'winner-redo)
  ;;   (define-key map (kbd "C-x <left>") #'winner-undo))
  ;; )

;;; Directional window motions (windmove)
;; (use-package windmove
;;   :ensure nil
;;   (setq windmove-create-window nil)     ; Emacs 27.1
;;   (let ((map global-map))
;;     ;; Those override some commands that are already available with
;;     ;; C-M-u, C-M-f, C-M-b.
;;     (define-key map (kbd "C-M-<up>") #'windmove-up)
;;     (define-key map (kbd "C-M-<right>") #'windmove-right)
;;     (define-key map (kbd "C-M-<down>") #'windmove-down)
;;     (define-key map (kbd "C-M-<left>") #'windmove-left)
;;     (define-key map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
;;     (define-key map (kbd "C-M-S-<right>") #'windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
;;     (define-key map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
;;     (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left)))

;;; Tabs for window layouts (tab-bar.el and prot-tab.el)
;; (use-package tab-bar
;;   (setq tab-bar-close-button-show nil)
;;   (setq tab-bar-close-last-tab-choice nil)
;;   (setq tab-bar-close-tab-select 'recent)
;;   (setq tab-bar-new-tab-choice t)
;;   (setq tab-bar-new-tab-to 'right)
;;   (setq tab-bar-position nil)
;;   (setq tab-bar-show nil)
;;   (setq tab-bar-tab-hints nil)
;;   (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
;; 
;;   (tab-bar-mode -1)                     ; see `prot-tab-status-line'
;; 
;;   ;; Same concept as `winner-mode'.  See the `prot-tab-winner-undo' and
;;   ;; its counterpart.
;;   (tab-bar-history-mode 1))
;; 
;; (use-package prot-tab
;;   (setq prot-tab-tab-select-num-threshold 3)
;;   (setq tab-bar-format                    ; Emacs 28
;;         '(tab-bar-format-tabs-groups
;;           tab-bar-format-align-right
;;           tab-bar-format-global))
;; 
;;   (add-hook 'after-init-hook #'prot-tab-status-line)
;; 
;;   (let ((map global-map))
;;     (define-key map (kbd "C-x <right>") #'prot-tab-winner-redo)
;;     (define-key map (kbd "C-x <left>") #'prot-tab-winner-undo)
;;     (define-key map (kbd "<f8>") #'prot-tab-status-line) ; unopinionated alternative: `prot-tab-bar-toggle'
;;     (define-key map (kbd "C-x t t") #'prot-tab-select-tab-dwim)))
;; 
;; ;; This is only included as a reference.
;; (use-package tab-line
;;   (global-tab-line-mode -1))

;;; Transposition and rotation of windows
;; (use-package transpose-frame
;;   :bind  ("C-x M-r" . rotate-frame-clockwise))

(provide 'gbl-emacs-window)

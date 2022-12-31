;; Functions definition

;;; code:

;; (defub gbl/sudo-edit ()
;;   "Commands that "
;;   (interactive))
(defun gbl/dired-choose-file (&optional file-handler)
  (interactive "MSave filename to: ")
  (let ((filename (dired-get-filename)))
    (if file-handler
        (progn
          (find-file file-handler)
          (insert filename)
          (save-buffer)
          (gbl/kill-buffer-window)))))

(defun gbl/scroll-half-page-down()
  (interactive)
  (forward-line 20)
  (recenter nil nil))

(defun gbl/search-web (query)
  (interactive "MQuery: ")
  (gbl/launcher "qutebrowser" query))

(defun gbl/scroll-half-page-up()
  (interactive)
  (forward-line -20)
  (recenter nil nil))

(defun gbl/dired-mark-regexp()
  (interactive)
  (call-interactively #'dired-mark-files-regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))


(defun gbl/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun gbl/maximize-window ()
  "This Command maximize the curent windowk. It is bound to C-s-f"
  (interactive)
  (maximize-window))

(defun gbl/dotfile-push ()
  (interactive)
  (dired-do-copy "~/.emacs.d/init.el" ))

;; (defun gbl/get-token ()
;;   (interactive)
;;  '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)
(defun gbl/bongo-go-music ()
  (interactive)
  (bongo)
  (kill-buffer)
  (tab-new)
  (tab-rename "Music")
  (dired "~/Music/"))

(defun gbl/go-to-books()
  (interactive)
  (tab-new)
  (tab-rename "Books")
  (dired "~/Documents/books/"))

(defun gbl/indent-buffer ()
  "Mark the all in the current buffer"
  (interactive)
  (save-excursion
    (push-mark (point-min) nil t)
    (goto-char (point-max))
    (call-interactively #'indent-region)))

(defun gbl/mark-buffer ()
  "Mark the all in the current buffer"
  (interactive)
  (push-mark (point-min) nil t)
  (goto-char (point-max)))

(defun gbl/get-token ()
  (interactive)
  (save-excursion
    (find-file "~/Documents/org/git_tokken")
    (call-interactively 'set-mark-command)
    (end-of-visual-line)
    (call-interactively 'kill-ring-save)
    (kill-buffer (current-buffer))
    (delete-window)))

(defun gbl/bongo-buffer ()
  (interactive)
  (get-buffer-create "*Bongo Playlist*")
  (switch-to-buffer "*Bongo Playlist*")
  (bongo-playlist-mode))

(defun gbl/scratch-buffer ()
  (interactive)
  (get-buffer-create "*scratch*")
  (switch-to-buffer "*scratch*")
  (lisp-interaction-mode))

(defun gbl/balance-window ()
  (interactive)
  (balance-windows))

(defun gbl/next-buffer-other-window()
  "Commands that split the current window & select the next buffer"
  (interactive)
  (split-window-right)
  (other-window 1)
  (next-buffer))

(defun gbl/dired-mode (arg)
  (interactive)
  (if ))

(defun gbl/prev-buffer-other-window()
  "Commands that split the current window & select the next buffer"
  (interactive)
  (split-window-right)
  (other-window 1)
  (previous-buffer))

(defun gbl/maximaze-other-window()
  "Command that maximaze other window"
  (interactive)
  (maximize-window (next-window))
  (other-window 1))

(defun gbl/initial-position-window ()
  "Move Exwm window by {x,y}"
  (interactive)
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

(defun gbl/move-window ()
  "Move Exwm window by {x,y}"
  (interactive)
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))

    (exwm-floating-move)))

(defun gbl/launcher (program &optional program-args)
  "Command to launch the app in PROGRAM with PROGRAM-ARGS."
  (interactive)
  (if (equal program-args "")
      (start-process-shell-command program nil program)
    (start-process "" nil program program-args)))

(defun gbl/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun gbl/my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))


(defhydra gbl/hydra-harpoon ()
  "Harpoon"
  ("j" (harpoon-go-to-1) "Go to 1")
  ("f" (harpoon-go-to-2) "Go to 2")
  ("k" (harpoon-go-to-3) "Go to 3")
  ("d" (harpoon-go-to-4) "Go to 4")
  ("l" (harpoon-go-to-5) "Go to 5")
  ("s" (harpoon-go-to-6) "Go to 6")
  ("m" (harpoon-go-to-7) "Go to 7")
  ("q" (harpoon-go-to-8) "Go to 8")  
  ("u" (harpoon-go-to-9) "Go to 9")
  ("M" (harpoon-toggle-quick-menu) "Menu")
  ("<space>" (harpoon-add-file) "Add current file")
  ("<escape>" nil "Quit" :exit t))


(defhydra gbl/hydra-bongo(:timeout 10)
  "This hydra define a set on function that resize a window"
  ("s" (bongo-start/stop) "Bongo Start/Stop")
  ("l" (bongo-next) "Bongo Next")
  ("h" (bongo-previous) "Bongo Prev")
  ("P" (bongo-playlist) "Bongo Playlist")
  ("b" (gbl/bongo-buffer) "Bongo Buffer")
  ("g" (gbl/bongo-go-music) "Bongo go Music")

  ("p" (bongo-pause/resume) "Bongo Pause/Resume")
  ("r" (bongo-replay-current) "Bongo Replay Current")
  ("H" (bongo-seek-backward-3) "Bongo Backward 3")
  ("L" (bongo-seek-forward-3) "Bongo Forward 3")
  ("a" (bongo-append-enqueue) "Bongo Append to queue")
  ("q" nil "Quit" :exit t))

(defun gbl/dired-bongo ()
  (when (equal dired-directory "~/Music/")
    (bongo-dired-library-mode 1)))


(defun gbl/bongo-playlist-mode()
  (when (equal major-mode 'bongo-playlist-mode)))

(defun gbl/set-transparency (number)
  "Set frame transparency by (NUMBER . NUMBER)."
  (interactive "p")
  (set-frame-parameter (selected-frame) `alpha `(,number . ,number)))

(defun gbl/hide-minor-modes ()
  "A function that shortten minor mode to a character."
  (interactive)
  (dolist (minor-mode minor-mode-list)
    (diminish minor-mode)))

(defun gbl/toggle-transparency ()
  "Toggle the transparency of the current frame."
  (interactive)
  (if (equal gbl/frame-transparency-v '(70 . 70))
      (set-frame-parameter (selected-frame) 'alpha 
                           (setq gbl/frame-transparency-v '(100 . 100)))
    (set-frame-parameter (selected-frame) 'alpha
                         (setq gbl/frame-transparency-v '(70 . 70)))))

(defun gbl/kill-buffer-window ()
  "a function for delete slip window and kill the buffer."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun gbl/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

(defun gbl/shutdown ()
  (interactive)
  (start-process-shell-command "systemctl" nil "systemctl poweroff"))

(defun gbl/restart ()
  (interactive)
  (start-process-shell-command "systemctl" nil "systemctl reboot"))

(defun gbl/increase-transparency ()
  "Increase transparency by (1 . 1)"
  (interactive)
  (cdr (car (cdr (cdr default-frame-alist))))
  (setcar '(1 . 1)))

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

(defhydra hydra-volume-up (:timeout 4)
  "Configure Volume"
  ("j" desktop-environment-volume-decrement "down")
  ("k" desktop-environment-volume-increment "up")
  ("q" nil "quit" :exit t))

(defhydra hydra-brightness-up (:timeout 4)
  "Configure Brightness"
  ("k" desktop-environment-brightness-increment "up")
  ("j" desktop-environment-brightness-decrement "down")
  ("q" nil "quit" :exit t))

(defhydra gbl/hydra-tab-bar(:timeout 10)
  "This hydra define a set of tab-bar operation"
  ("h" (tab-previous) "Goto Previous Tab")
  ("l" (tab-next) "Goto Next Tab")
  ("r" (call-interactively #'tab-rename) "Rename Tab")
  ("s" (call-interactively #'tab-switch) "Rename Tab")
  ("n" (tab-new) "Create New Tab")
  ("c" (tab-close) "Close The Current Tab")
  ("m" (call-interactively #'tab-move) "Move Tab Forward")
  ("M" (call-interactively #'tab-move-to) "Move Tab To")
  ("g" (call-interactively #'tab-group) "Add Tab to Group")
  ("q" nil "Quit" :exit t))

(provide 'gbl-emacs-utils)

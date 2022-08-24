;; Functions definition

;;; code:

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

(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

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

(defun kill-buffer-slip-window ()
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
  (start-process-shell-command "shutdown" nil "shutdown now"))

(defun gbl/restart ()
  (interactive)
  (start-process-shell-command "shutdown" nil "shutdown -r now"))

(provide 'gbl-utils)


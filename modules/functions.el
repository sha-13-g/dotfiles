;; Functions definition

;;; code:
(defun gbl/launcher (prog)
  "Command to launch the app in PROG."
  (interactive)
  (start-process-shell-command prog nil prog))

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
  (start-process-shell-command shutdown nil "shutdown now"))

(defun gbl/restart ()
  (interactive)
  (start-process-shell-command shutdown nil "shutdown -r now"))

(provide 'functions)


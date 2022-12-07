;;; Client-agnostic email settings (and gbl-mail.el)
(use-package auth-source
  :ensure nil
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq user-full-name "Ganfina Brice")
  (setq user-mail-address "ganfinab@protonmail.com"))

(use-package mm-encode
  :ensure nil
  :config
  (setq mm-encrypt-option nil) ; use 'guided if you need more control
  (setq mm-sign-option nil))   ; same

(use-package mml-sec
  :ensure nil
  :config
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

(use-package message
  :ensure nil
  :config
  (setq mail-user-agent 'message-user-agent)
  (setq mail-header-separator (purecopy "*****"))
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)
  (setq message-mail-user-agent t)      ; use `mail-user-agent'
  (setq mail-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n")
  (setq message-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n")

  ;; Instead of using a citation format like this:
  ;;
  ;; On DATE, PERSON wrote:
  ;; > MESSAGE
  ;;
  ;; I disable the citation line and `message-ignored-cited-headers' to
  ;; get this template instead:
  ;;
  ;; > From: PERSON
  ;; > Date: DATE
  ;; >
  ;; > MESSAGE
  ;;
  ;; (setq message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n")
  ;; (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-function nil)
  (setq message-ignored-cited-headers nil) ; default is "." for all headers

  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (add-hook 'message-setup-hook #'message-sort-headers))

(use-package gnus-dired ; does not require `gnus'
  :ensure nil
  :config
  (add-hook 'dired-mode-hook #'gnus-dired-mode))

(use-package gbl-mail
  :ensure nil
  :config
  ;; NOTE 2021-05-14: This is a generic indicator for new mail in the
  ;; maildir.  As I now use notmuch (see relevant section in this
  ;; document) I have an alternative approach in gbl-notmuch.el.
  (setq gbl-mail-maildir-path-regexp "~/.mail/*/Inbox/new/") ; shell regexp
  (setq gbl-mail-mode-line-indicator-commands
        '(notmuch-refresh-this-buffer))
  ;; mode line indicator with the number of new mails
  (gbl-mail-mail-indicator -1))

;;; Notmuch (mail indexer and mail user agent (MUA))
;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs.  Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(use-package notmuch
  :ensure nil
  :config

;;; Account settings
  (let ((prv (gbl-mail-auth-get-field "prv" :user))
        (pub (gbl-mail-auth-get-field "pub" :user))
        (inf (gbl-mail-auth-get-field "inf" :user)))
    (setq notmuch-identities
          (mapcar (lambda (str)
                    (format "%s <%s>" user-full-name str))
                  (list prv pub inf)))
    (setq notmuch-fcc-dirs
          `((,prv . "prv/Sent")
            (,inf . "inf/Sent")
            (,pub . "pub/Sent"))))

;;;; General UI
  (setq notmuch-show-logo nil)
  (setq notmuch-column-control 1.0)
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 20)
  (setq notmuch-hello-thousands-separator "")
  (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
  (setq notmuch-show-all-tags-list t)

;;;; Search
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flag" . notmuch-search-flagged-face)))
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        `(( :name "inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "unread all"
            :query "tag:unread not tag:archived"
            :sort-order newest-first
            :key ,(kbd "U"))
          ( :name "references"
            :query "tag:ref not tag:archived"
            :sort-order newest-first
            :key ,(kbd "r"))
          ( :name "todo"
            :query "tag:todo not tag:archived"
            :sort-order newest-first
            :key ,(kbd "t"))
          ( :name "mailing lists"
            :query "tag:list not tag:archived"
            :sort-order newest-first
            :key ,(kbd "m"))
          ;; Emacs
          ( :name "emacs-devel"
            :query "(from:emacs-devel@gnu.org or to:emacs-devel@gnu.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "e d"))
          ( :name "emacs-orgmode"
            :query "(from:emacs-orgmode@gnu.org or to:emacs-orgmode@gnu.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "e o"))
          ( :name "emacs-bugs"
            :query "'to:\"/*@debbugs.gnu.org*/\"' not tag:archived"
            :sort-order newest-first :key ,(kbd "e b"))
          ( :name "emacs-humanities"
            :query "(from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org) not tag:archived"
            :sort-order newest-first :key ,(kbd "e h"))
          ( :name "emacs-elpher"
            :query "(from:~michel-slm/elpher@lists.sr.ht or to:~michel-slm/elpher@lists.sr.ht) not tag:archived"
            :sort-order newest-first :key ,(kbd "e e"))
          ;; Others
          ( :name "notmuch"
            :query "(from:notmuch@notmuchmail.org or to:notmuch@notmuchmail.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "on"))
          ( :name "sourcehut"
            :query "(from:~sircmpwn/sr.ht-discuss@lists.sr.ht or to:~sircmpwn/sr.ht-discuss@lists.sr.ht) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "os"))))

;;;; Tags
  (setq notmuch-archive-tags '("-inbox" "+archived"))
  (setq notmuch-message-replied-tags '("+replied"))
  (setq notmuch-message-forwarded-tags '("+forwarded"))
  (setq notmuch-show-mark-read-tags '("-unread"))
  (setq notmuch-draft-tags '("+draft"))
  (setq notmuch-draft-folder "drafts")
  (setq notmuch-draft-save-plaintext 'ask)
  ;; ;; NOTE 2021-06-18: See an updated version in the `gbl-notmuch'
  ;; ;; section below.
  ;; (setq notmuch-tagging-keys
  ;;       `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
  ;;         (,(kbd "c") ("+archived" "-inbox" "-list" "-todo" "-ref" "-unread") "Complete and archive")
  ;;         (,(kbd "d") ("+del" "-inbox" "-archived" "-unread") "Mark for deletion")
  ;;         (,(kbd "f") ("+flag" "-unread") "Flag as important")
  ;;         ;; (,(kbd "r") notmuch-show-mark-read-tags "Mark as read")
  ;;         (,(kbd "r") ("+ref" "-unread") "Reference for the future")
  ;;         (,(kbd "s") ("+spam" "+del" "-inbox" "-unread") "Mark as spam")
  ;;         (,(kbd "t") ("+todo" "-unread") "To-do")
  ;;         (,(kbd "u") ("+unread") "Mark as unread")))
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged))))
  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

;;;; Email composition
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil) ; TODO 2021-05-12: Review hidden headers
  (setq notmuch-address-command nil)    ; FIXME 2021-05-13: Make it work with EBDB
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                "pi[èe]ce\s+jointe?\\|"
                "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))

;;;; Reading messages
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-save-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-cqitation-lines-prefix count
          notmuch-wash-citation-lines-suffix count))
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

;;;; Hooks and key bindings
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook 'notmuch-hl-line-mode) ; Check my `lin' package
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))

  (let ((map global-map))
    (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'
  (let ((map notmuch-search-mode-map)) ; I normally don't use the tree view, otherwise check `notmuch-tree-mode-map'
    (define-key map (kbd "/") #'notmuch-search-filter) ; alias for l
    (define-key map (kbd "r") #'notmuch-search-reply-to-thread) ; easier to reply to all by default
    (define-key map (kbd "R") #'notmuch-search-reply-to-thread-sender))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "r") #'notmuch-show-reply) ; easier to reply to all by default
    (define-key map (kbd "R") #'notmuch-show-reply-sender))
  (define-key notmuch-hello-mode-map (kbd "C-<tab>") nil))

(use-package gbl-notmuch
  :ensure nil
  :config
  ;; Those are for the actions that are available after pressing 'k'
  ;; (`notmuch-tag-jump').  For direct actions, refer to the key
  ;; bindings below.
  (setq notmuch-tagging-keys
        `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
          (,(kbd "c") gbl-notmuch-mark-complete-tags "Complete and archive")
          (,(kbd "d") gbl-notmuch-mark-delete-tags "Mark for deletion")
          (,(kbd "f") gbl-notmuch-mark-flag-tags "Flag as important")
          (,(kbd "s") gbl-notmuch-mark-spam-tags "Mark as spam")
          (,(kbd "t") gbl-notmuch-mark-todo-tags "To-do")
          (,(kbd "x") gbl-notmuch-mark-reference-tags "Reference for the future")
          (,(kbd "r") ("-unread") "Mark as read")
          (,(kbd "u") ("+unread") "Mark as unread")))

  (add-to-list 'notmuch-tag-formats
               '("encrypted" (propertize tag 'face 'gbl-notmuch-encrypted-tag)))
  (add-to-list 'notmuch-tag-formats
               '("sent" (propertize tag 'face 'gbl-notmuch-sent-tag)))
  (add-to-list 'notmuch-tag-formats
               '("ref" (propertize tag 'face 'gbl-notmuch-ref-tag)))
  (add-to-list 'notmuch-tag-formats
               '("todo" (propertize tag 'face 'gbl-notmuch-todo-tag)))
  (add-to-list 'notmuch-tag-formats
               '("spam" (propertize tag 'face 'gbl-notmuch-spam-tag)))

  ;; NOTE 2021-05-14: I have an alternative method of finding new mail
  ;; in a maildir tree by using the find command.  It is somewhat
  ;; simplistic, though it worked just fine: see gbl-mail.el.  I prefer
  ;; this implementation instead, as it leverages notmuch and so I can
  ;; pass arbitrary search terms to it.
  (setq gbl-notmuch-mode-line-count-args "tag:unread and tag:inbox")
  (setq gbl-notmuch-mode-line-indicator-commands
        '(notmuch notmuch-refresh-this-buffer))
  ;; Mode line indicator with the number of new mails.
  (gbl-notmuch-mail-indicator 1)

  (dolist (fn '(gbl-notmuch-check-valid-sourcehut-email
                gbl-notmuch-ask-sourcehut-control-code))
    (add-hook 'notmuch-mua-send-hook fn))

  (let ((map notmuch-search-mode-map))
    (define-key map (kbd "a") nil) ; the default is too easy to hit accidentally
    (define-key map (kbd "A") #'notmuch-search-archive-thread)
    (define-key map (kbd "D") #'gbl-notmuch-search-delete-thread)
    (define-key map (kbd "T") #'gbl-notmuch-search-todo-thread)
    (define-key map (kbd "X") #'gbl-notmuch-search-reference-thread)
    (define-key map (kbd "C") #'gbl-notmuch-search-complete-thread)
    (define-key map (kbd "S") #'gbl-notmuch-search-spam-thread)
    (define-key map (kbd "g") #'gbl-notmuch-refresh-buffer))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "a") nil) ; the default is too easy to hit accidentally
    (define-key map (kbd "A") #'notmuch-show-archive-message-then-next-or-next-thread)
    (define-key map (kbd "D") #'gbl-notmuch-show-delete-message)
    (define-key map (kbd "T") #'gbl-notmuch-show-todo-message)
    (define-key map (kbd "X") #'gbl-notmuch-show-reference-message)
    (define-key map (kbd "C") #'gbl-notmuch-show-complete-message)
    (define-key map (kbd "S") #'gbl-notmuch-show-spam-message))
  (define-key notmuch-show-stash-map (kbd "S") #'gbl-notmuch-stash-sourcehut-link)
  ;; Like C-c M-h for `message-insert-headers'
  (define-key notmuch-message-mode-map (kbd "C-c M-e") #'gbl-notmuch-patch-add-email-control-code))

(use-package ol-notmuch)

;;; Sending email (SMTP)
(use-package smtpmail
  :ensure nil
  :config
  (setq smtpmail-default-smtp-server "mail.gandi.net")
  (setq smtpmail-smtp-server "mail.gandi.net")
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-queue-mail nil))

(use-package sendmail
  :ensure nil
  :config
  (setq send-mail-function 'smtpmail-send-it))

;;; EBDB (mail contacts)
(use-package ebdb
  :config
  (require 'ebdb-message)
  (require 'ebdb-notmuch) ; FIXME 2021-05-13: does not activate the corfu-mode UI
  (setq ebdb-sources (locate-user-emacs-file "ebdb"))
  (setq ebdb-permanent-ignores-file (locate-user-emacs-file "ebdb-permanent-ignores"))

  (setq ebdb-mua-pop-up nil)
  (setq ebdb-default-window-size 0.25)
  (setq ebdb-mua-default-formatter ebdb-default-multiline-formatter)

  (setq ebdb-mua-auto-update-p 'existing)
  (setq ebdb-mua-reader-update-p 'existing)
  (setq ebdb-mua-sender-update-p 'create)
  (setq ebdb-message-auto-update-p 'create)

  (setq ebdb-message-try-all-headers t)
  (setq ebdb-message-headers
        '((sender "From" "Resent-From" "Reply-To" "Sender")
          (recipients "Resent-To" "Resent-Cc" "Resent-CC" "To" "Cc" "CC" "Bcc" "BCC")))
  (setq ebdb-message-all-addresses t)

  (setq ebdb-complete-mail 'capf)
  (setq ebdb-mail-avoid-redundancy t)
  (setq ebdb-completion-display-record nil)
  (setq ebdb-complete-mail-allow-cycling nil)

  (setq ebdb-record-self "ace719a4-61f8-4bee-a1ca-2f07e2292305")
  (setq ebdb-user-name-address-re 'self) ; match the above
  (setq ebdb-save-on-exit t)

  (with-eval-after-load 'gbl-mail ; check my `gbl-mail.el'
    (add-hook 'message-setup-hook #'gbl-mail-ebdb-message-setup))

  (let ((map ebdb-mode-map))
    (define-key map (kbd "D") #'ebdb-delete-field-or-record)
    (define-key map (kbd "M") #'ebdb-mail) ; disables `ebdb-mail-each'
    (define-key map (kbd "m") #'ebdb-toggle-record-mark)
    (define-key map (kbd "t") #'ebdb-toggle-all-record-marks)
    (define-key map (kbd "T") #'ebdb-toggle-records-format) ; disables `ebdb-toggle-all-records-format'
    (define-key map (kbd "U") #'ebdb-unmark-all-records)))

(provide 'gbl-emacs-email)

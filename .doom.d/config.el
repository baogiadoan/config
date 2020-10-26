;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bao Doan"
      user-mail-address "giabaodoan1320@gmail.com")
;; my configuration here
(setq projectile-project-search-path '("~/code/"))
;;
;;
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(after! org
  (setq org-directory "~/ownCloud/org/")
  (setq org-agenda-files (directory-files-recursively "~/ownCloud/org/" "\\.org$")) ;; set the file for the org agenda,
  ;; could be multiple files
  ;; (setq org-log-done 'time) ;; log the time after done the task
  (setq org-log-done 'note) ;; log the time and give a NOTE after done the task

  ;; blog settings -- Org capture template
  (defun create-blog-post ()
          "Create an org file in ~/ownCloud/org/blog/posts"
          (interactive)
          (let ((name (read-string "Filename: ")))
          (expand-file-name (format "%s.org" name) "~/ownCloud/org/blog/posts/")))
  (setq org-capture-templates
          '(("p" "Post" plain
                  (file create-blog-post)
                  (file "~/.doom.d/org-templates/post.orgcaptmpl"))))

  ;; pretty bullets
  (use-package org-bullets
      :hook (org-mode . org-bullets-mode))

  (require 'org-journal)
  (setq org-journal-dir "~/ownCloud/org/journal/")
 )
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(load! "+bindings")

;; mu4e set-ups
(require 'smtpmail)
; smtp
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("mail.example.com" 587 nil nil))
      smtpmail-default-smtp-server "mail.example.com"
      smtpmail-smtp-server "mail.example.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
(require 'mu4e)
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "/home/user/Maildir")

;; default
(setq mu4e-contexts
    `( ,(make-mu4e-context
        :name "gmail"
        :enter-func (lambda ()
                        (mu4e-message "Entering gmail context")
                        ;; Quicky jump to/move a mail to different folders
                        (setq mu4e-maildir-shortcuts  '( ("/gmail/INBOX"   . ?i)
                                                         ("/gmail/sent"    . ?s)
                                                         ("/gmail/trash"   . ?t)
                                                         ("/gmail/drafts"  . ?d)
                                                         ("/gmail/archive" . ?r))))
        :leave-func (lambda ()
                        (mu4e-message "Leaving gmail context"))
        :match-func (lambda (msg)
                        (when msg
                            ;; Clemson has two valid emails for each student
                            (or (mu4e-message-contact-field-matches msg
                                    :to "adday@clemson.edu")
                                (mu4e-message-contact-field-matches msg
                                    :to "adday@g.clemson.edu"))))

        :vars '( ( user-mail-address      . "giabaodoan1320@gmail.com"  )
                 ( user-full-name         . "Bao Doan" )
                 ( mu4e-drafts-folder     . "/gmail/drafts")
                 ( mu4e-sent-folder       . "/gmail/sent")
                 ( mu4e-trash-folder      . "/gmail/trash")
                 ( mu4e-refile-folder     . "/gmail/archive" )
                 ( mu4e-compose-signature . (concat "Bao Doan"))))))


;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "mbsync -a")

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
;; tell msmtp to choose the SMTP server by the 'from' field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

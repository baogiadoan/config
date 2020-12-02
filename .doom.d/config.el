;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bao Doan"
      user-mail-address "bao.doan@adelaide.edu.au")
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
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
;;
(defun reload-theme (frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (load-theme 'doom-one t)
        (load-theme 'nord t))))

;; (add-hook 'after-make-frame-functions #'reload-theme)


(require 'doom-themes)
  (setq doom-theme 'doom-dracula)
  ;; (setq doom-theme 'doom-one-light)
  (setq doom-one-brighter-comments t)
  (setq doom-one-comment-bg nil)

;; change the color of the comments to cyan
;; (custom-set-faces
;;  `(font-lock-comment-face ((t (:foreground ,(doom-lighten 'cyan .5)))))
;;  `(font-lock-doc-face     ((t (:foreground ,(doom-lighten 'cyan .25))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq doom-theme 'doom-one)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(after! org
  (setq org-directory "~/ownCloud/org/")
  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-agenda-files (directory-files-recursively "~/ownCloud/org/" "\\.org$")) ;; set the file for the org agenda,
  ;; could be multiple files
  ;; (setq org-log-done 'time) ;; log the time after done the task
  ;; (setq org-log-done 'note) ;; log the time and give a NOTE after done the task
  ;; this is no need as I added @ after each keyword to take notes for every state change.

  (require 'org-journal)
  (setq org-journal-dir "~/ownCloud/org/journal/2020/")
  ;; org-journal integrate to org-agenda
  (setq org-journal-enable-agenda-integration t)
  ;; date format
  (setq org-journal-date-format "%A, %d/%m/%y")
  ;; blog settings -- Org capture template
  (defun create-blog-post ()
          "Create an org file in ~/ownCloud/org/blog/posts"
          (interactive)
          (let ((name (read-string "Filename: ")))
          (expand-file-name (format "%s.org" name) "~/ownCloud/org/blog/posts/")))

  ;; pretty bullets
  (use-package! org-bullets
      :hook (org-mode . org-bullets-mode))



  ;;org-super-agenda
  ;;
  (require 'org-super-agenda)
  (use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-day nil ;; i.e. today
      org-agenda-span 7
      org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name ""
                                  :time-grid t
                                  :date today
                                  :order 0)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "Important"
                                   :priority "A")
                            (:name "Other Priorities"
                                   :priority< "A")
                            (:name "BIG 3"
                                   :tag "BIG")
                            (:name "Today's tasks"
                                   :file-path "journal")
                            (:name "Due Today"
                                   :deadline today)
                            (:name "Scheduled Soon"
                                   :scheduled future)
                            (:name "Due Soon"
                                   :deadline future)
                            (:name "Meetings"
                                   :tag "MEET")
                            (:name "Overdue"
                                   :deadline past)
                            ;; (:discard (:not (:todo "TODO")))))))))))
                            ))))))))
  :config
  (org-super-agenda-mode))
  ;; workaround for conflict keybinding at org-super-agenda vs evil-mode
  (setq org-super-agenda-header-map (make-sparse-keymap))


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; follow some pro tips for using org mode
        ;; I use C-c c to start capture mode
        (global-set-key (kbd "C-c c") 'org-capture)
        ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

        (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/ownCloud/org/refile.org")
                "* TODO %?\n%U\n%a" :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~/ownCloud/org/refile.org")
                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/ownCloud/org/refile.org")
                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/ownCloud/org/diary.org")
                "* %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/ownCloud/org/refile.org")
                "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/ownCloud/org/refile.org")
                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Post" plain
                        (file create-blog-post)
                        (file "~/.doom.d/org-templates/post.orgcaptmpl"))
                ("h" "Habit" entry (file "~/ownCloud/org/refile.org")
                "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
        ;; Remove empty LOGBOOK drawers on clock out
        (defun bh/remove-empty-drawer-on-clock-out ()
        (interactive)
        (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at (point))))

        (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
        ; set   the custom agenda view
        ;
        ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                        (org-agenda-files :maxlevel . 9))))

        ;; Do not dim blocked tasks
        (setq org-agenda-dim-blocked-tasks nil)
        ;; Compact the block agenda view
        (setq org-agenda-compact-blocks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
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

;; only my Macbook requires this
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; this is for my PC at work
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "/home/baodoan/Maildir")
;; (setq message-signature-file "~/.doom.d/.signature") ; put your signature in this file
;; show images
(setq mu4e-view-show-images t)
;; rich-text message
(setq mu4e-view-prefer-html t)

(defun my-mu4e-html2text (msg)
  "My html2text function; shows short message inline, show
long messages in some external browser (see `browse-url-generic-program')."
  (let ((html (or (mu4e-message-field msg :body-html) "")))
    (if (> (length html) 20000)
      (progn
	(mu4e-action-view-in-browser msg)
	"[Viewing message in external browser]")
      (mu4e-shr2text msg))))

(setq mu4e-html2text-command 'my-mu4e-html2text)

;; if it's complicated enough, we will open the email in browser
(add-to-list 'mu4e-view-actions
                '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

; get mail
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))
;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
  (lambda()
;; try to emulate some of the eww key-bindings
(local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
(local-set-key (kbd "<tab>") 'shr-next-link)
(local-set-key (kbd "<backtab>") 'shr-previous-link)))
;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
	(interactive)
	(setq mu4e-headers-fields
	      `((:human-date . 25) ;; alternatively, use :date
		(:flags . 6)
		(:from . 22)
		(:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		(:size . 7)))))

(require 'smtpmail)
;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)
;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode
;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")
(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)




(require 'org-mu4e)
;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)
;; don't ask when quitting
(setq mu4e-confirm-quit nil)
;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode
;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "work" ;;for uni-mail
    :enter-func (lambda () (mu4e-message "Entering context work"))
    :leave-func (lambda () (mu4e-message "Leaving context work"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "bao.doan@adelaide.edu.au")))
    :vars '((user-mail-address . "bao.doan@adelaide.edu.au")
	    (user-full-name . "Bao Doan")
	    (mu4e-sent-folder . "/uni-mail/Sent Items")
	    (mu4e-drafts-folder . "/uni-mail/Drafts")
	    (mu4e-trash-folder . "/uni-mail/Deleted Items")
	    (mu4e-compose-signature . (concat "Best Regards,\n" "Bao Doan\n"))
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "~/Maildir/uni-mail/queue/cur")
	    (message-send-mail-function . smtpmail-send-it)
	    ;; (smtpmail-starttls-credentials . (("smtp.office365.com" 587 nil nil)))
	    ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
            (auth-sources . (list "~/.authinfo.gpg"))
	    (smtpmail-default-smtp-server . "smtp.office365.com")
            (smtpmail-smtp-server . "smtp.office365.com")
	    (smtpmail-smtp-user . "a1761351@adelaide.edu.au")
            (smtpmail-stream-type . starttls)
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . ( ("/uni-mail/INBOX"            . ?i)
					("/uni-mail/Sent Items" . ?s)
					("/uni-mail/Deleted Items"       . ?t)
					("/uni-mail/Drafts"    . ?d)
					))))
   (make-mu4e-context
    :name "personal" ;;for gmail
    :enter-func (lambda () (mu4e-message "Entering context personal"))
    :leave-func (lambda () (mu4e-message "Leaving context personal"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "giabaodoan1320@gmail.com")))
    :vars '((user-mail-address . "giabaodoan1320@gmail.com")
	    (user-full-name . "Bao Gia Doan")
	    (mu4e-sent-folder . "/gmail/[gmail].Sent Mail")
	    (mu4e-drafts-folder . "/gmail/[gmail].drafts")
	    (mu4e-trash-folder . "/gmail/[gmail].trash")
	    (mu4e-compose-signature . (concat "Best Regards,\n" "Bao Doan\n"))
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "~/Maildir/gmail/queue/cur")
	    (message-send-mail-function . smtpmail-send-it)
	    (smtpmail-smtp-user . "giabaodoan1320@gmail.com")
	    ;; (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	    ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-user . "giabaodoan1320@gmail.com")
            (auth-sources . (list "~/.authinfo.gpg"))
            (smtpmail-stream-type . starttls)
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . ( ("/gmail/INBOX"            . ?i)
					("/gmail/[Gmail]/Sent Mail" . ?s)
					("/gmail/[Gmail]/Important" . ?t)
					("/gmail/[Gmail]/Bin"     . ?b)
					("/gmail/[Gmail]/All Mail"  . ?a)
					("/gmail/[Gmail]/Starred"   . ?r)
					("/gmail/[Gmail]/Drafts"    . ?d)
					))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;archive the DONE task
;; (defun my-org-archive-done-tasks ()
;;   (interactive)
;;   (org-map-entries 'org-archive-subtree "/DONE" 'file));
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (Setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file)
)

;; mu4e-alert
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-download
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'epa)
(epa-file-enable)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)
(custom-set-variables
 '(conda-anaconda-home "/home/baodoan/anaconda3/"))
(setq
  conda-env-home-directory (expand-file-name "/home/baodoan/anaconda3/") ;; as in previous example; not required
  conda-env-subdirectory "envs")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'lsp-mode)
(require 'dap-mode)
(require 'dap-ui)
(require 'dap-python)
(dap-mode 1)
(dap-ui-mode 1)

;; load ox-reveal
;; (load! "lisp/ox-reveal")
;; (setq org-reveal-root "file:///home/user/reveal.js-4.1.0")

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice)
(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")))

;; reminder set-up
; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

; org fancy priorities
; to make tasks a bit more fun and fancy
(use-package!
    org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
; set org-modules for habit
(setq org-modules '(org-habit))

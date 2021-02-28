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
;; set the beacon mode on everywhere
(use-package! beacon
  :custom
  (beacon-push-mark 10)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.3)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode)
  (global-hl-line-mode 1))

(require 'doom-themes)
  (setq doom-theme 'doom-dracula)
  ;; (setq doom-theme 'doom-one-light)
  (setq doom-one-brighter-comments t)
  (setq doom-one-comment-bg nil)

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

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
  ;; Agenda log mode items to display (closed and state changes by default)
  (setq org-agenda-log-mode-items (quote (closed state)))
  ;; put the state change into drawer
  (setq org-log-into-drawer t)
  ;; could be multiple files
  ;; (setq org-log-done 'time) ;; log the time after done the task
  ;; (setq org-log-done 'note) ;; log the time and give a NOTE after done the task
  ;; this is no need as I added @ after each keyword to take notes for every state change.

  ;; set up org-crypt
  (require 'org-crypt)
    ;; (add-hook 'before-save-hook #'org-crypt-use-before-save-magic)
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance '("crypt"))
    (setq org-crypt-key nil)

  ;; set tags
  ; Tags with fast selection keys
  (setq org-tag-alist (quote ((:startgroup)
                              ("@office" . ?O)
                              ("@home" . ?H)
                              (:endgroup)
                              ("WAITING" . ?w)
                              ("HOLD" . ?h)
                              ("PERSONAL" . ?p)
                              ("WORK" . ?W)
                              ("ORG" . ?o)
                              ("crypt" . ?e)
                              ("NOTE" . ?n)
                              ("CANCELLED" . ?c)
                              ("FLAGGED" . ??))))
  ; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

  ; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)

  (require 'org-journal)
  (setq org-journal-dir "~/ownCloud/org/journal/2021/")
  ;; (setq org-agenda-files (list "~/ownCloud/org/journal/2020/")
  ;; (setq org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|\\`[0-9]+\\'")
  ;; org-journal integrate to org-agenda
  (setq org-journal-enable-agenda-integration t)
  ;; (setq org-journal-enable-encryption t)
  ;; date format
  (setq org-journal-date-format "%A, %d/%m/%y")
  (setq org-journal-key nil)
  ;; blog settings -- Org capture template
  (defun create-blog-post ()
          "Create an org file in ~/ownCloud/org/blog/posts"
          (interactive)
          (let ((name (read-string "Filename: ")))
          (expand-file-name (format "%s.org" name) "~/ownCloud/org/blog/posts/")))


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
      ;; org-agenda-span 1
      org-agenda-start-on-weekday nil)
  ;; pretty bullets
  (use-package! org-bullets
      :hook (org-mode . org-bullets-mode))
        ; org fancy priorities
        ; to make tasks a bit more fun and fancy
        (use-package! org-fancy-priorities
        :ensure t
        :hook
        (org-mode . org-fancy-priorities-mode)
        :config
        (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
        ; set org-modules for habit
        (setq org-modules '(org-habit))

  (setq org-agenda-custom-commands
        '(("c" "Super view"
           (
            (agenda "" (
                        (org-agenda-overriding-header "")
                        (org-agenda-span 'day)
                        (org-habit-show-habits-only-for-today t)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :habit t
                                  ;; :scheduled today
                                  :date today
                                  :order 0)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "Tasks to Refile"
                                   :tag "REFILE")
                            (:name "Habits"
                                :habit t
                                :order 9
                             )
                            (:name "Important"
                                   :priority "A")
                            (:name "Next Tasks"
                                   :todo "NEXT")
                            (:name "Scheduled Soon"
                                :scheduled future
                             )
                            (:name "Other Priorities"
                                   :priority< "A")
                            ;; (:name "BIG 3"
                            ;;        :tag "BIG")
                            ;; (:name "Today's tasks"
                            ;;        :file-path "journal")
                            (:name "Someday"
                                :tag "SOMEDAY"
                                :order 8
                             )
                            (:name "Tasks"
                                   :todo "TODO")
                            (:name "Projects"
                                   :todo "PROJECT")
                            (:name "Due Today"
                                   :deadline today)
                            (:name "Meetings"
                                :todo "MEETING"
                                :scheduled today
                             )
                            (:name "Due Soon"
                                   :deadline future)
                            (:name "Tasks Waiting"
                                   :todo "WAITING")
                            (:name "Overdue"
                                   :deadline past)
                            (:name "On-hold"
                                   :todo "HOLD")
                            ;; (:discard (:not (:todo "TODO")))))))))))
                            ))))))))
  :config
  (org-super-agenda-mode))
  ;; workaround for conflict keybinding at org-super-agenda vs evil-mode
  (setq org-super-agenda-header-map (make-sparse-keymap))

        (defun org-journal-find-location ()
        ;; Open today's journal, but specify a non-nil prefix argument in order to
        ;; inhibit inserting the heading; org-capture will insert the heading.
        (org-journal-new-entry t)
        (unless (eq org-journal-file-type 'daily)
        (org-narrow-to-subtree))
        (goto-char (point-max)))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; follow some pro tips for using org mode
        ;; I use C-c c to start capture mode
        ;; (global-set-key (kbd "C-c c") 'org-capture)
        ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("PROJECT" ("PROJECT" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

        (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/ownCloud/org/refile.org")
                "* TODO %?\n%U\n%a" :clock-in t :clock-resume t)
                ("p" "project" entry (file "~/ownCloud/org/refile.org")
                "* PROJECT %? :PROJECT:\n%U\n%a" :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~/ownCloud/org/refile.org")
                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/ownCloud/org/refile.org")
                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ;; ("j" "Journal" entry (file+datetree "~/ownCloud/org/diary.org")
                ;; "* %?\n%U\n" :clock-in t :clock-resume t)
                ("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}%?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/ownCloud/org/refile.org")
                "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/ownCloud/org/refile.org")
                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Post" plain
                        (file create-blog-post)
                        (file "~/.doom.d/org-templates/post.orgcaptmpl"))
                ("h" "Habit" entry (file "~/ownCloud/org/refile.org")
                "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
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
        ;; Agenda clock report parameters
        (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
        ; Set default column view headings: Task Effort Clock_Summary
        (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
        ; global Effort estimate values
        ; global STYLE property values for completion
        (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clock in defined functions
 ;;
;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;;
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)

  (setq bh/keep-clock-running nil)

  (defun bh/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
  Skips capture tasks, projects, and subprojects.
  Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (bh/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
             (bh/is-project-p))
        "TODO"))))
  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))
  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))
  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun bh/punch-in (arg)
    "Start continuous clocking and set the default task to the
  selected task.  If no task is selected set the Organization task
  as the default task."
    (interactive "p")
    (setq bh/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        ;;
        ;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (bh/clock-in-organization-task-as-default)))
      ;;
      ;; We are not in the agenda
      ;;
      (save-restriction
        (widen)
        ; Find the tags on the current task
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))))

  (defun bh/punch-out ()
    (interactive)
    (setq bh/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))

  (defun bh/clock-in-default-task ()
    (save-excursion
      (org-with-point-at org-clock-default-task
        (org-clock-in))))

  (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

  (defun bh/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16))))


  (defun bh/clock-in-parent-task ()
    "Move point to the parent (project) task if any and clock in"
    (let ((parent-task))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (not parent-task) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (if parent-task
              (org-with-point-at parent-task
                (org-clock-in))
            (when bh/keep-clock-running
              (bh/clock-in-default-task)))))))

  (defun bh/clock-out-maybe ()
    (when (and bh/keep-clock-running
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (bh/clock-in-parent-task)))

  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

  (require 'org-id)
  (defun bh/clock-in-task-by-id (id)
    "Clock in a task by id"
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil)))

  (defun bh/clock-in-last-task (arg)
    "Clock in the interrupted task if there is one
  Skip the default task and get the next one.
  A prefix arg forces clock in of the default task."
    (interactive "p")
    (let ((clock-in-to-task
           (cond
            ((eq arg 4) org-clock-default-task)
            ((and (org-clock-is-active)
                  (equal org-clock-default-task (cadr org-clock-history)))
             (caddr org-clock-history))
            ((org-clock-is-active) (cadr org-clock-history))
            ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
            (t (car org-clock-history)))))
      (widen)
      (org-with-point-at clock-in-to-task
        (org-clock-in nil))))
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
(load! "+mybindings")
(load! "+zporg")

;; only my Macbook requires this
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; this is for my PC at work
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
;; this is PC at home
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "~/Maildir")
;; (setq message-signature-file "~/.doom.d/.signature") ; put your signature in this file
;; show images
(setq mu4e-view-show-images t)
;; rich-text message
(setq mu4e-view-prefer-html t)

;; (defun my-mu4e-html2text (msg)
;;   "My html2text function; shows short message inline, show
;; long messages in some external browser (see `browse-url-generic-program')."
;;   (let ((html (or (mu4e-message-field msg :body-html) "")))
;;     (if (> (length html) 20000)
;;       (progn
;; 	(mu4e-action-view-in-browser msg)
;; 	"[Viewing message in external browser]")
;;       (mu4e-shr2text msg))))

;; (setq mu4e-html2text-command 'my-mu4e-html2text)

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
;; (defun org-archive-done-tasks ()
;;   (interactive)
;;   (org-map-entries
;;    (lambda ()
;;      (org-archive-subtree)
;;      (Setq org-map-continue-from (outline-previous-heading)))
;;    "/DONE" 'file)
;;   (org-map-entries
;;    (lambda ()
;;      (org-archive-subtree)
;;      (setq org-map-continue-from (outline-previous-heading)))
;;    "/CANCELLED" 'file)
;; )
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/KILL" 'tree)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/CANCELLED" 'tree)
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

;; workaround to fix rg search within project
;; Will only work on macos/linux
(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

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
 '(conda-anaconda-home "~/anaconda3/"))
(setq
  conda-env-home-directory (expand-file-name "~/anaconda3/") ;; as in previous example; not required
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
;; (eval-after-load "org"
;;   '(progn
;;      ;; Change .pdf association directly within the alist
;;      (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")))

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


;; ;; set up org-brain
;; (use-package! org-brain :ensure t
;;   :init
;;   (setq org-brain-path "~/ownCloud/org/")
;;   ;; For Evil users
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/ownCloud/org/.org-id-locations")
;;   (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;;           "* %i%?" :empty-lines 1)
;;         org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 50)
;;   (setq org-brain-include-file-entries nil
;;         org-brain-file-entries-use-title nil)
;; ;; disable file entries, will enable later if I want to cross link I guess
;;   (setq org-brain-headline-entry-name-format-string "%2$s")
;;   (setq my/default-org-brain-file "brain")
;;   (setq org-brain-default-file-parent my/default-org-brain-file))

;; BUG this is the bug from the package, wait for the solution later
;; Allows you to edit entries directly from org-brain-visualize
;; (use-package polymode
;;   :config
;;   (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))


;; org-mind-map config
;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package! org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;; :ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )
;;config for flycheck
;; (defun my-flycheck-setup ()
;;   (flycheck-add-next-checker 'lsp 'python-flake8))

;; ;; These MODE-local-vars-hook hooks are a Doom thing. They're executed after
;; ;; MODE-hook, on hack-local-variables-hook. Although `lsp!` is attached to
;; ;; python-mode-local-vars-hook, it should occur earlier than my-flycheck-setup
;; ;; this way:
;; (add-hook 'python-mode-local-vars-hook #'my-flycheck-setup)

(add-hook 'lsp-after-initialize-hook (lambda
                                       ()
                                       (flycheck-disable-checker 'python-pylint)
                                       (flycheck-add-next-checker 'lsp 'python-flake8)))


;; (use-package! flycheck
;;   :config
;;   (add-to-list 'flycheck-disabled-checkers 'python-pylint)
;;   (add-hook 'pyhon-mode-local-vars-hook
;;           (lambda ()
;;             (when (flycheck-may-enable-checker 'python-flake8)
;;               (flycheck-select-checker 'python-flake8))))
;; )


;; setup python-black autopep
;; config.el
(use-package! python-black
  :demand t
  :after python)
(add-hook! 'python-mode-local-vars-hook #'python-black-on-save-mode)
;; Feel free to throw your own personal keybindings here
(map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
(map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
(map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)


;; ;; dap-mode setup
;; (use-package! dap-mode)
;; (require 'dap-python)
;; (use-package! python-mode
;;   :ensure t
;;   :hook (python-mode  . lsp-deferred)
;;   :custom
;;   (dap-python-executable "python")
;;   (dap-python-debugger 'debugpy)
;;   :config
;;   (require 'dap-python)
;;   )
(setq dap-python-debugger 'debugpy)
(unpin! dap-mode lsp-mode treemacs)
(setq inhibit-eol-conversion t)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
;; garbage tweak
;;
(after! gcmh
  (setq gcmh-high-cons-threshold 33554432))  ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb

(setq org-ditaa-jar-path "/bin/ditaa")

;; enable ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

;; reset checklist
(require 'org-checklist)
(setq org-deadline-warning-days 30)

;; highlight the clock if running out of time
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
;;prefer future dates or not
(setq org-read-date-prefer-future nil)
(setq org-read-date-prefer-future 'time)
(setq mu4e-index-update-error-warning nil)



;; variables for org-roam
(setq
   org_notes (concat (getenv "HOME") "/ownCloud/org/roam/notes/")
   deft-directory org_notes
   org-roam-directory org_notes
   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-bibtex set-up (need to set-up Mendeley to store to the following location
;; pdf stored to this location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bibtex-completion
(use-package! bibtex-completion)

(setq bibtex-completion-library-path '("~/ownCloud/org/roam"))
;; bibtex file stored to this location
(setq bibtex-completion-bibliography '("~/ownCloud/org/roam/library.bib"))
;; this enable helm-bibtex to know where is the location of the pdf file corresponding to the bibtex
(setq bibtex-completion-pdf-field "File")
(setq bibtex-completion-notes-path "~/ownCloud/org/roam/notes")
(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")
(setq bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up for org-ref
;; (require 'org-ref)
;; (setq reftex-default-bibliography '("~/ownCloud/org/roam/library.bib"))
;; (setq
;;   org-ref-bibliography-notes "~/ownCloud/org/roam/notes/"
;;   org-ref-default-bibliography '("~/ownCloud/org/roam/library.bib")
;;   org-ref-pdf-directory "~/ownCloud/org/roam/pdfs/")

(use-package! org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "~/ownCloud/org/roam/library.bib")
         ;; org-ref-bibliography-notes "~/ownCloud/org/bibtex/notes/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "~/ownCloud/org/roam/notes/"
         org-ref-notes-function 'orb-edit-notes
    ))
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :preface
  ;; Set this to nil so we can later detect whether the user has set a custom
  ;; directory for it, and default to `org-directory' if they haven't.
  (defvar org-roam-directory nil)
  :init
  :config
  (setq org-roam-directory "~/ownCloud/org/roam/notes/")
  (setq
        ;; org-roam-directory (expand-file-name (or org-roam-directory "roam")
        ;;                                      org-directory)
        org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
        org-roam-buffer-no-delete-other-windows t ; make org-roam buffer sticky
        org-roam-completion-system 'default
)


  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish among other org buffers.
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)


(use-package company-org-roam
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))



;; ;; set-up org-roam
;; (use-package! org-roam
;;       :ensure t
;;       :hook
;;       (after-init . org-roam-mode)
;;       :custom
      ;; (org-roam-directory "~/ownCloud/org/roam"))
(setq org-roam-db-update-method 'immediate)


;; org-roam-bibtex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; (use-package org-roam-bibtex
;;   :after (org-roam)
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :config
;;   (setq org-roam-bibtex-preformat-keywords
;;    '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;;   (setq orb-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

;; - tags ::
;; - keywords :: ${keywords}

;; \n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

;;            :unnarrowed t))))


(defvar orb-title-format "${author-or-editor-abbrev} (${date}).  ${title}."
  "Format of the title to use for `orb-templates'.")

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))
  ;; :load-path "~/projects/org-roam-bibtex/"
  ;; :bind (:map org-roam-bibtex-mode-map
  ;;        (("C-c n f" . orb-find-non-ref-file))
  ;;        :map org-mode-map
  ;;        (("C-c n t" . orb-insert-non-ref)
  ;;         ("C-c n a" . orb-note-actions)))
(setq orb-preformat-keywords
    '("citekey" "title" "url" "author-or-editor" "file")
    orb-process-file-keyword t
    orb-file-field-extensions '("pdf"))

(setq orb-templates
    '(("r" "ref" plain (function org-roam-capture--get-point)
       ""
       :file-name "${citekey}"
       :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}  ; <== special file keyword: if more than one filename
:END:")))

;; copy from zaeph config for pdf annotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(use-package! pdf-tools
  ;; :magic ("%PDF" . pdf-view-mode)
  :demand
  :config
  (pdf-tools-install :no-query))

(use-package! pdf-view
  :config
  (defvar zp/pdf-annot-default-annotation-color "#F1F23B"
    "Default color to use for annotations.")

  (setq zp/pdf-annot-default-annotation-color "#F1F23B")

  (setq pdf-annot-default-annotation-properties
        `((t (label . ,user-full-name))
          (text (icon . "Note") (color . ,zp/pdf-annot-default-annotation-color))
          (highlight (color . "#FFFF4D")) ;Lighter yellow
          (squiggly (color . "orange"))
          (strike-out (color . "red"))
          (underline (color . "blue"))))

  (defun zp/toggle-pdf-view-auto-slice-minor-mode ()
    "Toggle ‘pdf-view-auto-slice-minor-mode’ and reset slice."
    (interactive)
    (call-interactively 'pdf-view-auto-slice-minor-mode)
    (if (not pdf-view-auto-slice-minor-mode)
        (progn
          (pdf-view-reset-slice))))

  ;; Disable continuous view in pdf-view
  ;; I prefer to explicitly turn pages
  (setq pdf-view-continuous nil)

  ;; Automatically activate annotation when they’re created
  (setq pdf-annot-activate-created-annotations t)

  (defvar zp/pdf-view-save-after-annotation nil
    "When non-nil, save the PDF after an annotation is created.")

  ;; Save after creating an annotation
  (defun zp/pdf-view-save-buffer ()
    "Save buffer and preserve midnight state."
    (interactive)
    (call-interactively #'save-buffer)
    (pdf-view-midnight-minor-mode 'toggle)
    (pdf-view-midnight-minor-mode 'toggle))

  (defun zp/pdf-view-save-buffer-maybe ()
    "Save buffer and preserve midnight state."
    (when zp/pdf-view-save-after-annotation
      (zp/pdf-view-save-buffer)))

  (advice-add 'pdf-annot-edit-contents-commit :after 'zp/pdf-view-save-buffer-maybe)

  (defun zp/pdf-view-continuous-toggle ()
    (interactive)
    (cond ((not pdf-view-continuous)
           (setq pdf-view-continuous t)
           (message "Page scrolling: Continous"))
          (t
           (setq pdf-view-continuous nil)
           (message "Page scrolling: Constrained"))))

  (defun zp/pdf-view-open-in-evince ()
    "Open the current PDF with ‘evince’."
    (interactive)
    (save-window-excursion
      (let ((current-file (buffer-file-name))
            (current-page (number-to-string (pdf-view-current-page))))
        (async-shell-command
         (format "evince -i %s \"%s\"" current-page current-file))))
    (message "Sent to Evince"))

  ;; xournalpp is the hand-writing note-taking app
  (defun zp/pdf-view-open-in-xournalpp ()
    "Open the current PDF with ‘xournalpp’."
    (interactive)
    (save-window-excursion
      (let ((current-file (buffer-file-name)))
        (async-shell-command
         (format "xournalpp \"%s\"" current-file))))
    (message "Sent to Xournal++"))

  (defun zp/pdf-view-show-current-page ()
    "Show the current page."
    (interactive)
    (message "Page: %s" (pdf-view-current-page)))
  ;;--------------------
  ;; Custom annotations
  ;;--------------------

  (defun zp/pdf-annot-add-custom-annotation (type color &optional icon)
    "Add custom annotation with ICON and COLOR."
    (let* ((icon (or icon "Note"))
           (color (or color zp/pdf-annot-default-annotation-color))
           (pdf-annot-default-annotation-properties
            `((t (label . ,user-full-name))
              ,(pcase type
                 ('text `(text (icon . ,icon) (color . ,color)))
                 ('highlight `(highlight (color . ,color)))))))
      (call-interactively (pcase type
                            ('text #'pdf-annot-add-text-annotation)
                            ('highlight #'pdf-annot-add-highlight-markup-annotation)))))

  (defvar zp/pdf-custom-annot-list nil
    "List of custom annotations and their settings.
Each element in list must be a list with the following elements:
- Name of the function to create
- Key binding
- Name of the icon to use
- Color to use")

  (defun zp/pdf-custom-annot-init ()
    (seq-do
     (lambda (settings)
       (cl-destructuring-bind (name type key icon color) settings
         (let* ((root "zp/pdf-annot-add-text-annotation-")
                (fun (intern (concat root name))))
           (defalias fun
             `(lambda ()
                (interactive)
                (zp/pdf-annot-add-custom-annotation ,type ,color ,icon))
             (format "Insert a note of type ‘%s’." name))
           (when key
             (define-key pdf-view-mode-map
               (kbd key)
               `,fun)))))
     zp/pdf-custom-annot-list))
  (define-prefix-command 'zp/pdf-custom-annot-map)

  (define-key pdf-view-mode-map "a" 'zp/pdf-custom-annot-map)

  (setq zp/pdf-custom-annot-list
        `(("note" 'text "t" "Note" ,zp/pdf-annot-default-annotation-color)
          ("note-blue" 'text "T" "Note" "#389BE6")
          ("insert" 'text "ai" "Insert" "#913BF2")
          ("comment" 'text "c" "Comment" "#389BE6")
          ("comment-red" 'text "ac" "Comment" "#FF483E")
          ("circle" 'text "ay" "Circle" "#38E691")
          ("cross" 'text "an" "Cross" "#FF483E")

          ("hl-red" 'highlight nil nil "#FF7F7F")
          ("hl-blue" 'highlight nil nil "#7FDFFF")
          ("hl-green" 'highlight nil nil "#7FFF7F")
          ("hl-purple" 'highlight nil nil "#967FFF")
          ("hl-orange" 'highlight nil nil "#FFBF7F")))

  (setq pdf-annot-color-history
        '("#FFFF4D" "#FF7F7F" "#7FDFFF" "#7FFF7F" "#967FFF" "#FFBF7F"))

  (defun zp/pdf-annot-add-highlight-markup-annotation (arg &optional activate)
    "Add highlight markup annotation.
This wrapper includes presets which can be accessed with
numerical arguments."
    (interactive "P")
    (let ((pdf-annot-activate-created-annotations (when activate t)))
      (pcase arg
        (1 (zp/pdf-annot-add-text-annotation-hl-red))
        (2 (zp/pdf-annot-add-text-annotation-hl-blue))
        (3 (zp/pdf-annot-add-text-annotation-hl-green))
        (4 (zp/pdf-annot-add-text-annotation-hl-purple))
        (5 (zp/pdf-annot-add-text-annotation-hl-orange))
        (_ (call-interactively #'pdf-annot-add-highlight-markup-annotation))))
    (unless activate
      (zp/pdf-view-save-buffer-maybe)))

  (defun bd/pdf-annot-add-underline-markup-annotation (arg &optional activate)
    "Add underline markup annotation.
This wrapper includes presets which can be accessed with
numerical arguments."
    (interactive "P")
    (let ((pdf-annot-activate-created-annotations (when activate t)))
        (call-interactively #'pdf-annot-add-underline-markup-annotation))
    (unless activate
      (zp/pdf-view-save-buffer-maybe)))



  (defun zp/pdf-annot-add-highlight-markup-annotation-and-activate (arg)
    "Add highlight markup annotation and activate it.
This wrapper includes presets which can be accessed with
numerical arguments."
    (interactive "P")
    (zp/pdf-annot-add-highlight-markup-annotation arg t))

  (zp/pdf-custom-annot-init)

  ;;----------
  ;; Bindings
  ;;----------

  ;; Use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)

  (define-key pdf-view-mode-map (kbd "C-x C-s") 'zp/pdf-view-save-buffer)

  (define-key pdf-view-mode-map (kbd "m") 'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map (kbd "P") 'pdf-view-printer-minor-mode)
  (define-key pdf-view-mode-map (kbd "s") 'zp/toggle-pdf-view-auto-slice-minor-mode)
  (define-key pdf-view-mode-map (kbd "M") 'pdf-view-set-slice-using-mouse)
  (define-key pdf-view-mode-map (kbd "C") 'zp/pdf-view-continuous-toggle)
  (define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "f") 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map (kbd "RET") 'zp/pdf-view-open-in-evince)
  (define-key pdf-view-mode-map [(shift return)] 'zp/pdf-view-open-in-xournalpp)
  (define-key pdf-view-mode-map (kbd ".") 'zp/pdf-view-show-current-page)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "h") 'zp/pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "H") 'zp/pdf-annot-add-highlight-markup-annotation-and-activate)
  (define-key pdf-view-mode-map (kbd "x") 'bd/pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "O") 'org-noter-create-skeleton)

  (define-prefix-command 'slice-map)
  (define-key pdf-view-mode-map (kbd "S") 'slice-map)
  (define-key pdf-view-mode-map (kbd "S b") 'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "S m") 'pdf-view-set-slice-using-mouse)
  (define-key pdf-view-mode-map (kbd "S r") 'pdf-view-reset-slice)

  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))

(use-package! pdf-annot
  :config
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-c C-k") 'pdf-annot-edit-contents-abort))

(use-package! pdf-links
  :config
  (define-key pdf-links-minor-mode-map (kbd "f") 'pdf-view-fit-page-to-window))

;; my config to disable evil-mode in pdfviewers
(evil-set-initial-state 'pdf-view-mode 'emacs)

;;----------------------------------------------------------------------------
;; org-noter
;;----------------------------------------------------------------------------


(use-package! org-noter
  :after (:any org pdf-view)
  :bind (:map org-mode-map
         (("C-c N" . zp/org-noter-dwim))
         :map org-noter-doc-mode-map
         (("M-i" . zp/org-noter-insert-precise-note-dwim)))
  :config

  (setq org-noter-hide-other t
        org-noter-auto-save-last-location t
        org-noter-notes-window-location 'other-frame
        org-noter-always-create-frame nil
        ;; relative to the pdf file
        org-noter-notes-search-path (list org_notes)
        ;; org-noter-hide-other nil
        org-noter-doc-split-fraction '(0.57 0.43))

  (defun zp/org-noter-visual-line-mode ()
    "Enable visual-line-mode in ‘org-noter’ notes.
Workaround to counter race conditions with the margins."
    (let ((parent (current-buffer))
          (refresh (lambda (parent)
                     (with-current-buffer parent
                       (visual-line-mode 'toggle)
                       (visual-line-mode 'toggle)))))
      (run-at-time "1 sec" nil refresh parent)
      (run-at-time "5 sec" nil refresh parent)))

  (add-hook 'org-noter-notes-mode-hook #'zp/org-noter-visual-line-mode)

  ;; Fix for hiding truncation
  (defun org-noter--set-notes-scroll (_window &rest _ignored)
    nil)

  ;; Fix for visual-line-mode with PDF files
  (defun org-noter--note-after-tipping-point (_point _note-property _view)
    nil)

  (defun zp/org-noter-indirect (arg)
    "Ensure that org-noter starts in an indirect buffer.
Without this wrapper, org-noter creates a direct buffer
restricted to the notes, but this causes problems with the refile
system.  Namely, the notes buffer gets identified as an
agenda-files buffer.
This wrapper addresses it by having org-noter act on an indirect
buffer, thereby propagating the indirectness."
    (interactive "P")
    (if (org-entry-get nil org-noter-property-doc-file)
        (with-selected-window (zp/org-tree-to-indirect-buffer-folded nil t)
          (org-noter arg)
          (kill-buffer))
      (org-noter arg)))

  (defun zp/org-noter-dwim (arg)
    "Run org-noter on the current tree, even if we’re in the agenda."
    (interactive "P")
    (let ((in-agenda (derived-mode-p 'org-agenda-mode))
          (marker))
      (cond (in-agenda
             (setq marker (get-text-property (point) 'org-marker))
             (with-current-buffer (marker-buffer marker)
               (goto-char marker)
               (unless (org-entry-get nil org-noter-property-doc-file)
                 (user-error "No org-noter info on this tree"))
               (zp/org-noter-indirect arg)))
            (t
             (zp/org-noter-indirect arg)
             (setq marker (point-marker))))
      (org-with-point-at marker
        (let ((tags (org-get-tags)))
          (when (and (org-entry-get nil org-noter-property-doc-file)
                     (not (member "noter" tags)))
            (org-set-tags (push "noter" tags)))))
      (unless in-agenda
        (set-marker marker nil))))

  (defun zp/org-noter-insert-precise-note-dwim (force-mouse)
    "Insert note associated with a specific location.
If in nov-mode, use point rather than the mouse to target the
position."
    (interactive "P")
    (if (and (derived-mode-p 'nov-mode)
             (not force-mouse))
        (let ((pos (if (region-active-p)
                       (min (region-beginning) (point))
                     (point))))
          (org-noter-insert-note pos))
      (org-noter-insert-precise-note)))

  (define-key org-noter-doc-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key org-noter-doc-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-roam server
(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (smartparens-global-mode -1)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
    (smartparens-global-mode 1))

;; automatically enable server-mode
(after! org-roam
  (smartparens-global-mode -1)
  (org-roam-server-mode)
  (smartparens-global-mode 1))

;; debug the mu4e mail
;; damn this helps to prevent warning issue in mu4e that I tried to fix for a long time
(setq smtpmail-debug-info t)
;; (setq auth-source-debug t)

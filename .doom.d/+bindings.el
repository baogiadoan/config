;;; bindings.el -*- lexical-binding: t; -*-

(map!
 "C-h"  #'evil-window-left
 "C-l"  #'evil-window-right
 "C-j"  #'evil-window-down
 "C-k"  #'evil-window-up
 )

(map! :leader
      (:prefix ("j" . "journal") ;; org-journal bindings
        :desc "Create new journal entry" "j" #'org-journal-new-entry
        :desc "Open previous entry" "p" #'org-journal-open-previous-entry
        :desc "Open next entry" "n" #'org-journal-open-next-entry
        :desc "Search journal" "s" #'org-journal-search-forever))

;; The built-in calendar mode mappings for org-journal
;; conflict with evil bindings
(map!
 (:map calendar-mode-map
   :n "o" #'org-journal-display-entry
   :n "p" #'org-journal-previous-entry
   :n "n" #'org-journal-next-entry
   :n "O" #'org-journal-new-date-entry))
;; binding mu4e -- email client
;; (map! :leader
;;       :desc "Open mu4e" "m" #'mu4e)
;; Local leader (<SPC m>) bindings for org-journal in calendar-mode
;; I was running out of bindings, and these are used less frequently
;; so it is convenient to have them under the local leader prefix
(map!
 :map (calendar-mode-map)
 :localleader
 "w" #'org-journal-search-calendar-week
 "m" #'org-journal-search-calendar-month
 "y" #'org-journal-search-calendar-year)

;; try with my custom keybindings to move around windows
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)


;; projectile mode
(define-key evil-window-map (kbd "C-l") 'evil-window-right)
(define-key evil-window-map (kbd "C-h") 'evil-window-left)
(define-key evil-window-map (kbd "C-j") 'evil-window-down)
(define-key evil-window-map (kbd "C-k") 'evil-window-up)

;; some of my keybinding stealed from pros
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f9> i") 'bh/punch-in)
(global-set-key (kbd "<f9> o") 'bh/punch-out)
(global-set-key (kbd "C-c C-x i") 'org-insert-columns-dblock)
(global-set-key (kbd "C-c d") 'org-decrypt-entry)
(global-set-key (kbd "C-c e") 'org-encrypt-entry)


;; some defined key
(global-set-key (kbd "C-c N") 'org-noter)
(global-set-key (kbd "C-c m") 'mu4e)
;; yay, it worked

;; (map! :leader
;;       (:prefix ("r" . "roam")
;;        (:map org-roam-mode-map
;;         (:desc "org-roam" "l" #'org-roam
;;         :desc "find file" "f" #'org-roam-find-file
;;         :desc "graph" "g" #'org-roam-graph))
;;        (:map org-mode-map
;;         (:desc "insert" "i" #'org-roam-insert
;;         :desc "insert immediately" "I" #'org-roam-insert-immediate))
;;   )
;;  )


(map!
  (:map org-roam-mode-map
  "C-c n l" #'org-roam
  "C-c n f" #'org-roam-find-file
  "C-c n g" #'org-roam-graph
  "C-c n r" #'org-roam-buffer-toggle-display
  "C-c n b" #'org-roam-switch-to-buffer)
  (:map org-mode-map
  "C-c n i" #'org-roam-insert
  "C-c n I" #'org-roam-insert-immediate))

;; helm key map
(map! :leader
      :prefix ("e" . "helm")
        :desc "bibtex" "b" #'helm-bibtex
        :desc "bibtex with local bib" "B" #'helm-bibtex-with-local-bibliography
        :desc "bibtex with notes" "n" #'helm-bibtex-with-notes
        :desc "resume" "r" #'helm-resume
 )

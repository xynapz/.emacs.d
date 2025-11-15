;;; org-config.el --- Org mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; org-mode configuration for my Emacs config.

;;; Code:
(require 'org)

;; Org directories
(setq org-directory "~/org/"
      org-default-notes-file (expand-file-name "notes.org" org-directory))

;; Create org directory if it doesn't exist
(unless (file-exists-p org-directory)
  (make-directory org-directory t))

;; Org settings
(setq org-startup-folded 'content
      org-startup-indented t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " ▾"
      org-log-done 'time
      org-log-into-drawer t
      org-return-follows-link t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-fontify-quote-and-verse-blocks t
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-startup-with-inline-images t
      org-cycle-separator-lines 2)

;; Org agenda
(setq org-agenda-files (list org-directory)
      org-agenda-start-with-log-mode t
      org-agenda-window-setup 'current-window
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-deadline-warning-days 7)

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")))

;; TODO keyword faces
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff6c6b" :weight bold))
        ("NEXT" . (:foreground "#da8548" :weight bold))
        ("PROG" . (:foreground "#ECBE7B" :weight bold))
        ("WAIT" . (:foreground "#51afef" :weight bold))
        ("DONE" . (:foreground "#98be65" :weight bold))
        ("CANCELLED" . (:foreground "#5B6268" :weight bold))))

;; Org tags
(setq org-tag-alist
      '((:startgroup)
        (:endgroup)
        ("@work" . ?w)
        ("@home" . ?h)
        ("@errand" . ?e)
        ("planning" . ?p)
        ("idea" . ?i)
        ("note" . ?n)))

;; Org capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree (lambda () (expand-file-name "journal.org" org-directory)))
         "* %?\nEntered on %U\n  %i\n  %a")
        ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
         "* MEETING %? :meeting:\n  %U")
        ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
         "* %? :idea:\n  %U\n  %i\n  %a")))

;; Org babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (C . t)))

;; Don't ask for confirmation before evaluating
(setq org-confirm-babel-evaluate nil)

;; Org mode evil bindings
(with-eval-after-load 'org
  (require 'evil-org)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))

  ;; Additional evil bindings for org
  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    (kbd "RET") 'org-open-at-point
    (kbd "gh") 'outline-up-heading
    (kbd "gj") 'org-forward-heading-same-level
    (kbd "gk") 'org-backward-heading-same-level
    (kbd "gl") 'outline-next-visible-heading
    (kbd "t") 'org-todo
    (kbd ",a") 'org-archive-subtree
    (kbd ",t") 'org-set-tags-command
    (kbd ",p") 'org-priority
    (kbd ",s") 'org-schedule
    (kbd ",d") 'org-deadline))

;; Org modern for better aesthetics
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "○" "✸" "✿" "◆" "◇")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword nil
        org-modern-timestamp t
        org-modern-statistics t
        org-modern-todo t))

;; Org appear - Show emphasis markers on demand
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil))

;; Org present - Presentations
(use-package org-present
  :after org
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

;; Visual fill for better org reading
(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 100
                            visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))

(provide 'org-config)
;;; org-config.el ends here

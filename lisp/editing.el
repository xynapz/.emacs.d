;;; editing.el --- Editing behavior -*- lexical-binding: t; -*-
;;; Commentary:
;; editor settings.

;;; Code:
;; Dired
(use-package dired
  :ensure nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-Ahl --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package diredfl
  :after dired
  :config (diredfl-global-mode 1))

;; Smartparens
(use-package smartparens
  :diminish
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Whitespace
(use-package whitespace
  :ensure nil
  :diminish
  :config
  (setq whitespace-style '(face tabs trailing tab-mark))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))

;; Auto-fill in comments
(setq-default comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fill-column 80)
            (auto-fill-mode 1)))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Compile settings
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output t)
  (compilation-ask-about-save nil)
  (compilation-window-height 12)
  :hook
  (compilation-filter . (lambda ()
                          (ansi-color-apply-on-region
                           compilation-filter-start (point)))))

;; Better undo
(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-hist/" user-emacs-directory))))
  (global-undo-tree-mode))

;; Move text
(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode 1))

;; Better search/replace
(use-package anzu
  :diminish
  :config
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil))

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; Avy for jumping
(use-package avy
  :config
  (setq avy-timeout-seconds 0.3
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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
              (org-display-inline-images)
              ))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-remove-inline-images))))

;; Visual fill for better org reading
(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 100
                            visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))

(provide 'editing)
;;; editing.el ends here

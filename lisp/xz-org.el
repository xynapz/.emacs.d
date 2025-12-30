;;; xz-org.el --- Org Mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive Org Mode setup with Agenda and Capture for
;; learning system and project management.

;;; Code:

;; ---------------------------------------------------------------------------
;; Directory Structure
;; ---------------------------------------------------------------------------
(defvar xz/org-directory "~/org/"
  "Base directory for all Org files.")

(defvar xz/org-inbox-file (expand-file-name "inbox.org" xz/org-directory)
  "Default capture target for quick notes and tasks.")

(defvar xz/org-projects-file (expand-file-name "projects.org" xz/org-directory)
  "File for active projects.")

(defvar xz/org-learning-file (expand-file-name "learning.org" xz/org-directory)
  "File for learning resources and study tasks.")

(defvar xz/org-journal-file (expand-file-name "journal.org" xz/org-directory)
  "File for daily journal entries.")

(defvar xz/org-archive-file (expand-file-name "archive.org" xz/org-directory)
  "File for archived items.")

;; ---------------------------------------------------------------------------
;; Org Mode Configuration
;; ---------------------------------------------------------------------------
(use-package org
  :pin elpa
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;; General settings
  (setq org-directory xz/org-directory
        org-default-notes-file xz/org-inbox-file
        org-startup-folded 'content
        org-startup-indented t
        org-hide-emphasis-markers t
        org-ellipsis " ▾"
        org-log-done 'time
        org-log-into-drawer t
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-confirm-babel-evaluate nil)

  ;; ---------------------------------------------------------------------------
  ;; TODO Keywords
  ;; ---------------------------------------------------------------------------
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#BF616A" :weight bold))
          ("NEXT" . (:foreground "#EBCB8B" :weight bold))
          ("WAIT" . (:foreground "#D08770" :weight bold))
          ("DONE" . (:foreground "#A3BE8C" :weight bold))
          ("CANCELLED" . (:foreground "#4C566A" :weight bold))))

  ;; ---------------------------------------------------------------------------
  ;; Agenda
  ;; ---------------------------------------------------------------------------
  (setq org-agenda-files (list xz/org-directory))

  (setq org-agenda-custom-commands
        '(;; === Timeframe Views ===
          ("1" "Today"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-start-day "today")
                        (org-deadline-warning-days 0)
                        (org-agenda-overriding-header "📅 Today")))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))))

          ("2" "Next 2 Days"
           ((agenda "" ((org-agenda-span 2)
                        (org-agenda-start-day "today")
                        (org-deadline-warning-days 2)
                        (org-agenda-overriding-header "📅 Next 2 Days")))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))))

          ("7" "This Week"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday nil)
                        (org-deadline-warning-days 7)
                        (org-agenda-overriding-header "📅 This Week")))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))
            (todo "WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))))

          ("m" "This Month"
           ((agenda "" ((org-agenda-span 'month)
                        (org-agenda-start-day "today")
                        (org-deadline-warning-days 30)
                        (org-agenda-overriding-header
                         (format "📅 %s %s"
                                 (format-time-string "%B")
                                 (format-time-string "%Y")))))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))
            (stuck "" ((org-agenda-overriding-header "🚧 Stuck Projects")))))

          ;; === Dashboard & Categories ===
          ("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))
            (todo "WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
            (tags-todo "inbox" ((org-agenda-overriding-header "📥 Inbox - To Refile")))))

          ("p" "Projects"
           ((tags-todo "+project"
                       ((org-agenda-overriding-header "🚀 Active Projects")))))

          ("l" "Learning"
           ((tags-todo "+learning"
                       ((org-agenda-overriding-header "📚 Learning Tasks")))
            (tags-todo "+reading"
                       ((org-agenda-overriding-header "📖 Reading List")))))

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "DONE" ((org-agenda-overriding-header "✅ Completed This Week")))
            (stuck "" ((org-agenda-overriding-header "🚧 Stuck Projects")))))))

  (setq org-agenda-window-setup 'current-window
        org-agenda-start-with-log-mode t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  ;; ---------------------------------------------------------------------------
  ;; Capture Templates
  ;; ---------------------------------------------------------------------------
  (setq org-capture-templates
        `(("t" "Task" entry (file ,xz/org-inbox-file)
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("p" "Project" entry (file ,xz/org-projects-file)
           "* TODO %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Goals\n** Tasks\n*** TODO \n** Notes\n"
           :empty-lines 1)

          ("l" "Learning" entry (file ,xz/org-learning-file)
           "* TODO %? :learning:\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: \n:END:\n** Notes\n** Questions\n** Summary\n"
           :empty-lines 1)

          ("r" "Reading" entry (file+headline ,xz/org-learning-file "Reading List")
           "* TODO %? :reading:\n:PROPERTIES:\n:CREATED: %U\n:AUTHOR: \n:END:\n"
           :empty-lines 1)

          ("j" "Journal" entry (file+datetree ,xz/org-journal-file)
           "* %<%H:%M> %?\n"
           :empty-lines 1)

          ("n" "Note" entry (file ,xz/org-inbox-file)
           "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)))

  ;; ---------------------------------------------------------------------------
  ;; Refile
  ;; ---------------------------------------------------------------------------
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 2)))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  ;; ---------------------------------------------------------------------------
  ;; Archive
  ;; ---------------------------------------------------------------------------
  (setq org-archive-location (concat xz/org-archive-file "::* From %s"))

  ;; ---------------------------------------------------------------------------
  ;; Babel (Code Execution)
  ;; ---------------------------------------------------------------------------
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (setq org-babel-latex-preamble
        (lambda (_)
          "\\documentclass[tikz]{standalone}\n\\usepackage{tikz}"))
  (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")
  (setq org-format-latex-header
        "\\documentclass{article}\n\\usepackage{tikz}\n\\usepackage{amsmath}\n\\usepackage{amssymb}")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (latex . t)
     (shell . t)
     (C . t))))

;; ---------------------------------------------------------------------------
;; htmlize for org export
;; ---------------------------------------------------------------------------
(use-package htmlize :ensure t)

(provide 'xz-org)
;;; xz-org.el ends here

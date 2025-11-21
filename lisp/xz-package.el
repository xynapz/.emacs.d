;;; xz-package.el --- Package configuration -*- lexical-binding:t; -*-
;;; Commentary:
;; initializing the package repositories and some base initial packages

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Essential packages loaded early
(use-package diminish)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3) ;; adjust delay time (in seconds)
  (setq which-key-popup-type 'side-window) ;; or 'minibuffer or 'frame
  (which-key-setup-side-window-bottom))

(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :config
  ;; Performance
  (setq magit-refresh-status-buffer nil
        magit-git-executable "git"))

;; Projectile - Project management
(use-package projectile
  :diminish
  :init
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-completion-system 'default
        projectile-enable-caching t)
  :config
  (projectile-mode +1))

;; Org mode
(use-package org
  :pin elpa
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-directory "~/org/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-agenda-files (list org-directory)
        org-startup-folded 'content
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ▾"
        org-log-done 'time
        org-log-into-drawer t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
;; TODO keyword faces
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff6c6b" :weight bold))
        ("NEXT" . (:foreground "#da8548" :weight bold))
        ("PROG" . (:foreground "#ECBE7B" :weight bold))
        ("WAIT" . (:foreground "#51afef" :weight bold))
        ("DONE" . (:foreground "#98be65" :weight bold))
        ("CANCELLED" . (:foreground "#5B6268" :weight bold))))

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

;; Nerd icons
(use-package nerd-icons :ensure t)

;; Basic Dired configuration
(use-package dired
  :ensure nil  ; built-in package
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))  ; Quick jump to dired for current file
  :custom
  ;; Use human-readable sizes, group directories first
  (dired-listing-switches "-alhgo --group-directories-first")

  ;; IMPORTANT: Automatically kill buffers for deleted/moved files
  (dired-clean-up-buffers-too t)

  ;; If you have two dired windows open, use the other as default target
  (dired-dwim-target t)

  ;; Kill old dired buffer when opening new directory
  ;; This prevents accumulation of dired buffers
  (dired-kill-when-opening-new-dired-buffer t)

  :config
  ;; Load dired-x for extra features
  (require 'dired-x)

  ;; Enable 'a' command to reuse buffer (instead of creating new ones)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Better key bindings for navigation
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))))

;; Hide dotfiles by default (toggle with '.')
;; (use-package dired-hide-dotfiles
;;   :ensure t
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :bind (:map dired-mode-map
;;               ("." . dired-hide-dotfiles-mode)))

;; Dired subtree - expand/collapse directories in place
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("I" . dired-subtree-remove)))

;; Dired icons
(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Hide details in dired by default, but keep permissions visible
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Customize what dired-hide-details-mode hides
(setq dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil)

(provide 'xz-package)
;;; xz-package.el ends here


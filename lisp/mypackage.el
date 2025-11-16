;;; mypackage.el --- Package configuration -*- lexical-binding:t; -*-
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

;; Which-key - Shows available keybindings
(use-package which-key
  :diminish
  :init
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 6
        which-key-max-display-columns nil
        ;; Prevent showing transient states and other non-explicit bindings
        which-key-allow-evil-operators nil
        which-key-show-operator-state-maps nil
        which-key-show-early-on-C-h t
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  :config
  (which-key-mode)

  ;; Hide digit-argument bindings
  (push '(("\\`[0-9]\\'" . nil) . t) which-key-replacement-alist)
  (push '((nil . "digit-argument") . t) which-key-replacement-alist)
  (push '((nil . "negative-argument") . t) which-key-replacement-alist)

  ;; Hide universal-argument
  (push '((nil . "universal-argument") . t) which-key-replacement-alist)

  ;; Hide other common bindings you don't want to see
  (push '((nil . "self-insert-command") . t) which-key-replacement-alist)
  (push '((nil . "ignore") . t) which-key-replacement-alist)
  (push '((nil . "undefined") . t) which-key-replacement-alist))

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

;; Nerd icons
(use-package nerd-icons :ensure t)

;; Dired icons
(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dashboard
  :ensure t
  :after nerd-icons
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner "~/.emacs.d/banner.png")

  ;; Center only the banner, left-align content
  (setq dashboard-center-content nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-display-icons-p t)

  ;; Horizontal separator between sections
  (setq dashboard-items-default-length 8)
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")))

  ;; Dashboard sections
  (setq dashboard-items '((projects  . 8)
                          (recents   . 8)
                          (agenda    . 5)
                          (bookmarks . 5)))

  ;; Navigator buttons - simplified without icons for now
  (setq dashboard-navigator-buttons
        `((("GitHub" "Browse GitHub"
            (lambda (&rest _) (browse-url "https://github.com")))
           ("Projects" "Open project"
            (lambda (&rest _) (projectile-switch-project)))
           ("Config" "Settings"
            (lambda (&rest _) (find-file user-init-file))))
          (("Agenda" "View agenda"
            (lambda (&rest _) (org-agenda nil "a")))
           ("Capture" "New note"
            (lambda (&rest _) (org-capture)))
           ("Notes" "Browse notes"
            (lambda (&rest _) (find-file "~/org/"))))))

  (setq dashboard-footer-messages '("Happy hacking!"))

  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Dynamic full-width separator
  (setq dashboard-page-separator
        (concat "\n" (make-string (window-width) ?─) "\n\n"))

  ;; Update separator on window resize
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (string= (buffer-name) "*dashboard*")
                (setq dashboard-page-separator
                      (concat "\n" (make-string (window-width) ?─) "\n\n"))
                (dashboard-refresh-buffer))))

  (dashboard-setup-startup-hook)

  ;; Move cursor to first item after dashboard loads
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (run-with-timer 0.1 nil
                              (lambda ()
                                (goto-char (point-min))
                                (search-forward "Projects:" nil t)
                                (forward-line 1)
                                (beginning-of-line)))))

  ;; Also add emacs-state binding
  (define-key dashboard-mode-map (kbd "C-c d") 'dashboard-refresh-buffer))

(provide 'mypackage)
;;; mypackage.el ends here

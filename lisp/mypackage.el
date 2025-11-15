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

;; Evil mode - Vim emulation
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'magit))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode))

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

  ;; Hide all default evil bindings that aren't explicitly defined
  (push '((nil . "evil-") . t) which-key-replacement-alist)
  (push '((nil . "Evil") . t) which-key-replacement-alist)

  ;; Hide evilem (evil-easymotion) bindings
  (push '((nil . "evilem-") . t) which-key-replacement-alist)
  (push '((nil . "^evilem") . t) which-key-replacement-alist)

  ;; Hide digit-argument bindings
  (push '(("\\`[0-9]\\'" . nil) . t) which-key-replacement-alist)
  (push '((nil . "digit-argument") . t) which-key-replacement-alist)
  (push '((nil . "negative-argument") . t) which-key-replacement-alist)

  ;; Hide universal-argument
  (push '((nil . "universal-argument") . t) which-key-replacement-alist)

  ;; Hide other common bindings you don't want to see
  (push '((nil . "self-insert-command") . t) which-key-replacement-alist)
  (push '((nil . "ignore") . t) which-key-replacement-alist)
  (push '((nil . "undefined") . t) which-key-replacement-alist)

  ;; Add custom replacements for your defined prefixes
  (which-key-add-key-based-replacements
    "SPC b" "buffers"
    "SPC f" "files"
    "SPC f e" "emacs"
    "SPC p" "project"
    "SPC w" "windows"
    "SPC t" "toggle"
    "SPC h" "help"
    "SPC n" "notes"
    "SPC c" "code"
    "SPC g" "git"
    "SPC a" "applications"
    "SPC i" "insert"
    "SPC j" "jump"
    "SPC o" "open"
    "SPC q" "quit"
    "SPC s" "search"
    "SPC x" "execute"))

;; General - Better key definition
(use-package general
  :config
  (general-evil-setup t)

  ;; Set up SPC as leader key
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

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

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

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

  ;; Evil mode keybindings
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      "gr" 'dashboard-refresh-buffer
      "gp" 'projectile-switch-project
      "ga" 'org-agenda
      "gc" 'org-capture
      "gn" (lambda () (interactive) (find-file "~/org/"))))

  ;; Also add emacs-state binding
  (define-key dashboard-mode-map (kbd "C-c d") 'dashboard-refresh-buffer))

(provide 'mypackage)
;;; mypackage.el ends here

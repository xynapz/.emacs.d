;;; xz-editor.el --- Editing behavior -*- lexical-binding: t; -*-
;;; Commentary:
;; editor settings.

;;; Code:
(use-package dashboard :ensure t)

(setq-default cursor-type 'bar)
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Disable GUI elements
(use-package scroll-bar :ensure nil :config (scroll-bar-mode -1))
(use-package tool-bar   :ensure nil :config (tool-bar-mode -1))
(use-package menu-bar   :ensure nil :config (menu-bar-mode -1))

;; Visual line mode
(global-visual-line-mode 1)

;; Highlight current line
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1)
  (dolist (hook '(comint-mode-hook eshell-mode-hook term-mode-hook))
    (add-hook hook (lambda () (setq-local global-hl-line-mode nil)))))

;; Install and setup dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs, Angel 🧠")
  (setq dashboard-startup-banner "~/.emacs.d/banner.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)

  ;; Items to show
  (setq dashboard-items '((agenda  . 10)
                          (projects . 5)
                          (recents . 3)))

  ;; Footer messages
  (setq dashboard-footer-messages
        '("C-x C-f to open file | C-x b to switch buffer | C-x C-c to quit"
          "Let Emacs do the heavy lifting."))

  ;; Ensure dashboard is initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

;; Refresh dashboard on client startup
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (equal (buffer-name) "*dashboard*")
              (dashboard-refresh-buffer))))

;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 't
        display-line-numbers-width-start t)
  :config
  (dolist (hook '(prog-mode-hook conf-mode-hook nxml-mode-hook text-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  (dolist (hook '(eshell-mode-hook term-mode-hook vterm-mode-hook
                  shell-mode-hook treemacs-mode-hook org-mode-hook))
    (add-hook hook (lambda () (display-line-numbers-mode 0)))))

;; Doom-like modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-height 30
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t
        doom-modeline-project-detection 'projectile)
  :config
  (doom-modeline-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-Iosvkem t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show matching parentheses
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

;; Indent guides
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0))

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
              ;; ("C-c C-e" . markdown-do)
              ))

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
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Avy for jumping
(use-package avy
  :config
  (setq avy-timeout-seconds 0.3
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'xz-editor)
;;; xz-editor.el ends here

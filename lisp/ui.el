;;; ui.el --- UI configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; ui components of my Emacs config.

;;; Code:
;; Cursor
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

;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 'relative
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
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
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

(provide 'ui)
;;; ui.el ends here

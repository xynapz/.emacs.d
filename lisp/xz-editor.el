;;; xz-editor.el --- Editing behavior -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal editor settings.

;;; Code:
(setq-default cursor-type 'box)
(setq visible-bell nil
      ring-bell-function 'ignore)

(use-package scroll-bar :ensure nil :config (scroll-bar-mode -1))
(use-package tool-bar   :ensure nil :config (tool-bar-mode -1))
(use-package menu-bar   :ensure nil :config (menu-bar-mode -1))

(global-visual-line-mode 1)

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1)
  (dolist (hook '(comint-mode-hook eshell-mode-hook term-mode-hook))
    (add-hook hook (lambda () (setq-local global-hl-line-mode nil)))))

(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 't
        display-line-numbers-width-start t)
  :config
  (dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  (dolist (hook '(eshell-mode-hook term-mode-hook shell-mode-hook org-mode-hook))
    (add-hook hook (lambda () (display-line-numbers-mode 0)))))

;; Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c s") 'save-buffer)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c =") 'text-scale-increase)
(global-set-key (kbd "C-c -") 'text-scale-decrease)
(global-set-key (kbd "C-c /") 'comment-line)

(use-package winner
  :ensure nil
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo))
  :config (winner-mode 1))

(windmove-default-keybindings)

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-vcs-max-length 12
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
  (doom-themes-org-config))

;; Show matching parentheses
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; Electric Pairs
(electric-pair-mode 1)

;; Whitespace cleanup on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Compile settings
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output t)
  (compilation-ask-about-save nil)
  :hook
  (compilation-filter . (lambda ()
                          (ansi-color-apply-on-region
                           compilation-filter-start (point)))))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(provide 'xz-editor)
;;; xz-editor.el ends here

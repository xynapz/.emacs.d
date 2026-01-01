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

;; Session persistence (restore windows/buffers)
(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode 1))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1)
  (dolist (hook '(comint-mode-hook eshell-mode-hook term-mode-hook))
    (add-hook hook (lambda () (setq-local global-hl-line-mode nil)))))

(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t)
  :config
  (global-display-line-numbers-mode 1)
  
  ;; Disable in specific modes where it's clutter
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  treemacs-mode-hook
                  pdf-view-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

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

(use-package doom-modeline
  :ensure t
  :init
  (setq
   ;; Size / appearance
   doom-modeline-height 26
   doom-modeline-bar-width 4
   ;; File & buffer info
   doom-modeline-buffer-file-name-style 'truncate-upto-project
   doom-modeline-buffer-state-icon t     ;; modified / read-only
   doom-modeline-buffer-modification-icon t
   doom-modeline-file-size t              ;; useful for large files
   ;; Icons
   doom-modeline-icon t
   doom-modeline-major-mode-icon t
   doom-modeline-major-mode-color-icon t
   ;; Position info
   doom-modeline-position-line-format '("%l")
   doom-modeline-position-column-format '("%c")
   doom-modeline-percent-position nil
   ;; Git
   doom-modeline-vcs-max-length 18
   ;; LSP
   doom-modeline-lsp t
   ;; Encoding (only show when not UTF-8)
   doom-modeline-buffer-encoding t
   doom-modeline-buffer-encoding-conditional t
   ;; Indentation (tabs vs spaces)
   doom-modeline-indent-info t
   ;; Project
   doom-modeline-project-detection 'projectile
   ;; Minor modes (noise → off)
   doom-modeline-minor-modes nil
   ;; Workspace / window number
   doom-modeline-workspace-name t
   doom-modeline-window-width-limit 80)

  :config
  (doom-modeline-mode 1))


(use-package nord-theme
  :ensure t
  :config
  (load-theme 'xz-nord t))

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
;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode 1))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(provide 'xz-editor)
;;; xz-editor.el ends here

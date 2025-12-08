;;; xz-editor.el --- Editing behavior -*- lexical-binding: t; -*-
;;; Commentary:
;; editor settings.

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
  (dolist (hook '(prog-mode-hook conf-mode-hook nxml-mode-hook text-mode-hook))
    (add-hook hook #'display-line-numbers-mode))
  (dolist (hook '(eshell-mode-hook term-mode-hook vterm-mode-hook
                  shell-mode-hook treemacs-mode-hook org-mode-hook))
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
(global-set-key (kbd "C-c 0") 'text-scale-adjust)
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c M-/") 'comment-or-uncomment-region)

(use-package winner
  :ensure nil
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo))
  :config (winner-mode 1))

(windmove-default-keybindings)

;; Doom-like modeline
(use-package doom-modeline
  :ensure t
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
  :hook (prog-mode . (lambda ()
                       (when (and (buffer-file-name)
                                  (not (bound-and-true-p org-export-current-backend)))
                         (highlight-indent-guides-mode))))

  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0))

;; Electric Pairs
(electric-pair-mode 1)

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
  :init (setq markdown-command "multimarkdown"))

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

;; Vundo - Visual Undo
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Avy for jumping
(use-package avy
  :bind (("M-g c" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.3
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Git Gutter (diff-hl)
(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; Semantic Selection (expand-region)
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setq mc/always-run-for-all t))

(provide 'xz-editor)
;;; xz-editor.el ends here

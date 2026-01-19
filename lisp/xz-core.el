;;; xz-core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs defaults, session management, and file handling.

;;; Code:

;; General
(setq-default
 fill-column 80
 tab-width 4
 indent-tabs-mode nil
 sentence-end-double-space nil)

;; UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; No backup/lock files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Quiet
(setq visible-bell nil
      ring-bell-function 'ignore)

(blink-cursor-mode -1)

;; Auto-revert buffers when file changes
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Recent files
(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200))

;; Save history (minibuffer)
(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

;; Save place in files
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

;; Window Management
(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(provide 'xz-core)
;;; xz-core.el ends here

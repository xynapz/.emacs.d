;;; xz-core.el --- Core settings -*-lexical-binding: t; -*-
;;; Commentary:
;; code config.

;;; Code:
;; UTF-8 everywhere
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Startup behavior
;; (setopt initial-scratch-message nil
(setq inhibit-startup-screen nil)
;;         initial-major-mode 'fundamental-mode)

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (if (and (display-graphic-p)
                       (fboundp 'fancy-startup-screen))
                  (fancy-startup-screen)
                (display-startup-screen)))))

;; Reduce clutter
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      auto-save-list-file-prefix nil)

;; Frame basics
(add-to-list 'default-frame-alist '(alpha-background . 100))
(add-to-list 'default-frame-alist '(undecorated . t))
(setopt frame-title-format "%b")

;; Fonts
(set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 194)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font" :height 194)
(set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font" :height 194)

;; Performance
(setq gc-cons-threshold 100000000) ; 100MB
(setq read-process-output-max (* 1024 1024)) ; 1MB

(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)

;; Better defaults
(setq-default fill-column 80
              sentence-end-double-space nil
              tab-width 4
              indent-tabs-mode nil)

;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Yes/No prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Recent files (defer loading)
(add-hook 'emacs-startup-hook
          (lambda () (recentf-mode 1)))

(provide 'xz-core)
;;; xz-core.el ends here

;;; xz-ui.el --- UI and Theme configuration -*- lexical-binding: t; -*-

;;; Code:

;; Fonts
(defun xz/setup-fonts ()
  "Setup fonts."
  (set-face-attribute 'default nil :family "Iosevka" :height 194)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 194)
  (set-face-attribute 'variable-pitch nil :family "Iosevka" :height 194))

;; Run font setup on startup and when creating a new frame (daemon mode support)
(xz/setup-fonts)
(add-hook 'server-after-make-frame-hook #'xz/setup-fonts)

;; Theme
(use-package nord-theme
  :ensure t
  :config
  ;; Add themes directory to custom-theme-load-path
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
  ;; Load custom nord theme if available, else standard
  (if (file-exists-p (expand-file-name "themes/xz-nord-theme.el" user-emacs-directory))
      (load-theme 'xz-nord t)
    (load-theme 'nord t)))

;; Icons
(use-package nerd-icons
  :ensure t)

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 26
        doom-modeline-bar-width 4
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info t
        doom-modeline-lsp t
        doom-modeline-env-version t)
  :config
  (doom-modeline-mode 1))

;; Ensure default Emacs welcome screen is shown
(setq inhibit-startup-screen nil)
(setq initial-buffer-choice nil)

(provide 'xz-ui)
;;; xz-ui.el ends here

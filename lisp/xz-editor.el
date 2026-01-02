;;; xz-editor.el --- Editing and Evil configuration -*- lexical-binding: t; -*-

;;; Code:

;; General Editing
(use-package elect-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

;; Line Numbers (Relative)
(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t)
  :config
  (global-display-line-numbers-mode 1)

  ;; Disable in some modes
  (dolist (mode '(term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  pdf-view-mode-hook
                  vterm-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Indentation Defaults (Global)
;; Standard: Spaces > Tabs
(setq-default indent-tabs-mode nil  ; Use spaces
              tab-width 4           ; Default width
              fill-column 80)       ; Default line length


(provide 'xz-editor)
;;; xz-editor.el ends here

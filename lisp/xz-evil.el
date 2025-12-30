;;; xz-evil.el --- Evil mode support -*- lexical-binding: t; -*-
;;; Commentary:
;; Plug-and-play Evil mode. Load this file to enable Vim bindings.

;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  ;; Disable evil in magit (restore standard magit bindings)
  (setq evil-collection-mode-list (remove 'magit evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(provide 'xz-evil)
;;; xz-evil.el ends here

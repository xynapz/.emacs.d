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
  (evil-collection-init))

(provide 'xz-evil)
;;; xz-evil.el ends here

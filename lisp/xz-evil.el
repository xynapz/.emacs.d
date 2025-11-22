;;; xz-evil.el --- Vim emulation -*- lexical-binding: t; -*-
;;; Commentary:
;; Evil mode configuration for Vim-like editing.
;; Includes evil-collection for broad package support.

;;; Code:

;; Core Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ; Required for evil-collection
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)

  ;; Initial state for some modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil Collection - Keybindings for other packages
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Evil Surround - s/S for surrounding
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Evil Commentary - gc for comments
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; Integration with other packages
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(provide 'xz-evil)
;;; xz-evil.el ends here

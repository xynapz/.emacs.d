;;; xz-evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Dedicated module for Evil Mode and its extensions.
;; ensuring robust keybindings and collection setup.

;;; Code:

;; 1. Evil Core
(use-package evil
  :ensure t
  :init
  ;; These variables must be set before evil-mode is loaded.
  ;; Note: evil-want-keybinding is set in init.el / early-init.el
  (setq evil-want-integration t
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-u-delete t
        evil-undo-system 'undo-redo
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  
  ;; C-g to exit insert state
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  
  ;; jk to escape
  (define-key evil-insert-state-map (kbd "j")
    (lambda () (interactive)
      (let ((evt (read-event nil nil 0.15)))
        (if (and evt (eq evt ?k))
            (evil-normal-state)
          (insert "j")
          (when evt (push evt unread-command-events)))))))

;; 2. Evil Collection (The Fixer)
(use-package evil-collection
  :after evil
  :ensure t
  :init
  ;; Pre-load settings required for minibuffer and bindings
  (setq evil-collection-setup-minibuffer t
        evil-collection-calendar-want-org-bindings t)
  :config
  ;; Initialize ALL modes
  (evil-collection-init)
  
  ;; Force Dired re-initialization just in case
  (with-eval-after-load 'dired
    (evil-collection-init 'dired)))

;; 3. Evil Surround
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; 4. Evil Commentary
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode 1))

;; 5. Folding Support (for evil-fold-action)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(provide 'xz-evil)
;;; xz-evil.el ends here

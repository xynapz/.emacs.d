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

;; Evil Mode
(use-package evil
  :ensure t
  :preface
  (customize-set-variable 'evil-want-integration t)
  (customize-set-variable 'evil-want-keybinding nil)
  (customize-set-variable 'evil-undo-system 'undo-redo)
  (customize-set-variable 'evil-want-C-u-scroll t)
  (customize-set-variable 'evil-want-C-u-delete t)
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

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-calendar-want-org-bindings t))


(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode 1))

(provide 'xz-editor)
;;; xz-editor.el ends here

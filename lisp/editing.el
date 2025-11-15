;;; editing.el --- Editing behavior -*- lexical-binding: t; -*-
;;; Commentary:
;; editor settings.

;;; Code:
;; Dired
(use-package dired
  :ensure nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-Ahl --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package diredfl
  :after dired
  :config (diredfl-global-mode 1))

;; Better dired navigation with evil
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file
    (kbd "RET") 'dired-find-file))

;; Smartparens
(use-package smartparens
  :diminish
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil))

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

;; Better undo
(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-hist/" user-emacs-directory))))
  (global-undo-tree-mode))

;; Multiple cursors
(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

;; Move text
(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; Better search/replace
(use-package anzu
  :diminish
  :config
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil))

(use-package evil-anzu
  :after (evil anzu))

;; Better completion in region
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; Avy for jumping
(use-package avy
  :config
  (setq avy-timeout-seconds 0.3
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

(provide 'editing)
;;; editing.el ends here

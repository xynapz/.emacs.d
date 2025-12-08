;;; xz-completion.el --- Completion and search -*- lexical-binding: t; -*-
;;; Commentary:
;; Completion frameworks and search optimization

;;; Code:

;;Vertico
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15))

;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Rich annotations
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Consult
(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g o" . consult-outline)
         ("C-x r b" . consult-bookmark)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)))

;; embark
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; embark-consult
(use-package embark-consult
  :after (embark consult))

;; Corfu for in-buffer completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect 'first) ; Always select the first candidate
  (corfu-on-exact-match nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-echo-mode)
  (corfu-popupinfo-mode)
  :custom-face
  (corfu-current ((t (:inherit (highlight bold) :background "#3e4451" :foreground "white")))))

;; Add icons to completion
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape for additional completion backends
(use-package cape
  :ensure t
  :init
  ;; adding cape backends to the end (append) so they don't override LSP
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))

;; Eglot (Keybindings)
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format)
              ("C-c e o" . eglot-code-action-organize-imports)
              ("C-h ." . eldoc-doc-buffer))
  :config
  ;; Performance optimizations
  (setq eglot-events-buffer-size 0        ; Disable logging for speed
        eglot-connect-timeout 60          ; Increase timeout for heavy LSPs
        eglot-autoshutdown t))            ; Shutdown unused servers

(use-package consult-projectile
  :ensure t
  :after (consult projectile))

;; Helpful - Better help buffers
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

;; Wgrep - Editable grep buffers
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'xz-completion)
;;; xz-completion.el ends here

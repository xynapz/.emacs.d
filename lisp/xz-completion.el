;;; xz-completion.el --- Completion -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal completion setup

;;; Code:

;; Vertico
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10))

;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia
(use-package marginalia
  :init (marginalia-mode))

;; Consult
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s r" . consult-ripgrep)))

;; Corfu (in-buffer completion)
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preselect 'first)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Eglot LSP
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format))
  :config
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t)

  ;; Consolidated LSP server configuration
  (with-eval-after-load 'eglot
    ;; C/C++ (clangd)
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--clang-tidy")))
    ;; Python (pyright)
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
    ;; JS/TS (typescript-language-server)
    (add-to-list 'eglot-server-programs
                 '((js-mode js-ts-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
    ;; Web/HTML (vscode-html-language-server)
    (add-to-list 'eglot-server-programs
                 '((web-mode mhtml-mode) . ("vscode-html-language-server" "--stdio")))
    ;; CSS (vscode-css-language-server)
    (add-to-list 'eglot-server-programs
                 '((css-mode css-ts-mode) . ("vscode-css-language-server" "--stdio")))))

;; Helpful
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(provide 'xz-completion)
;;; xz-completion.el ends here

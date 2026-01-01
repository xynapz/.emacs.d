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
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 2000000)

  ;; Consolidated LSP server configuration
  (with-eval-after-load 'eglot
    (let ((servers
           '((("clangd" "--clang-tidy") . (c-mode c++-mode c-ts-mode c++-ts-mode))
             (("pyright-langserver" "--stdio") . (python-mode python-ts-mode))
             (("typescript-language-server" "--stdio") . (js-mode js-ts-mode typescript-ts-mode tsx-ts-mode))
             (("vscode-html-language-server" "--stdio") . (web-mode mhtml-mode))
             (("vscode-css-language-server" "--stdio") . (css-mode css-ts-mode)))))
      
      (dolist (server servers)
        (let ((cmd (car (car server)))     ; executable name
              (full-cmd (car server))      ; full command list
              (modes (cdr server)))        ; affected modes
          
          (if (executable-find cmd)
              (progn
                ;; 1. Register server
                (add-to-list 'eglot-server-programs `(,modes . ,full-cmd))
                ;; 2. Add eglot-ensure hook ONLY for supported modes
                (dolist (m modes)
                  (let ((hook-name (intern (concat (symbol-name m) "-hook"))))
                    (add-hook hook-name #'eglot-ensure))))
            (message "[XZ-Config] Warning: %s not found. Skipping LSP setup for %s" cmd modes)))))))

;; Helpful
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(provide 'xz-completion)
;;; xz-completion.el ends here

;;; python.el --- Python support -*- lexical-binding: t; -*-

(require 'eglot)
(require 'treesit)

;; Python configuration using Tree-sitter
(use-package python-ts-mode
  :ensure nil ; Built-in in Emacs 29+
  :mode "\\.py\\'"
  :hook ((python-ts-mode . eglot-ensure)
         ;; Auto-format on save using black/autopep8 via Eglot
         (python-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))))

;; Eglot configuration for Python (Pyright or Pylsp)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

(provide 'python)
;;; python.el ends here

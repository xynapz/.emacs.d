;;; py.el --- Python support -*- lexical-binding: t; -*-

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

;; loads the project's venv env
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)

  ;; Auto-activate venv when switching projects
  (defun my/auto-activate-venv ()
    (let ((venv-path (concat (projectile-project-root) ".venv")))
      (when (file-directory-p venv-path)
        (pyvenv-activate venv-path))))

  (add-hook 'projectile-after-switch-project-hook 'my/auto-activate-venv))

(provide 'py)
;;; py.el ends here

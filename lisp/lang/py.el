;;; py.el --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal Python config with pyright LSP

;;; Code:

;; Python mode (use tree-sitter if available)
(when (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

;; Eglot with pyright
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

;; Pyvenv for virtualenvs
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)

  (defun xz/auto-activate-venv ()
    "Auto-activate .venv in project root."
    (let ((venv (concat (projectile-project-root) ".venv")))
      (when (file-directory-p venv)
        (pyvenv-activate venv))))

  (add-hook 'projectile-after-switch-project-hook #'xz/auto-activate-venv))

;; Format on save (requires black or autopep8)
(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
(add-hook 'python-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

(provide 'py)
;;; py.el ends here

;;; py.el --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal Python config with pyright LSP

;;; Code:

;; Require eglot (built-in since Emacs 29)
(require 'eglot)

;; Python mode (use tree-sitter if available)
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

;; Eglot with pyright
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)



(defun xz/verify-python-deps ()
  "Check if python dependencies (node, pyright) are met."
  (unless (executable-find "node")
    (display-warning 'xz-python "Node.js not found! Pyright requires 'node' in PATH." :warning))
  (unless (executable-find "pyright-langserver")
    (display-warning 'xz-python "pyright-langserver not found! Install it with npm/pip." :warning)))

(add-hook 'python-mode-hook #'xz/verify-python-deps)
(add-hook 'python-ts-mode-hook #'xz/verify-python-deps)

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

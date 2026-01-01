;;; py.el --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal Python config with pyright LSP

;;; Code:



;; Python mode (use tree-sitter if available)
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))


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

  ;; Also activate when opening a Python file directly
  (defun xz/maybe-activate-venv ()
    "Activate venv if in a project with .venv."
    (when (and (buffer-file-name)
               (derived-mode-p 'python-mode 'python-ts-mode))
      (let* ((root (or (projectile-project-root)
                       (locate-dominating-file default-directory ".venv")))
             (venv (when root (expand-file-name ".venv" root))))
        (when (and venv (file-directory-p venv))
          (unless (and pyvenv-virtual-env
                       (string= (file-truename pyvenv-virtual-env)
                                (file-truename venv)))
            (pyvenv-activate venv))))))

  (add-hook 'python-mode-hook #'xz/maybe-activate-venv)
  (add-hook 'python-ts-mode-hook #'xz/maybe-activate-venv)
  (add-hook 'projectile-after-switch-project-hook #'xz/auto-activate-venv)

;; Restart LSP when venv changes
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (lsp-workspace-restart))))

  (add-hook 'pyvenv-post-deactivate-hooks
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (lsp-workspace-restart)))))

;; Format on save
(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
(add-hook 'python-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(provide 'py)
;;; py.el ends here

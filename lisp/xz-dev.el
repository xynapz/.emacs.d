;;; xz-dev.el --- Development configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-refresh-status-buffer nil))

(use-package treesit
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.c\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode))
  :config
  (setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp"))))

(defun xz/install-treesit-grammars ()
  "Install all configured tree-sitter grammars."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (treesit-install-language-grammar lang)))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-completion-provider :none  ; Use Corfu
        lsp-headerline-breadcrumb-enable nil
        read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t))

(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm-toggle)
  :config
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t)

(provide 'xz-dev)
;;; xz-dev.el ends here

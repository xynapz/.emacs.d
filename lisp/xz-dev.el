;;; xz-dev.el --- Development configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien)
  :config
(projectile-mode +1)
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-refresh-status-buffer nil))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

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
  :commands (lsp lsp-deferred)
  :hook ((python-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-completion-provider :none  ; Use Corfu
        lsp-headerline-breadcrumb-enable nil
        lsp-auto-configure t
        lsp-auto-guess-root t          ; Don't ask to import projects
        read-process-output-max (* 1024 1024))
  
  ;; Force the prefix map (C-c l) because the variable sometimes fails
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (define-key lsp-command-map (kbd "e") 'consult-lsp-diagnostics) ; Explicitly bind error list
  (define-key lsp-command-map (kbd "=") 'lsp-format-buffer)       ; Explicitly bind format

  ;; Evil bindings for LSP
  (with-eval-after-load 'evil
    (evil-define-key 'normal lsp-mode-map
      (kbd "K") 'lsp-ui-doc-glance
      (kbd "gd") 'lsp-find-definition
      (kbd "gr") 'lsp-find-references
      (kbd "gD") 'lsp-find-declaration
      (kbd "gi") 'lsp-find-implementation
      (kbd "gt") 'lsp-find-type-definition
      (kbd "ga") 'lsp-execute-code-action
      (kbd "rn") 'lsp-rename
      ;; Error Navigation
      (kbd "]e") 'flymake-goto-next-error
      (kbd "[e") 'flymake-goto-prev-error)))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; Python Environment
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(defun xz/auto-activate-python-env ()
  "Automatically activate .venv if it exists in the project root."
  (let* ((root (projectile-project-root))
         (venv-path (and root (expand-file-name ".venv" root))))
    (when (and venv-path (file-exists-p venv-path))
      (pyvenv-activate venv-path)
      (message "Activated venv: %s" venv-path))))

(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (xz/auto-activate-python-env) ; Activate venv before LSP
                          (lsp-deferred))))

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :bind ("C-c t" . vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project))

(provide 'xz-dev)
;;; xz-dev.el ends here

;;; xz-dev.el --- Development configuration -*- lexical-binding: t; -*-

;;; Code:

;; PROJECT & VERSION CONTROL

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien)
  :config
  (projectile-mode +1))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-refresh-status-buffer nil))

;; CODE FORMATTING

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)

  ;; JavaScript/TypeScript/Web (2 spaces)
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . black))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(css-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(html-mode . prettier))

  ;; Compiled languages - manual formatting only (not auto)
  (add-to-list 'apheleia-mode-alist '(go-ts-mode . goimports))
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode . rustfmt))
  (add-to-list 'apheleia-mode-alist '(terraform-mode . terraform))
  (add-to-list 'apheleia-mode-alist '(bash-ts-mode . shfmt))

  ;; Ensure formatters are defined
  (unless (alist-get 'goimports apheleia-formatters)
    (setf (alist-get 'goimports apheleia-formatters)
          '("goimports")))

  (unless (alist-get 'rustfmt apheleia-formatters)
    (setf (alist-get 'rustfmt apheleia-formatters)
          '("rustfmt" "--quiet" "--emit" "stdout")))

  (unless (alist-get 'terraform apheleia-formatters)
    (setf (alist-get 'terraform apheleia-formatters)
          '("terraform" "fmt" "-")))

  (unless (alist-get 'shfmt apheleia-formatters)
    (setf (alist-get 'shfmt apheleia-formatters)
          '("shfmt" "-i" "2" "-s")))

  ;; DISABLE auto-format on save for compiled/DevOps languages
  (setq-default apheleia-mode-alist
                (seq-filter (lambda (pair)
                              (not (memq (car pair)
                                         '(go-ts-mode rust-ts-mode terraform-mode bash-ts-mode))))
                            apheleia-mode-alist)))

;; Web Development Indentation (2 Spaces)
(dolist (mode '(html-mode-hook
                css-ts-mode-hook
                js-ts-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook mode (lambda ()
                   (setq-local tab-width 2)
                   (setq-local treesit-indent-offset 2)
                   (setq-local css-indent-offset 2)
                   (setq-local js-indent-level 2)
                   (setq-local typescript-indent-level 2))))

;; TREE-SITTER CONFIGURATION

(use-package treesit
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.c\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.rs\\'" . rust-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\.toml\\'" . toml-ts-mode)
         ("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.zsh\\'" . bash-ts-mode))
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
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown"))))

(defun xz/install-treesit-grammars ()
  "Install all configured tree-sitter grammars."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (treesit-install-language-grammar lang)))

(defun xz/format-buffer ()
  "Format buffer using standard formatter, automatically fixing indentation syntax errors first."
  (interactive)
  ;; 1. Fix Indentation Errors (Editor heuristics)
  (indent-region (point-min) (point-max))
  ;; 2. Run Formatter (Black/Prettier)
  (if (fboundp 'apheleia-format-buffer)
      (call-interactively 'apheleia-format-buffer)
    (lsp-format-buffer)))

;; C/C++ SMART HEADER DETECTION
(defun xz/detect-c-or-cpp-header ()
  "Detect whether a .h file should use C or C++ mode.
Defaults to C++ but checks for C++ patterns in content and corresponding files."
  (let* ((filename (buffer-file-name))
         (base-name (file-name-sans-extension filename))
         (has-cpp-file (or (file-exists-p (concat base-name ".cc"))
                           (file-exists-p (concat base-name ".cpp"))
                           (file-exists-p (concat base-name ".cxx"))))
         (has-c-file (file-exists-p (concat base-name ".c")))
         (content-suggests-cpp
          (save-excursion
            (goto-char (point-min))
            (or (re-search-forward "\\bclass\\b" nil t)
                (re-search-forward "\\bnamespace\\b" nil t)
                (re-search-forward "\\btemplate\\b" nil t)
                (re-search-forward "\\bpublic:\\|private:\\|protected:" nil t)
                (re-search-forward "::\\|std::" nil t)
                (re-search-forward "\\bconstexpr\\b" nil t)
                (re-search-forward "\\bauto\\b.*=" nil t)))))
    ;; Decision logic
    (cond
     (content-suggests-cpp 'c++-ts-mode)
     (has-cpp-file 'c++-ts-mode)
     (has-c-file 'c-ts-mode)
     (t 'c++-ts-mode)))) ; Default to C++

(defun xz/setup-h-file-mode ()
  "Automatically select C or C++ mode for .h files."
  (let ((detected-mode (xz/detect-c-or-cpp-header)))
    (funcall detected-mode)))

;; Apply smart detection to .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . xz/setup-h-file-mode))

;; Toggle command for manual override
(defun xz/toggle-c-cpp-mode ()
  "Toggle between C and C++ mode for the current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'c-ts-mode)
    (c++-ts-mode)
    (message "Switched to C++ mode"))
   ((derived-mode-p 'c++-ts-mode)
    (c-ts-mode)
    (message "Switched to C mode"))
   (t
    (message "Not in a C/C++ buffer"))))

(global-set-key (kbd "C-c C-t") 'xz/toggle-c-cpp-mode)

;; LSP MODE CONFIGURATION
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
         (go-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (toml-ts-mode . lsp-deferred)
         (dockerfile-ts-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-disabled-clients '(pyright)) ; Explicitly prefer Pylsp over Pyright
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)                   ; Disable IO logging for performance
  (lsp-completion-provider :none)    ; Use Corfu instead
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-diagnostics-enable nil) ; Use flymake instead
  (lsp-modeline-code-actions-enable nil)
  (lsp-lens-enable nil)              ; Disable code lens (perf heavy)
  (lsp-signature-auto-activate nil)  ; Manual signature help
  (lsp-signature-render-documentation nil)
  (lsp-enable-file-watchers nil)     ; Disable file watchers (big perf gain)
  (lsp-enable-folding nil)           ; Use native folding
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)

  :config
  (define-key lsp-command-map (kbd "e") 'consult-lsp-diagnostics)
  (define-key lsp-command-map (kbd "=") 'xz/format-buffer)
  (define-key lsp-command-map (kbd "s") 'lsp-treemacs-symbols)
  (define-key lsp-command-map (kbd "E") 'lsp-treemacs-errors-list)
  (define-key lsp-command-map (kbd "t") 'xz/toggle-c-cpp-mode)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  ;; Language-specific LSP settings

  ;; Golang
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.staticcheck" t t)))

  ;; Rust
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t)

  ;; YAML (Kubernetes & Docker Compose)
  (setq lsp-yaml-schemas
        '((kubernetes . ["/k8s/*.yaml" "/kubernetes/*.yaml"])
          (docker-compose . ["docker-compose*.yaml" "compose*.yaml"])))

  ;; Terraform
  (setq lsp-terraform-ls-enable-show-reference t
        lsp-terraform-ls-prefill-required-fields t)

  ;; Python (Pylsp)
  (setq lsp-pylsp-plugins-black-enabled t
        lsp-pylsp-plugins-isort-enabled t
        lsp-pylsp-plugins-flake8-enabled t))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
  :config
  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)))

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1))

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

;; LANGUAGE-SPECIFIC CONFIGURATIONS

;; Python
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

(add-hook 'python-ts-mode-hook (lambda ()
                                 (xz/auto-activate-python-env)
                                 (lsp-deferred)))

;; Golang
;; Prerequisites: go install golang.org/x/tools/gopls@latest
;;                go install golang.org/x/tools/cmd/goimports@latest

(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode t)))

;; Rust
;; Prerequisites: rustup component add rust-analyzer
;;                rustup component add rustfmt

(add-hook 'rust-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)))

;; YAML
(add-hook 'yaml-ts-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode nil)))

;; Kubernetes & Docker Compose detection
(add-to-list 'auto-mode-alist '("/k8s/.*\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("/kubernetes/.*\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("docker-compose\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("compose\\.ya?ml\\'" . yaml-ts-mode))

;; TOML
(add-hook 'toml-ts-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("Cargo\\.lock\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-ts-mode))

;; Bash/Shell
(add-hook 'bash-ts-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("bashrc\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("zshrc\\'" . bash-ts-mode))

;; Dockerfile
;; Prerequisites: npm install -g dockerfile-language-server-nodejs

(use-package dockerfile-ts-mode
  :ensure nil
  :mode ("Dockerfile\\'" "\\dockerfile\\'"))

(use-package dockerfile-mode
  :ensure t
  :defer t)

;; Terraform
;; Prerequisites: sudo dnf install terraform-ls
;;                sudo dnf install terraform

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" "\\.tfvars\\'"))

;; Nginx
(use-package nginx-mode
  :ensure t
  :mode ("nginx\\.conf\\'" "/nginx/.*\\.conf\\'"))

;; Environment Files
(use-package dotenv-mode
  :ensure t
  :mode ("\\.env\\'" "\\.env\\..*\\'"))

;; Markdown
;; Prerequisites (optional): brew install marksman

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local tab-width 2)
              (setq-local indent-tabs-mode nil)
              (when (executable-find "marksman")
                (lsp-deferred)))))

;; Terminal
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

(add-to-list 'display-buffer-alist
             '("^\\*vterm\\*"
               (display-buffer-reuse-window
                display-buffer-at-bottom)
               (window-height . 0.5)))

(provide 'xz-dev)
;;; xz-dev.el ends here

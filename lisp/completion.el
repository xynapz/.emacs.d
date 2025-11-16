;;; completion.el --- Completion and search -*- lexical-binding: t; -*-
;;; Commentary:
;; completion framewors for editor and minibuffers.

;;; Code:
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15))

;; Marginalia - Rich annotations
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Consult - Enhanced commands
(use-package consult
  :bind (;; Buffer switching
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; Editing
         ("M-y" . consult-yank-pop)
         ;; Navigation
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ;; Search
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep))
  :custom
  (consult-narrow-key "<")
  (consult-project-function #'projectile-project-root)
  :config
  ;; Ignore directories for ripgrep
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / \
--smart-case --no-heading --with-filename --line-number --search-zip \
--glob=!.git/ \
--glob=!node_modules/ \
--glob=!build/ \
--glob=!dist/ \
--glob=!target/ \
--glob=!elpa/ \
--glob=!eln-cache/ \
--glob=!venv/ \
--glob=!.venv/ \
--glob=!vendor/ \
--glob=!__pycache__/ \
--glob=!*.pyc \
--glob=!.next/ \
--glob=!.cache/ \
--glob=!.nuxt/ \
--glob=!coverage/ \
--glob=!.pytest_cache/ \
--glob=!.cargo/ \
--glob=!Cargo.lock \
--glob=!package-lock.json \
--glob=!yarn.lock \
--glob=!pnpm-lock.yaml")

  ;; Ignore directories for grep
  (setq consult-grep-args
        "grep --null --line-buffered --color=never --ignore-case \
--exclude-dir=.git \
--exclude-dir=node_modules \
--exclude-dir=build \
--exclude-dir=dist \
--exclude-dir=target \
--exclude-dir=elpa \
--exclude-dir=eln-cache \
--exclude-dir=venv \
--exclude-dir=.venv \
--exclude-dir=vendor \
--exclude-dir=__pycache__ \
--exclude-dir=.next \
--exclude-dir=.cache \
--exclude-dir=.nuxt \
--exclude-dir=coverage \
--exclude-dir=.pytest_cache \
--exclude-dir=.cargo \
--line-number -I -r")

  ;; Ignore directories for find
  (setq consult-find-args
        "find . -not ( \
-path '*/.git/*' -o \
-path '*/node_modules/*' -o \
-path '*/build/*' -o \
-path '*/dist/*' -o \
-path '*/target/*' -o \
-path '*/elpa/*' -o \
-path '*/eln-cache/*' -o \
-path '*/venv/*' -o \
-path '*/.venv/*' -o \
-path '*/vendor/*' -o \
-path '*/__pycache__/*' -o \
-path '*/.next/*' -o \
-path '*/.cache/*' -o \
-path '*/.nuxt/*' -o \
-path '*/coverage/*' -o \
-path '*/.pytest_cache/*' -o \
-path '*/.cargo/*' )"))

;; Embark - Contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (embark-quit-after-action nil)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Company - In-buffer completion
(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-nerd-icons
        company-box-doc-delay 0.3))

;; Flycheck - Syntax checking
(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3))

;; Helpful - Better help buffers
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(provide 'completion)
;;; completion.el ends here

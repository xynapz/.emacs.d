;;; xz-package.el --- Package configuration -*- lexical-binding:t; -*-
;;; Commentary:
;; Minimal package setup

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

(use-package which-key
  :defer 1
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package magit
  :commands (magit-status magit-dispatch)
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-refresh-status-buffer nil))

;; Projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien)
  :config
  (projectile-mode +1))

;; Org mode config moved to xz-org.el

;; Nerd icons
(use-package nerd-icons :ensure t)

;; Dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; LaTeX
(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . visual-line-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-engine 'xetex
        TeX-view-program-selection '((output-pdf "PDF Tools"))))

;; PDF Tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

;; Tree-sitter (minimal - just Python and JS)
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(defun xz/install-treesit-grammars (&optional force)
  "Install tree-sitter grammars.
If FORCE is non-nil, reinstall even if already available."
  (interactive "P")
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (if (and (not force) (treesit-language-available-p lang))
        (message "Tree-sitter grammar for %s already installed." lang)
      (message "Installing tree-sitter grammar for %s..." lang)
      (condition-case err
          (progn
            (treesit-install-language-grammar lang)
            (message "Successfully installed tree-sitter grammar for %s" lang))
        (error
         (message "Failed to install %s: %s" lang (error-message-string err)))))))

(use-package treesit
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.json\\'" . json-ts-mode)))

(use-package restart-emacs
  :ensure t)

(defun xz/clean-restart ()
  "Clear caches and restart Emacs (like VS Code 'Reload Window')."
  (interactive)
  (when (y-or-n-p "Clear caches and restart? ")
    ;; Clear Projectile cache
    (when (fboundp 'projectile-invalidate-cache)
      (projectile-invalidate-cache nil))
    
    ;; Save desktop (session) before killing
    (when (bound-and-true-p desktop-save-mode)
      (desktop-save-in-desktop-dir))
      
    ;; Clear file-based caches if necessary (careful with this)
    (let ((cache-dir (expand-file-name ".cache" user-emacs-directory)))
      (when (file-exists-p cache-dir)
        (delete-directory cache-dir t)))
        
    (restart-emacs)))

(global-set-key (kbd "C-c r r") #'xz/clean-restart)

(defun xz/init-setup ()
  "Run one-time setup: Icons, Grammars, PDF Tools."
  (interactive)
  (when (y-or-n-p "Install Nerd Icons? ")
    (require 'nerd-icons)
    (nerd-icons-install-fonts t))
  (when (y-or-n-p "Install Tree-sitter Grammars? ")
    (xz/install-treesit-grammars))
  (when (y-or-n-p "Install PDF Tools? ")
    (pdf-tools-install)))

(provide 'xz-package)
;;; xz-package.el ends here

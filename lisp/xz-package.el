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

;; Org mode (minimal)
(use-package org
  :pin elpa
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq org-directory "~/org/"
        org-startup-folded 'content
        org-startup-indented t
        org-hide-emphasis-markers t
        org-ellipsis " ▾"
        org-log-done 'time
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-confirm-babel-evaluate nil)
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (setq org-babel-latex-preamble
        (lambda (_)
          "\\documentclass[tikz]{standalone}\n\\usepackage{tikz}"))
  (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")
  (setq org-format-latex-header
        "\\documentclass{article}\n\\usepackage{tikz}\n\\usepackage{amsmath}\n\\usepackage{amssymb}")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (latex . t)
     (shell . t)
     (C . t))))

;; htmlize for org export
(use-package htmlize :ensure t)

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
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")))

(defun xz/install-treesit-grammars ()
  "Install tree-sitter grammars if missing."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(use-package treesit
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.js\\'" . js-ts-mode)))

(use-package restart-emacs
  :ensure t)

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

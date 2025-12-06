;;; xz-package.el --- Package configuration -*- lexical-binding:t; -*-
;;; Commentary:
;; initializing the package repositories and some base initial packages

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Essential packages loaded early
(use-package diminish)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3) ;; adjust delay time (in seconds)
  (setq which-key-popup-type 'side-window) ;; or 'minibuffer or 'frame
  (which-key-setup-side-window-bottom))

(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :config
  ;; Performance
  (setq magit-refresh-status-buffer nil
        magit-git-executable "git"))

;; Projectile - Project management
(use-package projectile
  :diminish
  :init
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
  :config
  (projectile-mode +1)
  :custom
  ;; Save known projects on exit
  (add-hook 'kill-emacs-hook #'projectile-save-known-projects))

;; Org mode
(use-package org
  :pin elpa
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :custom
  ;; Org directories
  (org-directory "~/org/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  :config
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))

  (require 'org-tempo)

  (setq org-image-actual-width '(800))
  (setq org-startup-folded 'content
        org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ▾"
        org-log-done 'time
        org-log-into-drawer t
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-fontify-quote-and-verse-blocks t
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-with-inline-images t
        org-cycle-separator-lines 2
        org-export-with-sub-superscripts '{}
        org-html-table-default-attributes '(:border "0" :cellspacing "0" :cellpadding "0"))

  ;; Suppress hooks during htmlize (export)
  (defun my/suppress-hooks-during-export (orig-fun &rest args)
    "Suppress major mode hooks during htmlize to prevent heavy modes from starting."
    (let ((prog-mode-hook nil)
          (c-mode-common-hook nil)
          (c-mode-hook nil)
          (c++-mode-hook nil)
          (flycheck-mode-hook nil)
          (eglot-stay-out-of '(c-mode c++-mode))
          (inhibit-message t))
      (apply orig-fun args)))

  (advice-add 'org-html-fontify-code :around #'my/suppress-hooks-during-export)

  ;; Org agenda
  (setq org-agenda-files (list org-directory)
        org-deadline-warning-days 7)

  ;; TODO keyword faces
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("NEXT" . (:foreground "#da8548" :weight bold))
          ("PROG" . (:foreground "#ECBE7B" :weight bold))
          ("WAIT" . (:foreground "#51afef" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold))))

  ;; Org babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t)))

  ;; Don't ask for confirmation before evaluating
  (setq org-confirm-babel-evaluate nil))

;; Org modern for better aesthetics
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "○" "✸" "✿" "◆" "◇")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-block-fringe nil
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword nil
        org-modern-timestamp t
        org-modern-todo t))

;; Org appear - Show emphasis markers on demand
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil))

;; Org present - Presentations
(use-package org-present
  :after org)

;; Visual fill for better org reading
(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 100
                            visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))

;; Ensure htmlize is installed for org-mode HTML export
(use-package htmlize
  :ensure t)

;; Org to PDF export configuration
(use-package ox-latex
  :ensure nil  ; built-in
  :after org
  :config
  (setq org-html-with-latex 'mathjax)
  (setq org-latex-pdf-process
        '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

  (setq org-latex-compiler "xelatex")

  (setq org-latex-remove-logfiles t)
  (setq org-latex-logfiles-extensions
        '("aux" "bcf" "blg" "fdb_latexmk" "fls" "fig" "idx" "log" "out" "run.xml" "toc" "vrb" "xdv" "snm" "nav"))

  (setq org-latex-packages-alist
        '(("margin=1in" "geometry" nil)
          ("" "fontspec" t)      ; Modern font support (requires XeLaTeX/LuaLaTeX)
          ("" "xunicode" t)      ; Unicode support for XeLaTeX
          ("" "ulem" nil)        ; Underlining package
          ("" "listings" nil)    ; Code listings
          ("" "color" nil)))     ; Color support

  (setq org-latex-src-block-backend 'listings)
  (setq org-latex-default-class "article")
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Nerd icons
(use-package nerd-icons :ensure t)


(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-alhgo --group-directories-first")
  (dired-clean-up-buffers-too t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))))


;; Dired subtree - expand/collapse directories in place
(use-package dired-subtree
  :ensure t
  :after dired)

;; Dired icons
(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Hide details in dired by default, but keep permissions visible
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Customize what dired-hide-details-mode hides
(setq dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil)

;; LaTeX Configuration
;; AUCTeX - Advanced LaTeX editing
(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-command-default "LatexMk"
        TeX-clean-confirm nil
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (setq-default TeX-output-dir "build-el")
  (add-to-list 'TeX-command-list
               '("LatexMk" "latexmk -pdf -%latex -interaction=nonstopmode -output-directory=build-el %f" TeX-run-TeX nil t :help "Run LatexMk"))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (let ((output-dir (expand-file-name "build-el" default-directory)))
                (unless (file-exists-p output-dir)
                  (make-directory output-dir t)))))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; CDLaTeX - Fast math insertion
(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex)))

;; PDF Tools - Better PDF viewer
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; Install without prompting if possible, otherwise user must run M-x pdf-tools-install
  (pdf-tools-install :no-query)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(provide 'xz-package)
;;; xz-package.el ends here

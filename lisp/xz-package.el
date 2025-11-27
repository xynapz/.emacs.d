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

  ;; Link abbreviations removed to allow custom link types to handle logic
  ;; (org-link-abbrev-alist ...) removed

  :config
  ;; Create org directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))

  ;; Hybrid Image Handling: Local Display + CDN Export
  ;; ----------------------------------------------------------------

  ;; 1. Define the base path for local images
  (defvar xz/pub-img-local-path "~/xynapz/pub_img/"
    "Local path to the pub_img repository.")

  ;; 2. Helper function to export to CDN
  (defun xz/org-export-img-link (path desc backend _info base-url)
    "Export function for custom image links."
    (let ((url (concat base-url path)))
      (cond
       ((org-export-derived-backend-p backend 'html)
        (format "<img src=\"%s\" alt=\"%s\" />" url (or desc "")))
       ((org-export-derived-backend-p backend 'latex)
        (format "\\includegraphics{%s}" url))
       (t (format "[[%s][%s]]" url (or desc ""))))))

  ;; 3. Define custom link types
  ;; "img" -> Export: https://cdn.../pub_img/
  (org-link-set-parameters "img"
   :follow (lambda (path) (find-file (expand-file-name path xz/pub-img-local-path)))
   :export (lambda (path desc backend info)
             (xz/org-export-img-link path desc backend info "https://cdn.jsdelivr.net/gh/xynapz/pub_img/")))

  ;; "kb_cpp" -> Export: .../pub_img/cpp/
  (org-link-set-parameters "kb_cpp"
   :follow (lambda (path) (find-file (expand-file-name (concat "cpp/" path) xz/pub-img-local-path)))
   :export (lambda (path desc backend info)
             (xz/org-export-img-link path desc backend info "https://cdn.jsdelivr.net/gh/xynapz/pub_img/cpp/")))

  ;; "kb_writings" -> Export: .../pub_img/writings/
  (org-link-set-parameters "kb_writings"
   :follow (lambda (path) (find-file (expand-file-name (concat "writings/" path) xz/pub-img-local-path)))
   :export (lambda (path desc backend info)
             (xz/org-export-img-link path desc backend info "https://cdn.jsdelivr.net/gh/xynapz/pub_img/writings/")))

  ;; 4. Advice to make inline images work
  ;; Instead of :image-data-fun (which can be flaky), we advise the display function
  ;; to temporarily see these links as file links.
  (defun xz/org-display-inline-images--expand-custom-links (orig-fun &rest args)
    "Temporarily expand custom links to file paths for inline display."
    (let ((org-link-abbrev-alist-local org-link-abbrev-alist))
      ;; Add temporary abbreviations that map our custom types to local file paths
      (add-to-list 'org-link-abbrev-alist-local
                   (cons "img" (expand-file-name xz/pub-img-local-path)))
      (add-to-list 'org-link-abbrev-alist-local
                   (cons "kb_cpp" (expand-file-name "cpp/" xz/pub-img-local-path)))
      (add-to-list 'org-link-abbrev-alist-local
                   (cons "kb_writings" (expand-file-name "writings/" xz/pub-img-local-path)))

      ;; We need to trick org into thinking these are "file" links for a moment
      (cl-letf (((symbol-function 'org-link-expand-abbrev)
                 (lambda (link)
                   (let ((type (car (split-string link ":"))))
                     (cond
                      ((string= type "img")
                       (concat "file:" (expand-file-name (substring link 4) xz/pub-img-local-path)))
                      ((string= type "kb_cpp")
                       (concat "file:" (expand-file-name (concat "cpp/" (substring link 7)) xz/pub-img-local-path)))
                      ((string= type "kb_writings")
                       (concat "file:" (expand-file-name (concat "writings/" (substring link 12)) xz/pub-img-local-path)))
                      (t link))))))
        (apply orig-fun args))))

  (advice-add 'org-display-inline-images :around #'xz/org-display-inline-images--expand-custom-links)

  ;; Image display settings
  (setq org-image-actual-width '(800))

  ;; Automatically redisplay inline images after executing blocks
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; Org settings
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

  ;; Conditional LaTeX style stripping for site-content exports
  ;; Only strip default CSS/Scripts when exporting from site-content directory
  (defun xz/is-site-content-file-p ()
    "Check if the current buffer file is within the site-content directory."
    (when (buffer-file-name)
      (string-prefix-p (expand-file-name "~/xynapz/angeld.me/site-content/")
                       (expand-file-name (buffer-file-name)))))

  (defun xz/org-html-head-filter (_output _backend _info)
    "Set HTML head inclusion based on whether we're exporting site-content."
    (if (xz/is-site-content-file-p)
        (progn
          (setq-local org-html-head-include-default-style nil)
          (setq-local org-html-head-include-scripts nil)
          ;; Suppress MathJax template for site content (handled by Astro)
          (setq-local org-html-mathjax-template ""))
      (progn
        (setq-local org-html-head-include-default-style t)
        (setq-local org-html-head-include-scripts t))))

  ;; Add the filter to org-export-before-processing-hook
  (add-hook 'org-export-before-processing-hook
            (lambda (_backend)
              (when (eq _backend 'html)
                (xz/org-html-head-filter nil nil nil))))

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
  (setq org-confirm-babel-evaluate nil)

  ;; Site Content Export
  (defvar xz/site-content-path "~/xynapz/angeld.me/site-content/"
    "Path to the site content directory.")

  (defvar xz/site-log-path "~/xynapz/angeld.me/logs/"
    "Path to the site export logs.")

  (defvar xz/current-export-log-file nil
    "Current log file for the export process.")

  (defun xz/log-export (level message)
    "Log a message to the current export log file."
    (let ((formatted-msg (format "[%s] [%s] %s\n"
                                 (format-time-string "%Y-%m-%d %H:%M:%S")
                                 level
                                 message)))
      ;; Write to file
      (when xz/current-export-log-file
        (write-region formatted-msg nil xz/current-export-log-file 'append))
      ;; Also print to *Messages*
      (message "[Site Export] %s: %s" level message)))

  (defun xz/export-all-site-content ()
    "Export all .org files in `xz/site-content-path` to HTML with logging, clean build, and directory sync."
    (interactive)
    (save-window-excursion ;; Prevent buffer popping/layout changes
      (unless (file-exists-p xz/site-log-path)
        (make-directory xz/site-log-path t))

      (setq xz/current-export-log-file
            (expand-file-name (format "export-%s.log" (format-time-string "%Y-%m-%d_%H-%M-%S"))
                              xz/site-log-path))

      ;; Collect stats and valid output paths
      (let* ((files (directory-files-recursively (expand-file-name xz/site-content-path) "\\.org$"))
             (total-docs (length files))
             (total-size 0)
             (doc-stats '())
             (valid-html-outputs '())) ;; List to store expected HTML file paths

        ;; Calculate stats and pre-scan for export paths
        (dolist (file files)
          (let ((attrs (file-attributes file)))
            (setq total-size (+ total-size (file-attribute-size attrs)))
            (push (format "%s (%s, %s)"
                          (file-name-nondirectory file)
                          (file-size-human-readable (file-attribute-size attrs))
                          (format-time-string "%Y-%m-%d %H:%M:%S" (file-attribute-modification-time attrs)))
                  doc-stats)
            ;; Extract #+EXPORT_FILE_NAME
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (when (re-search-forward "^#\\+EXPORT_FILE_NAME:[ \t]*\\(.*\\)$" nil t)
                (push (expand-file-name (match-string 1)) valid-html-outputs)))))

        ;; Write Header
        (let ((header (format "================================================================================
 SITE CONTENT EXPORT STATISTICS
 ================================================================================
 Execution Time:   %s
 Target Directory: %s
 Total Documents:  %d
 Total Size:       %s
 
 Document List:
 %s
 ================================================================================
 [LOGS START]
 "
                              (format-time-string "%Y-%m-%d %H:%M:%S")
                              xz/site-content-path
                              total-docs
                              (file-size-human-readable total-size)
                              (mapconcat (lambda (s) (concat "- " s)) (nreverse doc-stats) "\n"))))
          (write-region header nil xz/current-export-log-file))

        (xz/log-export "INFO" "Starting site content export...")

        (let ((success-count 0)
              (fail-count 0)
              ;; Disable hooks to prevent timers from firing on killed buffers
              (org-mode-hook nil)
              (vc-handled-backends nil)
              (find-file-hook nil))

          (dolist (file files)
            (xz/log-export "INFO" (format "Processing %s..." (file-name-nondirectory file)))
            (condition-case err
                (with-current-buffer (find-file-noselect file)
                  ;; Ensure target directory exists if EXPORT_FILE_NAME is set
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward "^#\\+EXPORT_FILE_NAME:[ \t]*\\(.*\\)$" nil t)
                      (let ((target-dir (file-name-directory (match-string 1))))
                        (unless (file-exists-p target-dir)
                          (make-directory target-dir t)
                          (xz/log-export "INFO" (format "Created directory: %s" target-dir))))))
                  
                  (org-html-export-to-html)
                  (kill-buffer) ;; Kill the Org buffer
                  ;; Also kill the generated HTML buffer if it exists (it usually does after export)
                  (let ((html-buffer (find-buffer-visiting (concat (file-name-sans-extension file) ".html"))))
                     (when html-buffer (kill-buffer html-buffer)))
                  
                  (setq success-count (1+ success-count))
                  (xz/log-export "INFO" (format "Successfully exported %s" (file-name-nondirectory file))))
              (error
               (setq fail-count (1+ fail-count))
               (xz/log-export "ERROR" (format "Failed to export %s: %s" (file-name-nondirectory file) err)))))

          ;; Clean Build: Remove dangling HTML files
          (xz/log-export "INFO" "Starting Clean Build (removing orphans)...")
          (let ((orphans-removed 0))
            ;; We assume the root of valid outputs is the common prefix or a known root.
            ;; For safety, we'll only scan directories that contain at least one valid output.
            (let ((scanned-dirs (delete-dups (mapcar 'file-name-directory valid-html-outputs))))
              (dolist (dir scanned-dirs)
                (when (file-exists-p dir)
                  (dolist (f (directory-files dir t "\\.html$"))
                    (unless (member (expand-file-name f) valid-html-outputs)
                      (delete-file f)
                      (setq orphans-removed (1+ orphans-removed))
                      (xz/log-export "CLEAN" (format "Removed orphan: %s" f)))))))
            
            (xz/log-export "INFO" (format "Clean Build complete. Orphans removed: %d" orphans-removed)))

          (xz/log-export "INFO" (format "Export complete. Success: %d, Failed: %d" success-count fail-count))
          (message "Site content export complete! Check logs at %s" xz/current-export-log-file))))))

;; Org modern for better aesthetics
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "○" "✸" "✿" "◆" "◇")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞"))
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
  ;; MathJax settings - always use mathjax but with empty template
  ;; Astro adds its own MathJax script for site-content files
  ;; Note: org-html-mathjax-template is handled conditionally in xz/org-html-head-filter
  (setq org-html-with-latex 'mathjax)
  ;; Use latexmk for PDF export (handles bibtex/biber automatically)
  (setq org-latex-pdf-process
        '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

  ;; Set XeLaTeX as the default compiler
  (setq org-latex-compiler "xelatex")

  ;; Remove temporary files after export
  (setq org-latex-remove-logfiles t)
  (setq org-latex-logfiles-extensions
        '("aux" "bcf" "blg" "fdb_latexmk" "fls" "fig" "idx" "log" "out" "run.xml" "toc" "vrb" "xdv" "snm" "nav"))

  ;; Default LaTeX packages for better PDF output with XeLaTeX
  (setq org-latex-packages-alist
        '(("margin=1in" "geometry" nil)
          ("" "fontspec" t)      ; Modern font support (requires XeLaTeX/LuaLaTeX)
          ("" "xunicode" t)      ; Unicode support for XeLaTeX
          ("" "ulem" nil)        ; Underlining package
          ("" "listings" nil)    ; Code listings
          ("" "color" nil)))     ; Color support

  ;; Enable code highlighting in exported PDFs
  (setq org-latex-listings t)
  ;; Better default document class
  (setq org-latex-default-class "article")
  ;; Customize LaTeX classes if needed
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

;; Basic Dired configuration
(use-package dired
  :ensure nil  ; built-in package
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))  ; Quick jump to dired for current file
  :custom
  ;; Use human-readable sizes, group directories first
  (dired-listing-switches "-alhgo --group-directories-first")

  ;; IMPORTANT: Automatically kill buffers for deleted/moved files
  (dired-clean-up-buffers-too t)

  ;; If you have two dired windows open, use the other as default target
  (dired-dwim-target t)

  ;; Kill old dired buffer when opening new directory
  ;; This prevents accumulation of dired buffers
  (dired-kill-when-opening-new-dired-buffer t)

  :config
  ;; Load dired-x for extra features
  (require 'dired-x)

  ;; Enable 'a' command to reuse buffer (instead of creating new ones)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Better key bindings for navigation
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))))

;; Hide dotfiles by default (toggle with '.')
;; (use-package dired-hide-dotfiles
;;   :ensure t
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :bind (:map dired-mode-map
;;               ("." . dired-hide-dotfiles-mode)))

;; Dired subtree - expand/collapse directories in place
(use-package dired-subtree
  :ensure t
  :after dired
  :config
  ;; Set keybindings for Evil mode
  ;; Use eval-after-load to run after both evil and evil-collection are loaded
  (with-eval-after-load 'evil
    (with-eval-after-load 'evil-collection
      ;; Force our keybindings after evil-collection by using a dired-mode-hook
      (defun my-dired-subtree-evil-setup ()
        "Set up dired-subtree keybindings for evil mode."
        (evil-local-set-key 'normal (kbd "i") 'dired-subtree-insert)
        (evil-local-set-key 'normal (kbd "I") 'dired-subtree-remove))
      (add-hook 'dired-mode-hook 'my-dired-subtree-evil-setup))))

;; Dired icons
(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Hide details in dired by default, but keep permissions visible
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Customize what dired-hide-details-mode hides
(setq dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil)

;; LaTeX Configuration (Overleaf-like setup)
;; ----------------------------------------------------------------

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
        ;; Use LatexMk by default
        TeX-command-default "LatexMk"
        ;; Don't ask to clean
        TeX-clean-confirm nil
        ;; Use PDF Tools for preview
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  ;; Update PDF buffers after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  
  ;; Define LatexMk command if not present
  (add-to-list 'TeX-command-list
               '("LatexMk" "latexmk -pdf -%latex -interaction=nonstopmode %f" TeX-run-TeX nil t :help "Run LatexMk"))

  ;; Auto-clean using latexmk -c (Orthodox way)
  (add-hook 'TeX-after-compilation-finished-functions
            (lambda (file status)
              (when (string-match-p "finished" status)
                (start-process "latex-clean" nil "latexmk" "-c" file)))))

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

;; Citar - Bibliography management (Better for Vertico)
(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/xynapz/angeld.me/site-content/bibliography/references.bib"))
  (citar-library-paths '("~/xynapz/angeld.me/site-content/bibliography/pdfs/"))
  (citar-notes-paths '("~/xynapz/angeld.me/site-content/bibliography/notes.org"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (setq org-cite-global-bibliography '("~/xynapz/angeld.me/site-content/bibliography/references.bib")
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(provide 'xz-package)
;;; xz-package.el ends here

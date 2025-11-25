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
        org-export-with-sub-superscripts '{})

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
  :ensure t
  :config
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-htmlize-font-prefix "org-"))

;; Enhanced HTML Export for Code Blocks (DISABLED for org-html-themes)
;; ----------------------------------------------------------------

;; (defconst xz/org-html-code-style
;;   "<style>
;;   :root {
;;     --code-bg: #fafafa;
;;     --code-border: #e1e4e8;
;;     --code-header-bg: #f6f8fa;
;;     --code-header-text: #24292e;
;;     --code-font: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
;;   }
;;   @media (prefers-color-scheme: dark) {
;;     :root {
;;       --code-bg: #1e1e1e;
;;       --code-border: #30363d;
;;       --code-header-bg: #21262d;
;;       --code-header-text: #c9d1d9;
;;     }
;;   }
;;   
;;   /* Container for the whole block */
;;   .xz-code-container {
;;     margin: 1.5em 0;
;;     border: 1px solid var(--code-border);
;;     border-radius: 6px;
;;     overflow: hidden;
;;     background: var(--code-bg);
;;     box-shadow: 0 1px 3px rgba(0,0,0,0.05);
;;   }

;;   /* Header with filename and copy button */
;;   .xz-code-header {
;;     display: flex;
;;     justify-content: space-between;
;;     align-items: center;
;;     padding: 8px 12px;
;;     background: var(--code-header-bg);
;;     border-bottom: 1px solid var(--code-border);
;;     font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
;;     font-size: 0.85em;
;;     color: var(--code-header-text);
;;   }

;;   .xz-code-filename {
;;     font-weight: 600;
;;     display: flex;
;;     align-items: center;
;;     gap: 6px;
;;   }
;;   
;;   /* Mac-like dots if no filename, or just decoration */
;;   .xz-code-dots {
;;     display: flex;
;;     gap: 4px;
;;     margin-right: 8px;
;;   }
;;   .xz-code-dot {
;;     width: 10px;
;;     height: 10px;
;;     border-radius: 50%;
;;     background-color: #e1e4e8;
;;   }
;;   .xz-code-dot.red { background-color: #ff5f56; }
;;   .xz-code-dot.yellow { background-color: #ffbd2e; }
;;   .xz-code-dot.green { background-color: #27c93f; }

;;   /* Copy button */
;;   .xz-copy-btn {
;;     background: transparent;
;;     border: 1px solid var(--code-border);
;;     border-radius: 4px;
;;     cursor: pointer;
;;     padding: 4px 8px;
;;     font-size: 0.8em;
;;     color: var(--code-header-text);
;;     transition: all 0.2s;
;;     opacity: 0.7;
;;   }
;;   .xz-copy-btn:hover {
;;     opacity: 1;
;;     background: var(--code-border);
;;   }
;;   .xz-copy-btn:active {
;;     transform: translateY(1px);
;;   }

;;   /* The actual code block */
;;   .xz-code-container pre.src {
;;     margin: 0;
;;     padding: 12px;
;;     font-family: var(--code-font);
;;     font-size: 0.9em;
;;     line-height: 1.5;
;;     overflow-x: auto;
;;     background: transparent;
;;     border: none;
;;     box-shadow: none;
;;   }
;;   
;;   /* Remove default Org src container styling if present */
;;   .org-src-container {
;;     border: none;
;;     box-shadow: none;
;;     margin: 0;
;;     padding: 0;
;;   }
;; </style>")

;; (defconst xz/org-html-code-script
;;   "<script>
;;   function copyCode(button) {
;;     const container = button.closest('.xz-code-container');
;;     const pre = container.querySelector('pre');
;;     const code = pre.innerText;
;;     
;;     navigator.clipboard.writeText(code).then(() => {
;;       const originalText = button.innerText;
;;       button.innerText = 'Copied!';
;;       setTimeout(() => {
;;         button.innerText = originalText;
;;       }, 2000);
;;     }).catch(err => {
;;       console.error('Failed to copy:', err);
;;       button.innerText = 'Error';
;;     });
;;   }
;; </script>")

;; (with-eval-after-load 'ox-html
;;   (setq org-html-head-extra
;;         (concat (or org-html-head-extra "")
;;                 xz/org-html-code-style
;;                 xz/org-html-code-script)))

;; (defun xz/org-html-format-src-block-advice (orig-fun src-block contents info)
;;   "Wrap the exported source block in a custom container with filename and copy button."
;;   (let* ((formatted-block (funcall orig-fun src-block contents info))
;;          (params (org-element-property :parameters src-block))
;;          ;; Try to get filename from :filename header arg (parsed from params), then #+NAME
;;          (filename (or (and params 
;;                             (string-match ":filename\\s-+\"\\([^\"]+\\)\"" params)
;;                             (match-string 1 params))
;;                        (org-element-property :name src-block)))
;;          (lang (org-element-property :language src-block)))
;;     
;;     (concat
;;      "<div class=\"xz-code-container\">"
;;      "<div class=\"xz-code-header\">"
;;      "<div class=\"xz-code-filename\">"
;;      ;; Add dots for decoration
;;      "<div class=\"xz-code-dots\">"
;;      "<div class=\"xz-code-dot red\"></div>"
;;      "<div class=\"xz-code-dot yellow\"></div>"
;;      "<div class=\"xz-code-dot green\"></div>"
;;      "</div>"
;;      (if filename (format "<span>%s</span>" filename) (if lang (format "<span>%s</span>" lang) ""))
;;      "</div>"
;;      "<button class=\"xz-copy-btn\" onclick=\"copyCode(this)\">Copy</button>"
;;      "</div>"
;;      formatted-block
;;      "</div>")))

;; (advice-add 'org-html-src-block :around #'xz/org-html-format-src-block-advice)

;; Org HTML Themes
(use-package org-html-themes
  :ensure t
  :config
  ;; You can set a default theme here if you want, or use #+SETUPFILE in your org files
  ;; Example: (setq org-html-themes-theme "readtheorg")
  )

;; Org to PDF export configuration
(use-package ox-latex
  :ensure nil  ; built-in
  :after org
  :config
  ;; Use pdflatex for PDF export
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Remove temporary files after export
  (setq org-latex-remove-logfiles t)
  ;; Default LaTeX packages for better PDF output
  (setq org-latex-packages-alist
        '(("margin=1in" "geometry" nil)
          ("" "fontspec" t)
          ("" "xunicode" t)
          ("" "listings" nil)
          ("" "color" nil)))
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

(provide 'xz-package)
;;; xz-package.el ends here

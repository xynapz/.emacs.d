;;; astro.el --- Major mode for Astro framework -*- lexical-binding: t; -*-
;;; Commentary:
;; Major mode for editing Astro files with LSP support via eglot
;;; Code:

(require 'eglot)

;; Syntax Table
(defvar astro-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    ;; Brackets
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    table)
  "Syntax table for `astro-mode'.")

;; Font Lock (Syntax Highlighting)
(defvar astro-mode-font-lock-keywords
  (list
   ;; Frontmatter delimiter
   '("^---$" . font-lock-keyword-face)

   ;; HTML tags
   '("</?\\([a-zA-Z][a-zA-Z0-9:-]*\\)" 1 font-lock-function-name-face)

   ;; HTML attributes
   '("\\s-+\\([a-zA-Z][a-zA-Z0-9:-]*\\)=" 1 font-lock-variable-name-face)

   ;; Astro components (capitalized tags)
   '("</?\\([A-Z][a-zA-Z0-9]*\\)" 1 font-lock-type-face)

   ;; JavaScript/TypeScript keywords in frontmatter and expressions
   '("\\b\\(const\\|let\\|var\\|function\\|async\\|await\\|import\\|export\\|from\\|default\\|type\\|interface\\|class\\|extends\\|implements\\)\\b"
     . font-lock-keyword-face)

   ;; JSX expressions in braces
   '("{[^}]*}" . font-lock-variable-name-face)

   ;; Props access
   '("\\bAstro\\.props\\b" . font-lock-constant-face)
   '("\\bAstro\\.slots\\b" . font-lock-constant-face)

   ;; String literals
   '("\"[^\"]*\"" . font-lock-string-face)
   '("'[^']*'" . font-lock-string-face)
   '("`[^`]*`" . font-lock-string-face))
  "Keyword highlighting for `astro-mode'.")

;; Indentation
(defun astro-indent-line ()
  "Indent current line in Astro mode."
  (interactive)
  (let ((indent-level 0)
        (cur-indent 0))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-level 0)
        ;; Check if inside frontmatter
        (if (astro--in-frontmatter-p)
            ;; Use basic 2-space indentation in frontmatter
            (progn
              (forward-line -1)
              (setq cur-indent (current-indentation))
              (beginning-of-line)
              (cond
               ((looking-at ".*{\\s-*$") (setq indent-level (+ cur-indent 2)))
               ((looking-at ".*}\\s-*$") (setq indent-level (max 0 (- cur-indent 2))))
               (t (setq indent-level cur-indent))))
          ;; HTML/template indentation
          (let ((not-indented t))
            (while not-indented
              (forward-line -1)
              (if (bobp)
                  (setq not-indented nil)
                (cond
                 ;; Closing tag
                 ((looking-at "^[ \t]*</")
                  (setq indent-level (current-indentation))
                  (setq not-indented nil))
                 ;; Opening tag
                 ((looking-at "^[ \t]*<[^/].*[^/]>\\s-*$")
                  (setq indent-level (+ (current-indentation) 2))
                  (setq not-indented nil))
                 ;; Self-closing tag or content
                 ((looking-at "^[ \t]*<.*/>\\s-*$")
                  (setq indent-level (current-indentation))
                  (setq not-indented nil))
                 (t
                  (if (bobp)
                      (setq not-indented nil))))))))))

    ;; Apply indentation
    (if (< indent-level 0)
        (setq indent-level 0))
    (indent-line-to indent-level)))

(defun astro--in-frontmatter-p ()
  "Check if point is inside frontmatter (between --- markers)."
  (save-excursion
    (let ((pos (point))
          (in-frontmatter nil))
      (goto-char (point-min))
      (when (looking-at "^---$")
        (forward-line 1)
        (when (re-search-forward "^---$" nil t)
          (when (and (> pos (point-at-bol 0))
                     (< pos (point-at-eol)))
            (setq in-frontmatter t))))
      in-frontmatter)))

;; Eglot LSP Configuration
(defun astro-setup-eglot ()
  "Setup eglot for Astro language server."
  (interactive)
  ;; Add Astro language server to eglot
  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "/usr/lib/node_modules/typescript/lib")))))

  ;; ;; Configure TypeScript path from project base node_modules
  ;; (let* ((project-root (or (and (fboundp 'projectile-project-root)
  ;;                               (projectile-project-root))
  ;;                          (project-root (project-current t))))
  ;;        (tsdk-path (expand-file-name "/usr/lib/node_modules/typescript/lib" project-root)))
  ;;   (when (file-exists-p tsdk-path)
  ;;     (setq-local eglot-workspace-configuration
  ;;                 `(:astro (:typescript (:tsdk ,tsdk-path))))))

  ;; Start eglot if not already running
  (unless (eglot-managed-p)
    (eglot-ensure)))

;; Mode Definition
(defvar astro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'eglot-format-buffer)
    (define-key map (kbd "C-c r") 'eglot-rename)
    (define-key map (kbd "C-c a") 'eglot-code-actions)
    (define-key map (kbd "C-c d") 'eglot-find-declaration)
    (define-key map (kbd "C-c i") 'eglot-find-implementation)
    map)
  "Keymap for `astro-mode'.")

;;;###autoload
(define-derived-mode astro-mode prog-mode "Astro"
  "Major mode for editing Astro framework files.

This mode provides Astro's syntax highlighting, indentation, and LSP
support via eglot.

Key bindings:
\\{astro-mode-map}"
  :syntax-table astro-mode-syntax-table

  ;; Set up font-lock
  (setq font-lock-defaults '(astro-mode-font-lock-keywords))

  ;; Set up indentation
  (setq-local indent-line-function 'astro-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Multi-line comments
  (setq-local comment-multi-line t)

  ;; Setup eglot
  (add-hook 'astro-mode-hook #'astro-setup-eglot))

;; Auto-mode-alist
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode))

;; Additional Utilities
(defun astro-new-component (name)
  "Create a new Astro component with NAME."
  (interactive "sComponent name: ")
  (let ((filename (format "%s.astro" name))
        (component-name (capitalize name)))
    (find-file filename)
    (insert (format "---
// %s Component
interface Props {
  // Add your props here
}

const { } = Astro.props;
---

<div class=\"%s\">
  <h1>%s</h1>
  <slot />
</div>

<style>
  .%s {
    /* Add your styles here */
  }
</style>

<script>
  // Add your client-side JavaScript here
</script>
" component-name (downcase name) component-name (downcase name)))
    (goto-char (point-min))
    (forward-line 2)))

(defun astro-insert-component ()
  "Insert an Astro component import and usage template."
  (interactive)
  (let ((name (read-string "Component name: ")))
    (insert (format "import %s from './%s.astro';\n" name name))))

;; Pretty Symbols (Optional)
(defun astro-setup-prettify-symbols ()
  "Setup prettify symbols for Astro mode."
  (setq prettify-symbols-alist
        '(("=>" . "⇒")
          ("->" . "→")
          ("function" . "ƒ")
          ("const" . "∷")))
  (prettify-symbols-mode 1))

;; Optionally enable prettify symbols
;; (add-hook 'astro-mode-hook #'astro-setup-prettify-symbols)

(provide 'astro)
;;; astro.el ends here

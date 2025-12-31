;;; xz-export.el --- Angeld Site Export Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Site export configuration for angeld.me
;; Handles XZHH_ meta tag injection and batch export functions.
;; Works both interactively (C-c C-e) and in batch mode (make export).
;;
;; Usage (batch):
;;   emacs --batch -l xz-export.el -f xzhh/batch-export SOURCE OUTPUT [LOG]
;;   emacs --batch -l xz-export.el -f xzhh/test-export

;;; Code:

;; ============================================================================
;; DEPENDENCIES (for batch mode)
;; ============================================================================

;; When running in batch mode, we need to bootstrap packages
(when noninteractive
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))
  (setq package-user-dir (expand-file-name "elpa" 
                           (file-name-directory (or load-file-name 
                                                    buffer-file-name
                                                    user-emacs-directory))))
  (package-initialize)
  
  ;; Install htmlize if not present
  (unless (package-installed-p 'htmlize)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'htmlize))
  
  (require 'htmlize))

(require 'org)
(require 'ox-html)

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar xzhh/log-file nil
  "Path to export log file. Set by caller or defaults to ./export.log")

(defvar xzhh/log-to-stdout t
  "If non-nil, also print log messages to stdout (for batch mode).")

;; HTML Export settings (applied always for consistency)
(setq org-export-with-sub-superscripts '{}
      org-html-prefer-user-labels t
      org-html-table-default-attributes '(:border "0" :cellspacing "0" :cellpadding "0")
      org-html-with-latex 'mathjax
      org-html-postamble nil
      org-html-preamble nil)

;; ============================================================================
;; XZHH META TAG INJECTION
;; ============================================================================

(defun xzhh/inject-meta-tags (backend)
  "Convert XZHH_ keywords to HTML meta tags during export.
#+XZHH_ORDER: 5 becomes <meta name=\"order\" content=\"5\" />"
  (when (org-export-derived-backend-p backend 'html)
    (let ((meta-tags '()))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+XZHH_\\([A-Za-z0-9_]+\\):[[:space:]]*\\(.+\\)$" nil t)
          (let* ((key (downcase (match-string 1)))
                 (value (string-trim (match-string 2))))
            (push (format "#+HTML_HEAD: <meta name=\"%s\" content=\"%s\" />" key value) meta-tags))))
      (when meta-tags
        (save-excursion
          (goto-char (point-min))
          (let ((insert-pos nil))
            (while (re-search-forward "^#\\+HTML_HEAD:" nil t)
              (setq insert-pos (line-end-position)))
            (unless insert-pos
              (goto-char (point-min))
              (while (and (re-search-forward "^#\\+[A-Za-z_]+:" nil t)
                          (not (looking-at-p ".*BEGIN_")))
                (setq insert-pos (line-end-position))))
            (when insert-pos
              (goto-char insert-pos)
              (insert "\n" (string-join (nreverse meta-tags) "\n")))))))))

;; Register the hook (runs for both interactive and batch exports)
(add-hook 'org-export-before-processing-hook #'xzhh/inject-meta-tags)

;; ============================================================================
;; LOGGING
;; ============================================================================

(defun xzhh/log (level msg &rest args)
  "Log MSG with LEVEL. ARGS are format arguments."
  (let* ((formatted-msg (apply #'format msg args))
         (timestamp (format-time-string "%H:%M:%S"))
         (prefix (pcase level
                   ('info  "INFO ")
                   ('warn  "WARN ")
                   ('error "ERROR")
                   ('ok    " OK  ")
                   (_      "     ")))
         (line (format "[%s] %s  %s\n" timestamp prefix formatted-msg)))
    ;; Print to stdout in batch mode
    (when (and xzhh/log-to-stdout noninteractive)
      (princ line))
    ;; Write to log file if set
    (when xzhh/log-file
      (make-directory (file-name-directory xzhh/log-file) t)
      (write-region line nil xzhh/log-file 'append 'silent))))

(defun xzhh/log-header ()
  "Log export run header."
  (let ((header (concat
                 "\n"
                 "╔══════════════════════════════════════════════════════════════╗\n"
                 (format "║  ANGELD SITE EXPORT - %s                    ║\n" (format-time-string "%Y-%m-%d %H:%M:%S"))
                 "╚══════════════════════════════════════════════════════════════╝\n")))
    (when (and xzhh/log-to-stdout noninteractive)
      (princ header))
    (when xzhh/log-file
      (write-region header nil xzhh/log-file 'append 'silent))))

(defun xzhh/log-footer (total errors elapsed)
  "Log summary footer."
  (let ((footer (concat
                 "\n"
                 "┌──────────────────────────────────────────────────────────────┐\n"
                 "│  EXPORT SUMMARY                                              │\n"
                 "├──────────────────────────────────────────────────────────────┤\n"
                 (format "│  Total Files:  %-5d                                         │\n" total)
                 (format "│  Errors:       %-5d                                         │\n" errors)
                 (format "│  Elapsed:      %.2fs                                         │\n" elapsed)
                 "└──────────────────────────────────────────────────────────────┘\n")))
    (when (and xzhh/log-to-stdout noninteractive)
      (princ footer))
    (when xzhh/log-file
      (write-region footer nil xzhh/log-file 'append 'silent))))

;; ============================================================================
;; EXPORT FUNCTIONS
;; ============================================================================

(defun xzhh/export-file (org-file output-file)
  "Export ORG-FILE to OUTPUT-FILE as HTML."
  (with-current-buffer (find-file-noselect org-file)
    (org-export-to-file 'html output-file)
    (kill-buffer)))

(defun xzhh/export-directory (source-dir output-dir &optional log-file)
  "Export all .org files from SOURCE-DIR to OUTPUT-DIR.
Optionally write logs to LOG-FILE."
  (interactive "DSource directory: \nDOutput directory: ")
  (when log-file
    (setq xzhh/log-file log-file))
  (let ((files (directory-files-recursively source-dir "\\.org$"))
        (count 0)
        (errors 0)
        (start-time (float-time)))

    (xzhh/log-header)
    (xzhh/log 'info "Source: %s" source-dir)
    (xzhh/log 'info "Output: %s" output-dir)
    (xzhh/log 'info "Files found: %d" (length files))

    (dolist (file files)
      (condition-case err
          (let* ((rel-path (file-relative-name file source-dir))
                 (rel-dir (file-name-directory rel-path))
                 (base-name (file-name-sans-extension (file-name-nondirectory file)))
                 (target-dir (if rel-dir
                                 (expand-file-name rel-dir output-dir)
                               output-dir))
                 (output-file (expand-file-name (concat base-name ".html") target-dir)))
            (unless (file-exists-p target-dir)
              (make-directory target-dir t))
            (xzhh/export-file file output-file)
            (xzhh/log 'ok "%s" rel-path)
            (setq count (1+ count)))
        (error
         (setq errors (1+ errors))
         (xzhh/log 'error "%s: %s" (file-relative-name file source-dir) (error-message-string err)))))

    (let ((elapsed (- (float-time) start-time)))
      (xzhh/log-footer count errors elapsed)
      (format "Exported %d files (%.1fs) | Errors: %d" count elapsed errors))))

;; ============================================================================
;; BATCH COMMANDS
;; ============================================================================

(defun xzhh/batch-export ()
  "Batch export for command-line.
Usage: emacs --batch -l xz-export.el -f xzhh/batch-export SOURCE OUTPUT [LOG]"
  (let ((source-dir (pop command-line-args-left))
        (output-dir (pop command-line-args-left))
        (log-file (pop command-line-args-left)))
    (unless (and source-dir output-dir)
      (error "Usage: emacs --batch -l xz-export.el -f xzhh/batch-export SOURCE OUTPUT [LOG]"))
    (xzhh/export-directory source-dir output-dir log-file)))

(defun xzhh/test-export ()
  "Test the export configuration with a sample file.
Creates a test org file, exports it, and verifies the output."
  (let* ((test-dir (make-temp-file "xzhh-test-" t))
         (test-org (expand-file-name "test.org" test-dir))
         (test-html (expand-file-name "test.html" test-dir)))

    (princ "\n═══════════════════════════════════════════\n")
    (princ "  XZHH EXPORT CONFIG TEST\n")
    (princ "═══════════════════════════════════════════\n\n")

    ;; Create test file
    (with-temp-file test-org
      (insert "#+TITLE: Test Export\n")
      (insert "#+XZHH_ORDER: 1\n")
      (insert "#+XZHH_PUB_DATE: 2025-01-01\n")
      (insert "#+XZHH_DESCRIPTION: Test description\n")
      (insert "\n* Hello World\n")
      (insert "This is a test.\n\n")
      (insert "#+BEGIN_SRC python\n")
      (insert "print('hello')\n")
      (insert "#+END_SRC\n"))

    (princ (format "✓ Created test file: %s\n" test-org))

    ;; Export
    (xzhh/export-file test-org test-html)
    (princ (format "✓ Exported to: %s\n" test-html))

    ;; Verify
    (let ((html-content (with-temp-buffer
                          (insert-file-contents test-html)
                          (buffer-string))))
      (princ "\n─── META TAG VERIFICATION ───\n")
      (if (string-match "name=\"order\" content=\"1\"" html-content)
          (princ "✓ order meta tag: FOUND\n")
        (princ "✗ order meta tag: MISSING\n"))
      (if (string-match "name=\"pub_date\" content=\"2025-01-01\"" html-content)
          (princ "✓ pub_date meta tag: FOUND\n")
        (princ "✗ pub_date meta tag: MISSING\n"))
      (if (string-match "name=\"description\"" html-content)
          (princ "✓ description meta tag: FOUND\n")
        (princ "✗ description meta tag: MISSING\n"))
      (if (string-match "<title>Test Export</title>" html-content)
          (princ "✓ title tag: FOUND\n")
        (princ "✗ title tag: MISSING\n")))

    ;; Cleanup
    (delete-directory test-dir t)
    (princ "\n✓ Test complete. Temp files cleaned.\n")
    (princ "═══════════════════════════════════════════\n\n")))

(provide 'xz-export)
;;; xz-export.el ends here

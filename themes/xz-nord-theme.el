;;; xz-nord-theme.el --- A darker Nord theme -*- lexical-binding: t; no-byte-compile: t; -*-

;; Based on Nord Theme by Sven Greb (MIT License)
;; Modified by Angel Dhakal - darker background variant

;;; Code:

(unless (>= emacs-major-version 24)
  (error "xz-nord theme requires Emacs 24 or later!"))

(deftheme xz-nord "A darker variant of the Nord theme")

;;;; Color Constants
(let ((class '((class color) (min-colors 89)))
  ;; Polar Night - DARKENED
  (nord0 "#1b1d1e")  ; was #2E3440 - main bg (from doom-Iosvkem)
  (nord1 "#262829")  ; was #3B4252 - hl-line
  (nord2 "#434C5E")  ; was #303030 - restore original for better visibility
  (nord3 "#4C566A")  ; unchanged - comments base
  ;; Snow Storm - unchanged
  (nord4 "#D8DEE9")
  (nord5 "#E5E9F0")
  (nord6 "#ECEFF4")
  ;; Frost - unchanged
  (nord7 "#8FBCBB")
  (nord8 "#88C0D0")
  (nord9 "#81A1C1")
  (nord10 "#5E81AC")
  ;; Aurora - unchanged
  (nord11 "#BF616A")
  (nord12 "#D08770")
  (nord13 "#EBCB8B")
  (nord14 "#A3BE8C")
  (nord15 "#B48EAD")
  ;; Semantic colors
  (nord-comment "#616e88"))

;;;; +------------+
;;;; + Core Faces +
;;;; +------------+
  (custom-theme-set-faces
    'xz-nord
    ;; +--- Base ---+
    `(bold ((,class (:weight bold))))
    `(bold-italic ((,class (:weight bold :slant italic))))
    `(default ((,class (:foreground ,nord4 :background ,nord0))))
    `(error ((,class (:foreground ,nord11 :weight bold))))
    `(escape-glyph ((,class (:foreground ,nord12))))
    `(font-lock-builtin-face ((,class (:foreground ,nord9))))
    `(font-lock-comment-face ((,class (:foreground ,nord-comment :slant italic))))
    `(font-lock-comment-delimiter-face ((,class (:foreground ,nord-comment))))
    `(font-lock-constant-face ((,class (:foreground ,nord9))))
    `(font-lock-doc-face ((,class (:foreground ,nord-comment))))
    `(font-lock-function-name-face ((,class (:foreground ,nord8 :weight bold))))
    `(font-lock-keyword-face ((,class (:foreground ,nord9))))
    `(font-lock-negation-char-face ((,class (:foreground ,nord9))))
    `(font-lock-preprocessor-face ((,class (:foreground ,nord10 :weight bold))))
    `(font-lock-reference-face ((,class (:foreground ,nord9))))
    `(font-lock-regexp-grouping-backslash ((,class (:foreground ,nord13))))
    `(font-lock-regexp-grouping-construct ((,class (:foreground ,nord13))))
    `(font-lock-string-face ((,class (:foreground ,nord14))))
    `(font-lock-type-face ((,class (:foreground ,nord7))))
    `(font-lock-variable-name-face ((,class (:foreground ,nord4))))
    `(font-lock-warning-face ((,class (:foreground ,nord13))))
    `(italic ((,class (:slant italic))))
    `(shadow ((,class (:foreground ,nord3))))
    `(underline ((,class (:underline t))))
    `(warning ((,class (:foreground ,nord13 :weight bold))))

    ;; +--- UI ---+
    `(border ((,class (:foreground ,nord4))))
    `(cursor ((,class (:background ,nord4))))
    `(fringe ((,class (:foreground ,nord4 :background ,nord0))))
    `(header-line ((,class (:foreground ,nord4 :background ,nord2))))
    `(highlight ((,class (:foreground ,nord8 :background ,nord2))))
    `(hl-line ((,class (:background ,nord1))))
    `(isearch ((,class (:foreground ,nord0 :background ,nord8))))
    `(isearch-fail ((,class (:foreground ,nord11))))
    `(lazy-highlight ((,class (:foreground ,nord0 :background ,nord7))))
    `(link ((,class (:underline t))))
    `(link-visited ((,class (:underline t))))
    `(linum ((,class (:foreground ,nord3 :background ,nord0))))
    `(line-number ((,class (:foreground ,nord3 :background ,nord0))))
    `(line-number-current-line ((,class (:foreground ,nord8 :background ,nord0))))
    `(match ((,class (:inherit isearch))))
    `(minibuffer-prompt ((,class (:foreground ,nord8 :weight bold))))
    `(mode-line ((,class (:foreground ,nord8 :background ,nord2))))
    `(mode-line-buffer-id ((,class (:weight bold))))
    `(mode-line-highlight ((,class (:inherit highlight))))
    `(mode-line-inactive ((,class (:foreground ,nord4 :background ,nord1))))
    `(region ((,class (:background ,nord2))))
    `(secondary-selection ((,class (:background ,nord2))))
    `(show-paren-match ((,class (:foreground ,nord0 :background ,nord8))))
    `(show-paren-mismatch ((,class (:background ,nord11))))
    `(success ((,class (:foreground ,nord14))))
    `(vertical-border ((,class (:foreground ,nord2))))
    `(whitespace-trailing ((,class (:foreground ,nord11 :background ,nord1))))

    ;; +--- Org Mode ---+
    `(org-level-1 ((,class (:foreground ,nord7 :weight extra-bold))))
    `(org-level-2 ((,class (:foreground ,nord8 :weight bold))))
    `(org-level-3 ((,class (:foreground ,nord9 :weight semi-bold))))
    `(org-level-4 ((,class (:foreground ,nord10 :weight normal))))
    `(org-level-5 ((,class (:inherit org-level-4))))
    `(org-level-6 ((,class (:inherit org-level-4))))
    `(org-level-7 ((,class (:inherit org-level-4))))
    `(org-level-8 ((,class (:inherit org-level-4))))
    `(org-block ((,class (:foreground ,nord4 :background ,nord1))))
    `(org-block-begin-line ((,class (:foreground ,nord3))))
    `(org-block-end-line ((,class (:foreground ,nord3))))
    `(org-code ((,class (:foreground ,nord7))))
    `(org-date ((,class (:foreground ,nord8))))
    `(org-document-title ((,class (:foreground ,nord8 :weight bold))))
    `(org-document-info ((,class (:foreground ,nord4))))
    `(org-done ((,class (:foreground ,nord14 :weight bold))))
    `(org-todo ((,class (:foreground ,nord13 :weight bold))))
    `(org-hide ((,class (:foreground ,nord0))))
    `(org-link ((,class (:foreground ,nord8 :underline t))))
    `(org-table ((,class (:foreground ,nord9))))
    `(org-verbatim ((,class (:foreground ,nord7))))
    `(org-quote ((,class (:inherit org-block :slant italic))))

    ;; +--- Diffs ---+
    `(diff-added ((,class (:foreground ,nord14))))
    `(diff-changed ((,class (:foreground ,nord13))))
    `(diff-removed ((,class (:foreground ,nord11))))
    `(diff-header ((,class (:foreground ,nord9 :weight bold))))

    ;; +--- Magit ---+
    `(magit-section-heading ((,class (:foreground ,nord7 :weight bold))))
    `(magit-section-highlight ((,class (:background ,nord2))))
    `(magit-branch-local ((,class (:foreground ,nord7 :weight bold))))
    `(magit-branch-remote ((,class (:foreground ,nord14 :weight bold))))
    `(magit-hash ((,class (:foreground ,nord8))))
    `(magit-diff-context-highlight ((,class (:background ,nord1))))
    `(magit-diffstat-added ((,class (:foreground ,nord14))))
    `(magit-diffstat-removed ((,class (:foreground ,nord11))))

    ;; +--- Company ---+
    `(company-tooltip ((,class (:foreground ,nord4 :background ,nord2))))
    `(company-tooltip-selection ((,class (:background ,nord3 :weight bold))))
    `(company-tooltip-common ((,class (:foreground ,nord8))))
    `(company-scrollbar-bg ((,class (:background ,nord1))))
    `(company-scrollbar-fg ((,class (:background ,nord3))))

    ;; +--- Flycheck ---+
    `(flycheck-error ((,class (:underline (:style wave :color ,nord11)))))
    `(flycheck-warning ((,class (:underline (:style wave :color ,nord13)))))
    `(flycheck-info ((,class (:underline (:style wave :color ,nord8)))))

    ;; +--- Git Gutter ---+
    `(git-gutter:modified ((,class (:foreground ,nord13))))
    `(git-gutter:added ((,class (:foreground ,nord14))))
    `(git-gutter:deleted ((,class (:foreground ,nord11))))

    ;; +--- Markdown ---+
    `(markdown-header-face-1 ((,class (:foreground ,nord8 :weight bold))))
    `(markdown-header-face-2 ((,class (:foreground ,nord8 :weight bold))))
    `(markdown-header-face-3 ((,class (:foreground ,nord8 :weight bold))))
    `(markdown-inline-code-face ((,class (:foreground ,nord7))))
    `(markdown-link-face ((,class (:foreground ,nord8))))
    `(markdown-url-face ((,class (:foreground ,nord4 :underline t))))

    ;; +--- Rainbow Delimiters ---+
    `(rainbow-delimiters-depth-1-face ((,class :foreground ,nord7)))
    `(rainbow-delimiters-depth-2-face ((,class :foreground ,nord8)))
    `(rainbow-delimiters-depth-3-face ((,class :foreground ,nord9)))
    `(rainbow-delimiters-depth-4-face ((,class :foreground ,nord10)))
    `(rainbow-delimiters-depth-5-face ((,class :foreground ,nord12)))
    `(rainbow-delimiters-depth-6-face ((,class :foreground ,nord13)))
    `(rainbow-delimiters-depth-7-face ((,class :foreground ,nord14)))
    `(rainbow-delimiters-depth-8-face ((,class :foreground ,nord15)))

    ;; +--- Which Key ---+
    `(which-key-key-face ((,class (:foreground ,nord8))))
    `(which-key-separator-face ((,class (:foreground ,nord3))))
    `(which-key-command-description-face ((,class (:foreground ,nord4))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'xz-nord)

;;; xz-nord-theme.el ends here

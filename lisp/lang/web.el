;;; web.el --- HTML/CSS/JS support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal web development config (no frameworks)

;;; Code:

;; Web Mode (HTML + Jinja2 + mixed content)
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jinja2?\\'" . web-mode)
         ("\\.j2\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        ;; Auto-detect jinja
        web-mode-engines-alist '(("jinja" . "\\.html?\\'"))))

;; Tree-sitter for pure CSS/JS
(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))


(provide 'web)
;;; web.el ends here

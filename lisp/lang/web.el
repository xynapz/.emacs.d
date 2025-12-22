;;; web.el --- HTML/CSS/JS support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal web development config (no frameworks)

;;; Code:

;; HTML - use built-in mhtml-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . mhtml-mode))

;; CSS - use built-in css-mode
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; JavaScript - use js-mode (built-in, or js-ts-mode if grammar available)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

;; JS settings
(setq js-indent-level 2)

;; Jinja2 templates
(use-package jinja2-mode
  :ensure t
  :mode (("\\.jinja2?\\'" . jinja2-mode)
         ("\\.j2\\'" . jinja2-mode)))

;; Eglot for JS (typescript-language-server handles JS too)
(add-hook 'js-mode-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (executable-find "typescript-language-server"))
              (eglot-ensure))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(js-mode . ("typescript-language-server" "--stdio"))))

(provide 'web)
;;; web.el ends here

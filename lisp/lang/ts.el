;;; ts.el --- TypeScript and JavaScript support -*- lexical-binding: t; -*-

(require 'eglot)
(require 'treesit)

;; TypeScript (Tree-sitter)
(use-package typescript-ts-mode
  :ensure nil ; Built-in in Emacs 29+
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
         (tsx-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))))

;; JavaScript (Tree-sitter)
(use-package js-ts-mode
  :ensure nil
  :mode "\\.js\\'"
  :hook ((js-ts-mode . eglot-ensure)
         (js-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))))

;; Eglot Configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

(provide 'ts)
;;; ts.el ends here

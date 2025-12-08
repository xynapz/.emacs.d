;;; ts.el --- TypeScript and JavaScript support -*- lexical-binding: t; -*-

(require 'eglot)

;; TypeScript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook ((typescript-mode . eglot-ensure)
         ;; Format on save
         (typescript-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))))

;; JavaScript
(use-package js
  :ensure nil
  :hook ((js-mode . eglot-ensure)
         ;; Format on save
         (js-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))))

;; Eglot Configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

(provide 'ts)
;;; ts.el ends here

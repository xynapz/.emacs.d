;;; ts.el --- TypeScript and JavaScript support -*- lexical-binding: t; -*-

(require 'eglot)
(require 'treesit)

;; Define grammar sources
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")))

;; Function to ensure grammars are installed (optional helper)
(defun ts/install-grammars ()
  "Install tree-sitter grammars if missing."
  (interactive)
  (dolist (lang '(typescript tsx javascript))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

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

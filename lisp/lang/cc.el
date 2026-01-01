;;; cc.el --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal C/C++ config with clangd LSP

;;; Code:

;; Treat .h as C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Use Tree-sitter if available
(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

;; Style
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; Format on save
(add-hook 'c-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
(add-hook 'c++-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; Compile shortcuts
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)

(provide 'cc)
;;; cc.el ends here

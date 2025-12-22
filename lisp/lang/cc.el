;;; cc.el --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal C/C++ config with clangd LSP

;;; Code:

;; Treat .h as C (change to c++-mode if you prefer C++)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;; Style
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; Eglot LSP
(defun xz/eglot-ensure-c ()
  "Start eglot for C/C++ unless in org export."
  (when (and (buffer-file-name)
             (not (bound-and-true-p org-export-current-backend)))
    (eglot-ensure)))

(add-hook 'c-mode-hook #'xz/eglot-ensure-c)
(add-hook 'c++-mode-hook #'xz/eglot-ensure-c)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd" "--clang-tidy"))))

;; Format on save with clang-format
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style "file"
        clang-format-fallback-style "llvm"))

(defun xz/format-c-buffer ()
  "Format C/C++ buffer on save."
  (when (and (derived-mode-p 'c-mode 'c++-mode)
             (executable-find "clang-format"))
    (clang-format-buffer)))

(add-hook 'c-mode-hook
          (lambda () (add-hook 'before-save-hook #'xz/format-c-buffer nil t)))
(add-hook 'c++-mode-hook
          (lambda () (add-hook 'before-save-hook #'xz/format-c-buffer nil t)))

;; Compile shortcuts
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)

(provide 'cc)
;;; cc.el ends here

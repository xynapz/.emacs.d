;;; init.el --- entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Emacs config

;;; Code:

;; Add Lisp directories
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))

;; Load modules
(load "xz-core")
(load "xz-package")
(load "xz-editor")
(load "xz-completion")
(load "xz-evil")
(load "xz-org")
(load "xz-export")

;; Languages
(load "cc")
(load "web")
(load "py")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("646f492c314c2621ac2dd98a81679aa73a0a38a4e16cc16d9f048594705ba27c"
     "5a4cdc4365122d1a17a7ad93b6e3370ffe95db87ed17a38a94713f6ffe0d8ceb" default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

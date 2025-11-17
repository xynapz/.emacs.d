;;; init.el --- entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for my Emacs config

;;; Code:

;; Add Lisp directories
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))

;; Load modules in order
(load "xz-core")
(load "xz-package")
(load "xz-editor")
(load "xz-completion")
(load "xz-keybindings")
;; (load "xz-server")

;; Languages
(load "cc")
(load "astro")

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(load "xz-evil")
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
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "21d2bf8d4d1df4859ff94422b5e41f6f2eeff14dd12f01428fa3cb4cb50ea0fb"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
     "4b88b7ca61eb48bb22e2a4b589be66ba31ba805860db9ed51b4c484f3ef612a7"
     "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
     "d12b1d9b0498280f60e5ec92e5ecec4b5db5370d05e787bc7cc49eae6fb07bc0"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4" default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

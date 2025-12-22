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


;; Languages
(load "cc")
(load "astro")
(load "ts")
(load "py")

;;; init.el ends here

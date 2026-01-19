;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Restore GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; Native compilation settings (silence warnings)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-deferred-compilation t))

;; LSP performance (must be set early)
(setq read-process-output-max (* 1024 1024))  ; 1MB for LSP

;; Disable package.el in early init (we handle it in init.el/xz-pkg)
(setq package-enable-at-startup nil)

;; Disable GUI elements early to avoid flickering
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here

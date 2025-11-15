;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; Early init file.

;;; Code:
;; Improve startup time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun restore-gc-cons-threshold ()
  "Restore GC threshold after startup."
  (setq gc-cons-threshold 64000000
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'restore-gc-cons-threshold 105)

;; Disable package.el GUI
(setq package-enable-at-startup nil)

;; Native comp settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-jit-compilation t))

;;; early-init.el ends here

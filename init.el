;;; init.el --- Emacs Configuration Entry Point -*- lexical-binding: t; -*-

;;; Commentary:
;; Modular Emacs configuration refactored for stability and performance.

;;; Code:

;; Add lisp directory to load-path
(defvar xz/config-root
  (file-name-directory (or load-file-name buffer-file-name user-init-file))
  "The root of the configuration.")

(add-to-list 'load-path (expand-file-name "lisp" xz/config-root))

;; 1. Package Management
(require 'xz-pkg)

;; 2. Core Settings (Editor defaults, Evil, Desktop session, etc.)
(require 'xz-core)
(require 'xz-editor)

;; 3. UI & Theme
(require 'xz-ui)

(require 'xz-evil)

;; 4. Completion (Vertico/Corfu)
(require 'xz-completion)

;; 5. Development (LSP, Treesit, Languages)
(require 'xz-dev)

;; 6. Org Mode & Export (Preserved modules)
(require 'xz-org)
(require 'xz-export)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e175343df8461d701e40ea88d5a6e39df8174b770dc40e05ba15e1485798374" default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

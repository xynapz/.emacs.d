;;; xz-pkg.el --- Package management -*- lexical-binding: t; -*-

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t
      use-package-verbose nil
      use-package-verbose nil
      use-package-expand-minimally t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; Automatic package updates
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t))

;; Restart Emacs
(use-package restart-emacs
  :ensure t
  :bind ("C-c r r" . restart-emacs))

(provide 'xz-pkg)
;;; xz-pkg.el ends here

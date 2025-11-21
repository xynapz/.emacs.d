;;; xz-config.el --- Manage extra configs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'comint)
(require 'ansi-color)

;; instead of using #+OPTIONS: ^:{} to get sub/sup script on each file this is a
;; perm fix
(setq org-export-with-sub-superscripts '{})

(provide 'xz-server)
;;; xz-config.el ends here

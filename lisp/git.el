;;; git.el --- Magit Git configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; magit, gittimemachine

;;; Code:
;; Magit - Git porcelain for Emacs
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :config
  ;; Performance
  (setq magit-refresh-status-buffer nil
        magit-git-executable "git"))

(provide 'git)
;;; git.el ends here

;;; leader.el --- Leader key bindings (SPC) -*- lexical-binding: t; -*-
;;; Commentary:
;; keybindings with SPC as leader

;;; Code:
(require 'general)

;; Helper functions
(defun my/open-config ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my/reload-config ()
  "Reload Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Config reloaded!"))

(defun my/split-window-right-and-focus ()
  "Split window right and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my/split-window-below-and-focus ()
  "Split window below and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun my/delete-other-windows-or-treemacs ()
  "Delete other windows or close treemacs if it's the only other window."
  (interactive)
  (if (and (fboundp 'treemacs-current-visibility)
           (eq (treemacs-current-visibility) 'visible))
      (if (= (count-windows) 2)
          (treemacs)
        (delete-other-windows))
    (delete-other-windows)))

;; Main leader key bindings
(my/leader-keys
  ;; Top level single keys
  "SPC" '(execute-extended-command :which-key "M-x")
  ":" '(eval-expression :which-key "eval")
  "." '(find-file :which-key "find file")
  "," '(switch-to-buffer :which-key "switch buffer")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")

  ;; Applications
  "a" '(:ignore t :which-key "applications")
  "at" '(treemacs :which-key "treemacs")
  "ad" '(dired :which-key "dired")
  "ap" '(proced :which-key "process list")

  ;; Buffers
  "b" '(:ignore t :which-key "buffers")
  "bb" '(switch-to-buffer :which-key "switch buffer")
  "bd" '(kill-current-buffer :which-key "kill buffer")
  "bD" '(kill-buffer :which-key "kill buffer (select)")
  "bn" '(next-buffer :which-key "next buffer")
  "bp" '(previous-buffer :which-key "previous buffer")
  "br" '(revert-buffer :which-key "revert buffer")
  "bs" '(save-buffer :which-key "save buffer")
  "bS" '(evil-write-all :which-key "save all buffers")
  "bx" '(kill-buffer-and-window :which-key "kill buffer & window")
  "bR" '(rename-buffer :which-key "rename buffer")

  ;; Code
  "c" '(:ignore t :which-key "code")
  "cc" '(compile :which-key "compile")
  "cC" '(recompile :which-key "recompile")
  "ck" '(kill-compilation :which-key "kill compilation")
  "cd" '(flymake-show-buffer-diagnostics :which-key "buffer diagnostics")
  "cD" '(flymake-show-project-diagnostics :which-key "project diagnostics")
  "cn" '(flymake-goto-next-error :which-key "next error")
  "cp" '(flymake-goto-prev-error :which-key "previous error")
  "cx" '(flycheck-list-errors :which-key "list errors (flycheck)")
  "cX" '(flycheck-verify-setup :which-key "verify flycheck")
  "ca" '(embark-act :which-key "embark act")
  "cA" '(embark-dwim :which-key "embark dwim")

  ;; Files
  "f" '(:ignore t :which-key "files")
  "ff" '(find-file :which-key "find file")
  "fr" '(recentf-open-files :which-key "recent files")
  "fs" '(save-buffer :which-key "save file")
  "fS" '(write-file :which-key "save as")
  "fD" '(delete-file :which-key "delete file")
  "fR" '(rename-file :which-key "rename file")
  "fy" '(my/copy-file-path :which-key "copy file path")
  "fe" '(:ignore t :which-key "emacs")
  "fed" '(my/open-config :which-key "open config")
  "feR" '(my/reload-config :which-key "reload config")

  ;; Git (placeholder for future magit integration)
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "status")
  "gd" '(magit-diff-buffer-file :which-key "diff file")
  "gD" '(magit-diff :which-key "diff")
  "gb" '(magit-branch :which-key "branch")
  "gB" '(magit-blame :which-key "blame")
  "gc" '(magit-commit :which-key "commit")
  "gC" '(magit-clone :which-key "clone")
  "gf" '(magit-fetch :which-key "fetch")
  "gF" '(magit-fetch-all :which-key "fetch all")
  "gl" '(magit-log :which-key "log")
  "gL" '(magit-log-buffer-file :which-key "log file")
  "gp" '(magit-push :which-key "push")
  "gP" '(magit-pull :which-key "pull")
  "gs" '(magit-stage-file :which-key "stage file")
  "gS" '(magit-stage-modified :which-key "stage modified")
  "gu" '(magit-unstage-file :which-key "unstage file")
  "gr" '(magit-rebase :which-key "rebase")
  "gm" '(magit-merge :which-key "merge")
  "gt" '(git-timemachine :which-key "time machine")
  "gT" '(magit-todos-list :which-key "todos")
  "gy" '(git-link :which-key "copy link")
  "gY" '(git-link-commit :which-key "copy commit link")
  "g[" '(git-gutter:previous-hunk :which-key "previous hunk")
  "g]" '(git-gutter:next-hunk :which-key "next hunk")
  "gh" '(git-gutter:popup-hunk :which-key "show hunk")
  "gR" '(git-gutter:revert-hunk :which-key "revert hunk")
  "gv" '(blamer-mode :which-key "toggle blamer")

  ;; Help
  "h" '(:ignore t :which-key "help")
  "hf" '(helpful-callable :which-key "describe function")
  "hv" '(helpful-variable :which-key "describe variable")
  "hk" '(helpful-key :which-key "describe key")
  "hc" '(helpful-command :which-key "describe command")
  "hm" '(describe-mode :which-key "describe mode")
  "hp" '(describe-package :which-key "describe package")
  "hb" '(embark-bindings :which-key "show bindings")
  "hi" '(info :which-key "info")
  "ha" '(helpful-at-point :which-key "help at point")

  ;; Insert
  "i" '(:ignore t :which-key "insert")
  "iu" '(insert-char :which-key "unicode char")
  "iy" '(consult-yank-from-kill-ring :which-key "from kill ring")

  ;; Jump/Join
  "j" '(:ignore t :which-key "jump")
  "jj" '(evil-avy-goto-char-timer :which-key "jump to char")
  "jl" '(evil-avy-goto-line :which-key "jump to line")
  "jw" '(evil-avy-goto-word-1 :which-key "jump to word")

  ;; Notes/Org
  "n" '(:ignore t :which-key "notes")
  "na" '(org-agenda :which-key "org agenda")
  "nc" '(org-capture :which-key "org capture")
  "nf" '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "notes file")
  "nl" '(org-store-link :which-key "store link")
  "no" '((lambda () (interactive) (find-file (expand-file-name "notes.org" org-directory))) :which-key "open notes")

  ;; Open
  "o" '(:ignore t :which-key "open")
  "ot" '(treemacs :which-key "treemacs")
  "oe" '(eshell :which-key "eshell")
  "ov" '(vterm :which-key "new vterm")
  "os" '(my/vterm-new-server :which-key "server terminal")
  "oS" '(my/vterm-list-servers :which-key "list servers")
  "op" '(my/vterm-project :which-key "project vterm")
  "ob" '(my/vterm-split-below :which-key "vterm split below")

  ;; Project
  "p" '(:ignore t :which-key "project")
  "pp" '(projectile-switch-project :which-key "switch project")
  "pf" '(projectile-find-file :which-key "find file")
  "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
  "pk" '(projectile-kill-buffers :which-key "kill buffers")
  "pc" '(projectile-compile-project :which-key "compile")
  "pr" '(projectile-run-project :which-key "run")

  ;; Quit
  "q" '(:ignore t :which-key "quit")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
  "qQ" '(kill-emacs :which-key "quit (no save)")

  ;; Search
  "s" '(:ignore t :which-key "search")
  "ss" '(consult-line :which-key "search buffer")
  "si" '(consult-imenu :which-key "imenu")
  "so" '(consult-outline :which-key "outline")
  "sm" '(consult-mark :which-key "jump to mark")
  "sM" '(consult-global-mark :which-key "global mark")
  "sp" '(consult-ripgrep :which-key "search project (ripgrep)")
  "sP" '(consult-grep :which-key "search project (grep)")
  "sb" '(consult-buffer :which-key "search buffers")
  "sB" '(consult-line-multi :which-key "search all buffers")
  "sf" '(consult-find :which-key "find file")
  "sg" '(consult-git-grep :which-key "git grep")
  "sy" '(consult-yank-pop :which-key "yank from kill ring")
  "sj" '(consult-goto-line :which-key "goto line")

  ;; Toggle
  "t" '(:ignore t :which-key "toggle")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
  "tw" '(whitespace-mode :which-key "whitespace")
  "tv" '(visual-line-mode :which-key "visual line")
  "tt" '(treemacs :which-key "treemacs")
  "te" '(treemacs-add-and-display-current-project-exclusively :which-key "treemacs refresh project")
  "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
  "tT" '(load-theme :which-key "load theme")
  "tg" '(git-gutter-mode :which-key "git gutter")
  "tb" '(blamer-mode :which-key "git blamer")

  ;; Windows
  "w" '(:ignore t :which-key "windows")
  "ww" '(other-window :which-key "other window")
  "wd" '(delete-window :which-key "delete window")
  "wD" '(delete-other-windows :which-key "delete other windows")
  "ws" '(my/split-window-below-and-focus :which-key "split below")
  "wv" '(my/split-window-right-and-focus :which-key "split right")
  "wh" '(evil-window-left :which-key "left")
  "wj" '(evil-window-down :which-key "down")
  "wk" '(evil-window-up :which-key "up")
  "wl" '(evil-window-right :which-key "right")
  "wH" '(evil-window-move-far-left :which-key "move far left")
  "wJ" '(evil-window-move-very-bottom :which-key "move bottom")
  "wK" '(evil-window-move-very-top :which-key "move top")
  "wL" '(evil-window-move-far-right :which-key "move far right")
  "w=" '(balance-windows :which-key "balance windows")
  "wm" '(my/delete-other-windows-or-treemacs :which-key "maximize"))

;; Vertico keybindings in insert mode
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "C-d") #'vertico-scroll-up)
  (define-key vertico-map (kbd "C-u") #'vertico-scroll-down))

;; Company keybindings in insert mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-d") #'company-next-page)
  (define-key company-active-map (kbd "C-u") #'company-previous-page)
  (define-key company-active-map (kbd "C-h") #'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-w") #'evil-delete-backward-word))

;; Embark keybindings
(with-eval-after-load 'embark
  (define-key embark-general-map (kbd "j") #'embark-next-symbol)
  (define-key embark-general-map (kbd "k") #'embark-previous-symbol))

;; Additional helper for sending region to vterm
(general-define-key
 :states '(normal visual)
 :keymaps 'override
 "SPC x v" '(my/vterm-send-region :which-key "send to vterm"))

;; Additional useful helpers
(defun my/copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied: %s" filepath))))

(provide 'leader)
;;; leader.el ends here

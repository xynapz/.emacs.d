;;; xz-evil.el --- Evil mode support -*- lexical-binding: t; -*-
;;; Commentary:
;; Robust Evil mode config with standard fixes.

;;; Code:

(use-package evil
  :ensure t
  :demand t
  :init
  ;; Required before evil loads
  (setq evil-want-integration t
        evil-want-keybinding nil      ; Let evil-collection handle this
        evil-want-C-u-scroll t        ; C-u scrolls up (vim behavior)
        evil-want-C-d-scroll t        ; C-d scrolls down
        evil-want-Y-yank-to-eol t     ; Y yanks to EOL (vim behavior)
        evil-undo-system 'undo-redo   ; Native undo-redo
        evil-split-window-below t     ; Split below
        evil-vsplit-window-right t    ; Vsplit right
        evil-respect-visual-line-mode t)  ; j/k on visual lines

  :config
  (evil-mode 1)

  ;; Fast ESC - no delay
  (setq evil-esc-delay 0)

  ;; Cursor shapes per state
  (setq evil-normal-state-cursor  '(box "#00ff41")      ; Green box
        evil-insert-state-cursor  '(bar "#ffb000")      ; Amber bar
        evil-visual-state-cursor  '(hollow "#40f4ff")   ; Cyan hollow
        evil-replace-state-cursor '(hbar "#ff40ff")     ; Magenta hbar
        evil-emacs-state-cursor   '(box "#ff4040"))     ; Red (shouldn't see this)

  ;; Better word boundaries (treat _ as word char)
  (defalias 'forward-evil-word 'forward-evil-symbol)

  ;; Fine-grained undo in insert mode
  (setq evil-want-fine-undo t)

  ;; jk to escape insert mode (fast)
  (define-key evil-insert-state-map (kbd "j")
    (lambda () (interactive)
      (let ((evt (read-event nil nil 0.15)))
        (if (and evt (eq evt ?k))
            (evil-normal-state)
          (insert "j")
          (when evt (push evt unread-command-events)))))))

(use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :init
  (setq evil-collection-setup-minibuffer t)  ; Evil in minibuffer
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode 1))

(provide 'xz-evil)
;;; xz-evil.el ends here

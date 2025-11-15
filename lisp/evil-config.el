;;; evil-config.el --- Evil mode specific configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; evil-mode configuration for my Emacs config.

;;; Code:
(require 'evil)

;; Escape quits everything
(defun my/evil-escape ()
  "Close popups, clear search highlight, and return to normal state."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((evil-ex-hl-active-p 'evil-ex-search)
         (evil-ex-nohighlight))
        (t
         (evil-normal-state))))


;; Use 'jk' or 'kj' as alternative escape (optional)
;; Uncomment if desired:
(use-package evil-escape
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2))

;; Make ESC work everywhere
(global-set-key (kbd "<escape>") #'my/evil-escape)

;; Window navigation in normal state
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Better undo
(evil-set-undo-system 'undo-redo)

;; Cursor appearance
(setq evil-normal-state-cursor '(box "orange")
      evil-insert-state-cursor '(bar "green")
      evil-visual-state-cursor '(hollow "purple")
      evil-replace-state-cursor '(hbar "red")
      evil-operator-state-cursor '(hollow "yellow"))

;; Ex command aliases (Doom-like)
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "wq" 'my/save-and-kill-buffer)
(evil-ex-define-cmd "W" 'save-buffer)

(defun my/save-and-kill-buffer ()
  "Save the current buffer and kill it."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(provide 'evil-config)
;;; evil-config.el ends here

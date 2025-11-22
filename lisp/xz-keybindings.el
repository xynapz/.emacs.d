;;; xz-keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global keybindings that span multiple packages or core Emacs functions.
;; Package-specific bindings are in their use-package declarations.

;;; Code:

;;;; Core Emacs Improvements
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;;; Window Management
(winner-mode 1)
(global-set-key (kbd "C-c <left>") 'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)

;; Windmove - Shift + arrows for window navigation
(windmove-default-keybindings)

;;;; Quick Access Commands (Personal Namespace C-c <letter>)
(global-set-key (kbd "C-c s") 'save-buffer)           ; Quick save
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c r") 'revert-buffer)         ; Refresh buffer
(global-set-key (kbd "C-c =") 'text-scale-increase)
(global-set-key (kbd "C-c -") 'text-scale-decrease)
(global-set-key (kbd "C-c 0") 'text-scale-adjust)

;;;; Search & Navigation (Consult Integration)
;; Override defaults with better Consult versions
(global-set-key (kbd "C-s") 'consult-line)            ; Better isearch
(global-set-key (kbd "C-x b") 'consult-buffer)        ; Better buffer switch
(global-set-key (kbd "M-y") 'consult-yank-pop)        ; Better yank
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "C-x r b") 'consult-bookmark)

;; Advanced search
(global-set-key (kbd "M-s d") 'consult-find)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)

;;;; Embark - Context Actions
(global-set-key (kbd "C-.") 'embark-act)              ; Context menu
(global-set-key (kbd "C-;") 'embark-dwim)             ; Do what I mean
(global-set-key (kbd "C-h B") 'embark-bindings)       ; Explore bindings

;;;; Project Management (Projectile)
;; Use C-c p as projectile prefix (standard convention)
(global-set-key (kbd "C-c p") 'projectile-command-map)

;;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;;;; Avy (Jump to things)
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)

;;;; Org Mode Quick Access
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;;;; Code Navigation & LSP (Eglot)
;; These work when eglot is active
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'xref-go-back)
  (define-key eglot-mode-map (kbd "M-?") 'xref-find-references)
  (define-key eglot-mode-map (kbd "C-h .") 'eldoc-doc-buffer))

;;;; Flycheck
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c ! l") 'flycheck-list-errors)
  (define-key flycheck-mode-map (kbd "C-c ! n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c ! p") 'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c ! v") 'flycheck-verify-setup))

;;;; Helpful Utilities
;; Comment/uncomment
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c M-/") 'comment-or-uncomment-region)

;; Semantic Selection
(global-set-key (kbd "C-=") 'er/expand-region)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'xz-keybindings)
;;; xz-keybindings.el ends here

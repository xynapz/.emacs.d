;;; xz-evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil Mode with selective mode enablement.
;; Key optimizations:
;; - Selective evil-collection loading (not all modes)
;; - Emacs state for Org mode (avoids binding conflicts)
;; - Deferred loading where possible
;; - evil-escape for efficient jk handling

;;; Code:

;; 1. EVIL CORE
(use-package evil
  :ensure t
  :demand t  ; Load immediately since we need it everywhere
  :init
  ;; MUST be set BEFORE evil loads
  (setq evil-want-integration t
        evil-want-keybinding nil       ; Required for evil-collection
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump t           ; C-i for jump forward
        evil-want-Y-yank-to-eol t      ; Y yanks to EOL (like Vim)
        evil-undo-system 'undo-redo    ; Native Emacs 28+ undo
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-fine-undo t          ; More granular undo in insert mode
        evil-respect-visual-line-mode t
        evil-symbol-word-search t)     ; * and # search symbols not words

  :config
  (evil-mode 1)

  ;; C-g to exit insert state
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; USE EMACS STATE FOR PROBLEMATIC MODES
  ;; These modes have their own excellent keybindings
  (dolist (mode '(dired-mode
                  eshell-mode
                  shell-mode
                  term-mode
                  vterm-mode
                  help-mode
                  Info-mode
                  Custom-mode
                  messages-buffer-mode
                  debugger-mode))
    (evil-set-initial-state mode 'emacs))

  ;; MOTION STATE for read-only buffers
  (dolist (mode '(special-mode
                  compilation-mode
                  git-commit-mode))
    (evil-set-initial-state mode 'motion))

  ;; Custom keybindings
  (define-key evil-normal-state-map (kbd "gR") 'revert-buffer)

  ;; Window navigation (like Vim C-w hjkl)
  (define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right))


;; 2. EVIL-ESCAPE (Efficient jk handling)
;; Much more efficient than manual read-event
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.15
        evil-escape-unordered-key-sequence nil)  ; Must be j then k
  (evil-escape-mode 1))


;; 3. EVIL-COLLECTION (Selective Loading)
;; ONLY load for modes where I actually want Evil bindings
(use-package evil-collection
  :ensure t
  :after evil
  :config
  ;; Disable minibuffer setup (often causes issues)
  (setq evil-collection-setup-minibuffer nil)

  ;; SELECTIVE: Only init specific modes I need
  (evil-collection-init '(magit
                          ibuffer
                          vertico
                          consult
                          corfu
                          embark
                          xref
                          ediff
                          flymake
                          profiler)))


;; 4. EVIL-ORG (Vim bindings for Org mode)
(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; Enable all key themes:
  ;; - navigation: hjkl in headings
  ;; - insert: better RET/o behavior
  ;; - textobjects: vae (around element), vie (inner element)
  ;; - additional: M-hjkl for heading manipulation
  ;; - calendar: navigate calendar with hjkl
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))


;; 5. EVIL EXTENSIONS (Lazy-loaded)

;; Surround (cs, ds, ys commands)
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Commentary (gc to comment)
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

;; Increment/decrement numbers (C-a / C-x like Vim)
(use-package evil-numbers
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ("C-x" . evil-numbers/dec-at-pt)
              ;; Visual mode support
              :map evil-visual-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ("C-x" . evil-numbers/dec-at-pt)))


;; 6. FOLDING SUPPORT
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; 7. OPTIONAL: Quick toggle to Emacs state
;; Sometimes I just want to use Emacs bindings temporarily
(define-key evil-normal-state-map (kbd "C-z") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-z") 'evil-normal-state)


(provide 'xz-evil)
;;; xz-evil.el ends here

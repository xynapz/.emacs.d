;;; xz-completion.el --- Completion and search -*- lexical-binding: t; -*-
;;; Commentary:
;; completion framewors for editor and minibuffers.

;; vertico:
                                        ; Displays a vertical list of completion candidates.
                                        ; cycling through candidates from top to bottom and vice versa.
                                        ; allows the minibuffer to grow or shrink dynamically.
;; orderless:
                                        ; Instead of matching just prefixes, you can match using space-separated terms.
                                        ; Typing fu bar could match foobar, barfoo, func-bar, etc.
                                        ; Completion-category-overrides lets us use partial-completion for files, which improves file path matching.
;; marginilia:
                                        ; Enhances completion candidates with metadata.
                                        ; Makes it easier to distinguish similar-looking candidates.
;; consult:
                                        ; Replaces and extends built-in basic commands.
                                        ; consult-line: Search within the current buffer.
                                        ; consult-buffer: Switch between buffers with completion.
                                        ; consult-yank-pop: Browse and insert from kill ring.

;; Embark
                                        ; Provides contextual actions on the item at point.
                                        ; Allows acting on completion candidates from the minibuffer.
                                        ; Offers action menus, previews, and batch operations.

;; Embark-Consult
                                        ; Integrates Embark actions with Consult search/results.
                                        ; Adds live previews and narrowing in Embark menus.
                                        ; Enables exporting results to Consult buffers for further filtering.

;; Corfu
                                        ; Popup-style minimal completion UI using standard CAPF.
                                        ; Non-intrusive inline suggestions with optional documentation popups.
                                        ; Compatible with any completion-at-point backend.

;; Cape
                                        ; Provides extra CAPF backends (keywords, files, symbols, etc.).
                                        ; Allows combining and modifying CAPFs (e.g., cape-super-capf).
                                        ; Makes completion pipelines modular and highly configurable.

;; Flycheck
                                        ; Asynchronous on-the-fly syntax checking.
                                        ; Supports many languages with inline error indicators.
                                        ; Uses external linters for real-time code feedback.

;;; Code:

;;Vertico
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15))

;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Rich annotations
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Consult
(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-y" . consult-yank-pop)))

;; embark
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; embark-consult
(use-package embark-consult
  :after (embark consult))

;; Corfu for in-buffer completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-echo-mode))

;; Cape for additional completion backends
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; flycheck for syntax checking
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; keybinds are in xz-keybindings.el
(use-package consult-projectile
  :ensure t
  :after (consult projectile))

;; Helpful - Better help buffers
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(provide 'xz-completion)
;;; xz-completion.el ends here

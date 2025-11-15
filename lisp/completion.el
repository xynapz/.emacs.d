;;; completion.el --- Completion and search -*- lexical-binding: t; -*-
;;; Commentary:
;; completion framewors for editor and minibuffers.

;;; Code:
;; Vertico - Better minibuffer completion
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15)
  :config
  ;; Evil-like navigation in vertico
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "C-d") #'vertico-scroll-up)
  (define-key vertico-map (kbd "C-u") #'vertico-scroll-down)
  (define-key vertico-map (kbd "C-g") #'abort-recursive-edit))

;; Orderless - Better matching
;; (use-package orderless
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles partial-completion)))))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))
                                    (buffer (styles orderless))
                                    (command (styles orderless))
                                    (variable (styles orderless))
                                    (symbol (styles orderless))))
  :config
  ;; Use orderless everywhere
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  ;; Orderless style dispatchers for more power
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal
                                     orderless-regexp
                                     orderless-initialism
                                     orderless-prefixes)))

;; Marginalia - Rich annotations
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Consult - Enhanced commands
(use-package consult
  :bind (;; Buffer switching
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; Editing
         ("M-y" . consult-yank-pop)
         ;; Navigation
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ;; Search
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep))
  :custom
  (consult-narrow-key "<")
  (consult-project-function #'projectile-project-root)
  :config
  ;; Evil-compatible preview for consult
  (consult-customize
   consult-line
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   :preview-key '(:debounce 0.2 any)))

;; Embark - Contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (embark-quit-after-action nil)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Evil-like navigation in embark
  (define-key embark-general-map (kbd "j") #'embark-next-symbol)
  (define-key embark-general-map (kbd "k") #'embark-previous-symbol)
  (define-key embark-general-map (kbd "C-j") #'embark-next-symbol)
  (define-key embark-general-map (kbd "C-k") #'embark-previous-symbol))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Company - In-buffer completion
(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  :config
  ;; Evil-compatible keybindings
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-d") #'company-next-page)
  (define-key company-active-map (kbd "C-u") #'company-previous-page)
  (define-key company-active-map (kbd "C-h") #'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-w") #'evil-delete-backward-word)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)

  ;; Use TAB to complete, not RET
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-nerd-icons
        company-box-max-candidates 50
        company-box-doc-delay 0.3))

;; Flycheck - Syntax checking
(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3)

  ;; Evil navigation for flycheck errors
  (with-eval-after-load 'evil
    (evil-define-key 'normal flycheck-error-list-mode-map
      (kbd "j") 'flycheck-error-list-next-error
      (kbd "k") 'flycheck-error-list-previous-error
      (kbd "RET") 'flycheck-error-list-goto-error
      (kbd "q") 'quit-window)))

;; Flymake integration with evil
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)

  ;; Evil navigation
  (with-eval-after-load 'evil
    (evil-define-key 'normal flymake-diagnostics-buffer-mode-map
      (kbd "j") 'flymake-goto-next-error
      (kbd "k") 'flymake-goto-prev-error
      (kbd "RET") 'flymake-show-diagnostic
      (kbd "q") 'quit-window)))

;; Helpful - Better help buffers
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  :config
  ;; Evil navigation in helpful buffers
  (with-eval-after-load 'evil
    (evil-define-key 'normal helpful-mode-map
      (kbd "q") 'quit-window
      (kbd "g?") 'describe-mode
      (kbd "gr") 'helpful-update)))

(provide 'completion)
;;; completion.el ends here

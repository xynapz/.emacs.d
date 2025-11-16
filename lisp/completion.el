;;; completion.el --- Completion and search -*- lexical-binding: t; -*-
;;; Commentary:
;; completion framewors for editor and minibuffers.
;; vertico:
;; marginilia:
;; orderless:
;; helpful:

;;; Code:
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15))

;; Marginalia - Rich annotations
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Helpful - Better help buffers
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(provide 'completion)
;;; completion.el ends here

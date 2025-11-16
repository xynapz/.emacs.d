;;; vterm-config.el --- VTerm terminal configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; vterm splits

;;; Code:
;; VTerm - Fast terminal emulator
(use-package vterm )

;; vterm configuration
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Set shell to use
  (setq vterm-shell (getenv "SHELL"))

  ;; Maximum scrollback
  (setq vterm-max-scrollback 10000)

  ;; Kill buffer when vterm process exits
  (setq vterm-kill-buffer-on-exit t)

  ;; Evil mode integration
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))

  ;; Keybindings
  :bind (("C-c t" . vterm-project)
         ("C-c T" . vterm-project-other-window)))

;; Function to open vterm in project root
(defun vterm-project ()
  "Open vterm in project root (full buffer)."
  (interactive)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (default-directory project-root)
         (buffer-name (format "*vterm: %s*"
                            (file-name-nondirectory
                             (directory-file-name project-root)))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))))

;; Function to open vterm in split window
(defun vterm-project-other-window ()
  "Open vterm in project root (split window)."
  (interactive)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (default-directory project-root)
         (buffer-name (format "*vterm: %s*"
                            (file-name-nondirectory
                             (directory-file-name project-root)))))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (progn
        (split-window-below)
        (other-window 1)
        (vterm buffer-name)))))

;; Optional: Integration with projectile
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p t") 'vterm-project)
  (define-key projectile-mode-map (kbd "C-c p T") 'vterm-project-other-window))

;; Toggle functions for terminals
(defun vterm-toggle ()
  "Toggle vterm buffer visibility."
  (interactive)
  (let ((vterm-buffers (seq-filter
                        (lambda (buf)
                          (with-current-buffer buf
                            (eq major-mode 'vterm-mode)))
                        (buffer-list))))
    (if vterm-buffers
        (if (eq major-mode 'vterm-mode)
            (previous-buffer)
          (switch-to-buffer (car vterm-buffers)))
      (vterm))))

(defun vterm-toggle-split ()
  "Toggle vterm in split window."
  (interactive)
  (let ((vterm-windows (seq-filter
                        (lambda (win)
                          (with-current-buffer (window-buffer win)
                            (eq major-mode 'vterm-mode)))
                        (window-list))))
    (if vterm-windows
        (if (eq major-mode 'vterm-mode)
            (delete-window)
          (select-window (car vterm-windows)))
      (vterm-project-other-window))))

;; SPC t bindings (add to your general/which-key config)
;; Assuming you have general.el installed
(general-define-key
 :states '(normal visual motion)
 :keymaps 'override
 :prefix "SPC"

 ;; Terminal category
 "v" '(:ignore t :which-key "vterm")
 "vt" '(vterm-toggle :which-key "toggle terminal")
 "vT" '(vterm-toggle-split :which-key "toggle split")
 "vn" '(vterm :which-key "new vterm")
 "vp" '(vterm-project :which-key "project terminal")
 "vs" '(vterm-project-other-window :which-key "split below")
 "vk" '(kill-buffer :which-key "kill terminal")
 "vr" '((lambda () (interactive)
          (when (eq major-mode 'vterm-mode)
            (vterm-send-C-l))) :which-key "clear terminal")
 "vi" '((lambda () (interactive)
          (when (eq major-mode 'vterm-mode)
            (evil-insert-state))) :which-key "insert mode"))

(provide 'vterm-config)
;;; vterm.el ends here

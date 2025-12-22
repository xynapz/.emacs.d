;;; cc.el --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;; C++ LSP, formatting, and linting with Google style
;;; Code:

;; Make .h open as C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Google style configuration
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "google")
            (setq c-basic-offset 2)))

;; Disable namespace indentation
(c-set-offset 'innamespace 0)
(setq-default indent-tabs-mode nil
              c-basic-offset 2
              tab-width 2)

;; Eglot LSP Configuration
(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode) . xz/eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . eglot-format-buffer))
  :preface
  :preface
  (defun xz/eglot-ensure ()
    "Start eglot unless we are in an org export buffer."
    (when (and (buffer-file-name)
               (not (bound-and-true-p org-export-current-backend)))
      (eglot-ensure)))
  :config
  (setq-default eglot-workspace-configuration
                '(:clangd (:fallbackFlags ["-std=c++20"])))

  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--header-insertion=never"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion-decorators"
                    "--pch-storage=memory"
                    "--background-index"))))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style "google"
        clang-format-fallback-style "google"))

(defun xz/c++-format-on-save ()
  "Format C++ buffer with clang-format on save."
  (when (and (derived-mode-p 'c++-mode 'c-mode)
             (executable-find "clang-format"))
    (clang-format-buffer)))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'xz/c++-format-on-save nil 'local)))

(add-hook 'c-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'xz/c++-format-on-save nil 'local)))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(defun xz/cmake-clean ()
  "Delete the build directory from project root."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (project-root (project-current t))))
         (build-dir (file-name-as-directory (concat project-root "build"))))
    (when (file-exists-p build-dir)
      (if (yes-or-no-p (format "Delete %s? " build-dir))
          (progn
            (delete-directory build-dir t)
            (message "Deleted build directory"))
        (message "Cancelled")))))

(defun xz/cmake-configure ()
  "Run cmake .. from build directory and create symlink for compile_commands.json."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (project-root (project-current t))))
         (build-dir (file-name-as-directory (concat project-root "build")))
         (default-directory build-dir)
         (compile-commands-src (concat build-dir "compile_commands.json"))
         (compile-commands-dest (concat project-root "compile_commands.json")))
    (unless (file-exists-p build-dir)
      (make-directory build-dir t))
    (compile "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..")
    ;; Create symlink after cmake runs
    (run-with-timer 2 nil
                    (lambda ()
                      (when (file-exists-p compile-commands-src)
                        (when (file-exists-p compile-commands-dest)
                          (delete-file compile-commands-dest))
                        (make-symbolic-link compile-commands-src compile-commands-dest t)
                        (message "Created symlink: compile_commands.json -> build/"))))))

(defun xz/cmake-build ()
  "Compile the project using make in build directory."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (project-root (project-current t))))
         (build-dir (file-name-as-directory (concat project-root "build")))
         (default-directory build-dir))
    (compile "make")))

(defun xz/cmake-run ()
  "Run the compiled executable in an interactive comint buffer."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (project-root (project-current t))))
         (build-dir (file-name-as-directory (concat project-root "build")))
         (default-directory build-dir)
         (executables (directory-files build-dir nil "^[^.].*[^.o]$"))
         (exe (if (= (length executables) 1)
                  (car executables)
                (completing-read "Run executable: " executables)))
         (buffer-name (format "*run: %s*" exe)))
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (comint-mode)
        (setq-local comint-process-echoes nil)
        (erase-buffer))
      (make-comint-in-buffer exe buf (concat "./" exe))
      (pop-to-buffer buf))))

;; Key Bindings
(global-set-key (kbd "<f4>") 'xz/cmake-clean)
(global-set-key (kbd "<f5>") 'xz/cmake-configure)
(global-set-key (kbd "<f6>") 'xz/cmake-build)
(global-set-key (kbd "<f7>") 'xz/cmake-run)
(global-set-key (kbd "<f8>") 'recompile)

;; C++ mode local keybindings
(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c C-f") 'clang-format-buffer)
  (define-key c++-mode-map (kbd "C-c C-r") 'clang-format-region))

(provide 'cc)
;;; cc.el ends here

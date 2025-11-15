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
  :hook ((c-mode c++-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . eglot-format-buffer))
  :config
  ;; Configure clangd with C++20 and clang-tidy
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

;; Clang-Format Integration (Google Style)
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style "google"
        clang-format-fallback-style "google"))

;; Auto-format on save
(defun my/c++-format-on-save ()
  "Format C++ buffer with clang-format on save."
  (when (and (derived-mode-p 'c++-mode 'c-mode)
             (executable-find "clang-format"))
    (clang-format-buffer)))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/c++-format-on-save nil 'local)))

(add-hook 'c-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/c++-format-on-save nil 'local)))

;; Flycheck with Clang-Tidy (Google Style)
(use-package flycheck
  :ensure t
  :hook ((c-mode c++-mode) . flycheck-mode)
  :config
  ;; Set C++20 standard
  (setq flycheck-clang-language-standard "c++20"
        flycheck-gcc-language-standard "c++20")

  ;; Configure clang-tidy with Google checks
  (setq flycheck-clang-tidy-extra-options
        '("--checks=google-*,modernize-*,readability-*,performance-*")))

;; Enable clang-tidy checker
(with-eval-after-load 'flycheck
  (when (require 'flycheck-clang-tidy nil 'noerror)
    (flycheck-clang-tidy-setup)))

;; CMake Support
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;; CMake Build Functions
(defun my/cmake-clean ()
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

(defun my/cmake-configure ()
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

(defun my/cmake-build ()
  "Compile the project using make in build directory."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (project-root (project-current t))))
         (build-dir (file-name-as-directory (concat project-root "build")))
         (default-directory build-dir))
    (compile "make")))

(defun my/cmake-run ()
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
(global-set-key (kbd "<f4>") 'my/cmake-clean)      ; delete build folder
(global-set-key (kbd "<f5>") 'my/cmake-configure)  ; cmake .. + symlink
(global-set-key (kbd "<f6>") 'my/cmake-build)      ; make
(global-set-key (kbd "<f7>") 'my/cmake-run)        ; run executable (interactive)
(global-set-key (kbd "<f8>") 'recompile)           ; recompile on error

;; C++ mode local keybindings
(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c C-f") 'clang-format-buffer)
  (define-key c++-mode-map (kbd "C-c C-r") 'clang-format-region))

(provide 'cc)
;;; cc.el ends here

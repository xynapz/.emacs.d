;;; server-manager.el --- Manage development servers -*- lexical-binding: t; -*-
;;; Commentary:
;; Elegant way to manage multiple development servers in Emacs
;; Uses comint-mode for better performance than vterm
;;; Code:

(require 'comint)
(require 'ansi-color)

;; Server Registry - Define your common servers here
(defvar server-manager-servers
  '((flask . (:name "Flask"
                    :command "python -m flask run"
                    :port 5000
                    :dir nil))
    (go . (:name "Go"
                 :command "go run main.go"
                 :port 8080
                 :dir nil))
    (node . (:name "Node.js"
                   :command "node server.js"
                   :port 3000
                   :dir nil))
    (pnpm-dev . (:name "pnpm dev"
                       :command "pnpm run dev"
                       :port 3000
                       :dir nil))
    (docker . (:name "Docker Compose"
                     :command "docker-compose up"
                     :port nil
                     :dir nil))
    (python-http . (:name "Python HTTP"
                          :command "python -m http.server"
                          :port 8000
                          :dir nil)))
  "Registry of common development servers.")

;; Core Functions
(defvar server-manager--active-servers nil
  "List of currently active server processes.")

(defun server-manager--get-project-root ()
  "Get project root directory."
  (or (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      (when-let ((project (project-current)))
        (project-root project))
      default-directory))

(defun server-manager-start (server-key &optional custom-command)
  "Start a server from the registry by SERVER-KEY.
If CUSTOM-COMMAND is provided, use that instead."
  (interactive
   (list (intern (completing-read "Start server: "
                                  (mapcar #'car server-manager-servers)
                                  nil t))))
  (let* ((server-config (alist-get server-key server-manager-servers))
         (server-name (plist-get server-config :name))
         (command (or custom-command (plist-get server-config :command)))
         (port (plist-get server-config :port))
         (dir (or (plist-get server-config :dir)
                  (server-manager--get-project-root)))
         (buffer-name (format "*server: %s*" server-name))
         (default-directory dir))

    ;; Check if server is already running
    (when (get-buffer buffer-name)
      (if (y-or-n-p (format "Server '%s' is already running. Restart? " server-name))
          (server-manager-stop server-key)
        (user-error "Server already running")))

    ;; Start the server
    (let* ((buffer (get-buffer-create buffer-name))
           (process (start-process-shell-command
                     (format "server-%s" server-name)
                     buffer
                     command)))

      (with-current-buffer buffer
        (server-manager-mode)
        (setq-local server-manager--server-key server-key)
        (setq-local server-manager--port port)
        (goto-char (point-max)))

      ;; Track active server
      (push (list server-key buffer process) server-manager--active-servers)

      ;; Set up process sentinel
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (message "Server '%s' %s" server-name (string-trim event))
           (setq server-manager--active-servers
                 (cl-remove-if (lambda (s) (eq (nth 2 s) proc))
                               server-manager--active-servers)))))

      ;; Display buffer
      (display-buffer buffer)

      (message "Started %s server on port %s in %s"
               server-name
               (or port "N/A")
               dir))))

(defun server-manager-stop (server-key)
  "Stop a running server by SERVER-KEY."
  (interactive
   (list (intern (completing-read "Stop server: "
                                  (mapcar #'car server-manager--active-servers)
                                  nil t))))
  (if-let* ((server-entry (assq server-key server-manager--active-servers))
            (buffer (nth 1 server-entry))
            (process (nth 2 server-entry)))
      (progn
        (when (process-live-p process)
          (interrupt-process process)
          (sit-for 0.5)
          (when (process-live-p process)
            (kill-process process)))
        (kill-buffer buffer)
        (setq server-manager--active-servers
              (assq-delete-all server-key server-manager--active-servers))
        (message "Stopped server: %s" server-key))
    (user-error "Server not found: %s" server-key)))

(defun server-manager-stop-all ()
  "Stop all running servers."
  (interactive)
  (when (and server-manager--active-servers
             (y-or-n-p (format "Stop all %d server(s)? "
                               (length server-manager--active-servers))))
    (dolist (server server-manager--active-servers)
      (let ((process (nth 2 server)))
        (when (process-live-p process)
          (kill-process process)))
      (kill-buffer (nth 1 server)))
    (setq server-manager--active-servers nil)
    (message "All servers stopped")))

(defun server-manager-restart (server-key)
  "Restart a server by SERVER-KEY."
  (interactive
   (list (intern (completing-read "Restart server: "
                                  (mapcar #'car server-manager--active-servers)
                                  nil t))))
  (server-manager-stop server-key)
  (sit-for 0.5)
  (server-manager-start server-key))

(defun server-manager-list ()
  "Show all running servers."
  (interactive)
  (if (null server-manager--active-servers)
      (message "No servers running")
    (let ((buf (get-buffer-create "*Server Manager*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert (propertize "Running Servers\n" 'face 'bold)
                (propertize "================\n\n" 'face 'bold))
        (dolist (server server-manager--active-servers)
          (let* ((key (car server))
                 (buffer (nth 1 server))
                 (process (nth 2 server))
                 (config (alist-get key server-manager-servers))
                 (name (plist-get config :name))
                 (port (plist-get config :port))
                 (status (if (process-live-p process) "Running" "Stopped")))
            (insert (format "• %s - %s (port: %s) [%s]\n"
                            (propertize (symbol-name key) 'face 'font-lock-keyword-face)
                            name
                            (or port "N/A")
                            (propertize status 'face
                                        (if (process-live-p process)
                                            'success
                                          'error))))))
        (insert "\n" (propertize "Keybindings:\n" 'face 'bold))
        (insert "  RET - Jump to server buffer\n")
        (insert "  k   - Kill server\n")
        (insert "  r   - Restart server\n")
        (insert "  q   - Quit window\n")
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf))))

(defun server-manager-custom ()
  "Start a custom server with user-provided command."
  (interactive)
  (let* ((command (read-string "Command: "))
         (name (read-string "Server name: " (car (split-string command))))
         (port (read-number "Port (0 for none): " 0)))
    (server-manager-start
     (intern name)
     command)))

(defun server-manager-open-browser ()
  "Open browser for the current server."
  (interactive)
  (if-let ((port (and (boundp 'server-manager--port) server-manager--port)))
      (browse-url (format "http://localhost:%d" port))
    (message "No port configured for this server")))

;; Server Manager Mode (for server buffers)
(defvar server-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'server-manager-stop-current)
    (define-key map (kbd "C-c C-r") 'server-manager-restart-current)
    (define-key map (kbd "C-c C-b") 'server-manager-open-browser)
    (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
    (define-key map (kbd "C-l") 'server-manager-clear-buffer)
    map)
  "Keymap for server manager mode.")

(define-derived-mode server-manager-mode comint-mode "Server"
  "Major mode for server output buffers.

\\{server-manager-mode-map}"
  (setq-local comint-process-echoes nil)
  (setq-local comint-scroll-to-bottom-on-output t)
  (setq-local comint-scroll-show-maximum-output t)

  ;; Enable ANSI colors
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer nil t)
  (add-hook 'comint-output-filter-functions 'ansi-color-process-output nil t))

(defun server-manager-stop-current ()
  "Stop the server in the current buffer."
  (interactive)
  (when (boundp 'server-manager--server-key)
    (server-manager-stop server-manager--server-key)))

(defun server-manager-restart-current ()
  "Restart the server in the current buffer."
  (interactive)
  (when (boundp 'server-manager--server-key)
    (server-manager-restart server-manager--server-key)))

(defun server-manager-clear-buffer ()
  "Clear the server output buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (comint-send-input))

;; Projectile Integration (Optional)
(defun server-manager-detect-and-start ()
  "Auto-detect project type and start appropriate server."
  (interactive)
  (let* ((root (server-manager--get-project-root))
         (default-directory root))
    (cond
     ((file-exists-p "package.json")
      (server-manager-start 'pnpm-dev))
     ((file-exists-p "app.py")
      (server-manager-start 'flask))
     ((file-exists-p "go.mod")
      (server-manager-start 'go))
     ((file-exists-p "docker-compose.yml")
      (server-manager-start 'docker))
     ((file-exists-p "config.toml")
      (server-manager-start 'hugo))
     (t
      (message "No known server configuration detected")))))

;; Global Keybindings
(global-set-key (kbd "C-c s s") 'server-manager-start)
(global-set-key (kbd "C-c s k") 'server-manager-stop)
(global-set-key (kbd "C-c s r") 'server-manager-restart)
(global-set-key (kbd "C-c s l") 'server-manager-list)
(global-set-key (kbd "C-c s c") 'server-manager-custom)
(global-set-key (kbd "C-c s a") 'server-manager-stop-all)
(global-set-key (kbd "C-c s d") 'server-manager-detect-and-start)
(global-set-key (kbd "C-c s b") 'server-manager-open-browser)

(provide 'server-manager)
;;; server-manager.el ends here

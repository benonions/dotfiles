;;; launchers.el --- app launchers -*- lexical-binding: t; -*-
(defun discover-and-launch-mac-app ()
  "Fuzzy-select and open an app from /Applications."
  (interactive)
  (let* ((apps (delete-dups
                (append
                 (directory-files "/Applications" nil "\\.app$")
                 (directory-files "/System/Applications" nil "\\.app$"))))
         (clean-names (mapcar (lambda (a) (string-remove-suffix ".app" a)) apps))
         (choice (completing-read "Launch app: " clean-names)))
    (start-process "open-mac-app" nil "open" "-a" choice)))

(defvar my/ghostty-processes (make-hash-table :test 'equal)
  "Hash table mapping project names to their Ghostty processes.")

(defun my/project-session-name ()
  "Get a tmux session name based on current projectile project."
  (if (projectile-project-p)
      (let ((name (projectile-project-name)))
        ;; tmux session names can't have dots or colons
        (replace-regexp-in-string "[.:]" "-" name))
    "emacs"))

(defun open-ghostty-here ()
  "Open Ghostty with tmux session tied to current projectile project."
  (interactive)
  (let* ((dir (if (projectile-project-p)
                  (projectile-project-root)
                (or (and (buffer-file-name)
                         (file-name-directory (buffer-file-name)))
                    default-directory)))
         (session-name (my/project-session-name))
         (tmux-cmd (format "tmux new-session -A -s %s" session-name))
         (proc (start-process (concat "ghostty-" session-name) nil
                              "/Applications/Ghostty.app/Contents/MacOS/ghostty"
                              (concat "--working-directory=" (expand-file-name dir))
                              "-e" "bash" "-c" tmux-cmd)))
    (puthash session-name proc my/ghostty-processes)
    (message "Ghostty opened with tmux session: %s" session-name)))

(defun kill-ghostty ()
  "Kill the Ghostty process for the current project."
  (interactive)
  (let* ((session-name (my/project-session-name))
         (proc (gethash session-name my/ghostty-processes)))
    (if (and proc (process-live-p proc))
        (progn
          (kill-process proc)
          (remhash session-name my/ghostty-processes)
          (message "Ghostty killed for session: %s" session-name))
      (message "No Ghostty process for session: %s" session-name))))

(defun kill-all-ghostty ()
  "Kill all Ghostty processes spawned by Emacs."
  (interactive)
  (maphash (lambda (name proc)
             (when (process-live-p proc)
               (kill-process proc)))
           my/ghostty-processes)
  (clrhash my/ghostty-processes)
  (message "All Ghostty processes killed"))

(defun open-k9s-here ()
  "Open Ghostty running k9s for the current project."
  (interactive)
  (let* ((dir (if (projectile-project-p)
                  (projectile-project-root)
                (or (and (buffer-file-name)
                         (file-name-directory (buffer-file-name)))
                    default-directory)))
         (session-name (concat "k9s-" (my/project-session-name)))
         (proc (start-process (concat "ghostty-" session-name) nil
                              "/Applications/Ghostty.app/Contents/MacOS/ghostty"
                              (concat "--working-directory=" (expand-file-name dir))
                              "-e" "k9s")))
    (puthash session-name proc my/ghostty-processes)
    (message "k9s opened for: %s" session-name)))

(defun open-lnav-here ()
  "Open Ghostty running lnav on the project's logs/ directory."
  (interactive)
  (let* ((dir (if (projectile-project-p)
                  (projectile-project-root)
                default-directory))
         (logs-dir (expand-file-name "logs" dir)))
    (if (file-directory-p logs-dir)
        (let* ((session-name (concat "lnav-" (my/project-session-name)))
               (proc (start-process (concat "ghostty-" session-name) nil
                                    "/Applications/Ghostty.app/Contents/MacOS/ghostty"
                                    (concat "--working-directory=" (expand-file-name dir))
                                    "-e" "lnav" logs-dir)))
          (puthash session-name proc my/ghostty-processes)
          (message "lnav opened for: %s" logs-dir))
      (message "No logs/ directory found in %s" dir))))

(map! :leader
      :desc "Open Ghostty" "o g" #'open-ghostty-here
      :desc "Kill Ghostty" "o G" #'kill-ghostty
      :desc "Open k9s" "o k" #'open-k9s-here
      :desc "Open lnav" "o l" #'open-lnav-here)

(provide 'launchers)

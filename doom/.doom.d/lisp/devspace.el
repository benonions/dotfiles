;;; devspace.el --- DevSpace integration for Emacs -*- lexical-binding: t; -*-

;; Author: Ben
;; Keywords: kubernetes, devspace, development

;;; Commentary:
;; Simple DevSpace integration with transient menu and project awareness.

;;; Code:

(require 'project)
(require 'transient)
(require 'comint)

(defgroup devspace nil
  "DevSpace integration."
  :group 'tools
  :prefix "devspace-")

(defcustom devspace-executable "devspace"
  "Path to devspace executable."
  :type 'string
  :group 'devspace)

(defcustom devspace-terminal nil
  "Terminal to use for external commands. Set to nil to use vterm."
  :type '(choice (const :tag "Use vterm" nil)
                 (string :tag "Terminal command"))
  :group 'devspace)

(defvar devspace--last-namespace nil
  "Last used namespace.")

(defvar devspace--last-context nil
  "Last used kube context.")

;;; Project Detection

(defun devspace--project-root ()
  "Find project root containing devspace.yaml."
  (locate-dominating-file default-directory "devspace.yaml"))

(defun devspace--ensure-project ()
  "Ensure we're in a devspace project, error otherwise."
  (or (devspace--project-root)
      (user-error "No devspace.yaml found in project hierarchy")))

;;; Command Builders

(defun devspace--build-args ()
  "Build common args from transient state."
  (let (args)
    (when devspace--last-namespace
      (push (format "--namespace=%s" devspace--last-namespace) args))
    (when devspace--last-context
      (push (format "--kube-context=%s" devspace--last-context) args))
    (nreverse args)))

;;; Commands

(defun devspace--run-in-terminal (cmd)
  "Run CMD in external terminal or vterm based on `devspace-terminal'."
  (let ((dir (devspace--ensure-project)))
    (if devspace-terminal
        (cond
         ((string= devspace-terminal "ghostty")
          (let ((script-file (make-temp-file "devspace-" nil ".sh")))
            (with-temp-file script-file
              (insert "#!/bin/bash\n")
              (insert (format "cd %s\n" (shell-quote-argument dir)))
              (insert cmd "\n")
              (insert "exec $SHELL\n"))
            (chmod script-file #o755)
            (start-process "devspace-terminal" nil
                           "open" "-na" "Ghostty" "--args"
                           "-e" script-file)))
         (t
          (start-process "devspace-terminal" nil
                         devspace-terminal "-e" "sh" "-c"
                         (format "cd %s && %s; exec $SHELL" dir cmd))))
      (let ((default-directory dir))
        (devspace--run-in-vterm "*devspace*" cmd)))))

(defun devspace--run-in-vterm (buffer-name cmd)
  "Run CMD in vterm with BUFFER-NAME, similar to run-devspace-dev."
  (require 'vterm)
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (if (yes-or-no-p (format "%s already running. Kill and restart? " buffer-name))
          (kill-buffer buffer)
        (pop-to-buffer buffer)
        (cl-return-from devspace--run-in-vterm))))
  (let ((vterm-buffer (get-buffer-create buffer-name)))
    (with-current-buffer (vterm vterm-buffer)
      (vterm-send-string cmd)
      (vterm-send-return))
    (pop-to-buffer vterm-buffer)))

(defun devspace-dev ()
  "Run `devspace dev' in terminal."
  (interactive)
  (let* ((default-directory (devspace--ensure-project))
         (args (devspace--build-args))
         (cmd (string-join (append (list devspace-executable "dev") args) " ")))
    (devspace--run-in-terminal cmd)))

(defun devspace-reset-pods ()
  "Run `devspace reset pods'."
  (interactive)
  (let* ((default-directory (devspace--ensure-project))
         (args (devspace--build-args))
         (cmd (string-join
               (append (list devspace-executable "reset" "pods") args)
               " ")))
    (message "Running: %s" cmd)
    (async-shell-command cmd "*devspace-reset*")))

(defun devspace-logs ()
  "Run `devspace logs' in terminal."
  (interactive)
  (let* ((default-directory (devspace--ensure-project))
         (args (devspace--build-args))
         (cmd (string-join (append (list devspace-executable "logs" "-f") args) " ")))
    (devspace--run-in-terminal cmd)))

(defun devspace-enter ()
  "Run `devspace enter' in terminal."
  (interactive)
  (let* ((default-directory (devspace--ensure-project))
         (args (devspace--build-args))
         (cmd (string-join (append (list devspace-executable "enter") args) " ")))
    (devspace--run-in-terminal cmd)))

;;; Container Picker

(defun devspace--get-pods ()
  "Get list of pods in current namespace."
  (let* ((default-directory (devspace--ensure-project))
         (ns-arg (if devspace--last-namespace
                     (format "-n %s" devspace--last-namespace)
                   ""))
         (cmd (format "kubectl get pods %s -o jsonpath='{range .items[*]}{.metadata.name}{\"\\n\"}{end}'" ns-arg)))
    (split-string (shell-command-to-string cmd) "\n" t)))

(defun devspace-enter-pod ()
  "Select a pod and exec into it."
  (interactive)
  (let* ((default-directory (devspace--ensure-project))
         (pods (devspace--get-pods))
         (pod (completing-read "Pod: " pods nil t)))
    (devspace--run-in-terminal (format "kubectl exec -it %s -- sh" pod))))

(defun devspace-logs-pod ()
  "Select a pod and tail its logs."
  (interactive)
  (let* ((default-directory (devspace--ensure-project))
         (pods (devspace--get-pods))
         (pod (completing-read "Pod: " pods nil t)))
    (devspace--run-in-terminal (format "kubectl logs -f %s" pod))))

;;; Port Forwards

(defun devspace--get-port-forwards ()
  "Get active port forwards from devspace."
  (let* ((default-directory (devspace--ensure-project))
         (output (shell-command-to-string "devspace list ports 2>/dev/null")))
    output))

(defun devspace-list-ports ()
  "Display active port forwards."
  (interactive)
  (let ((ports (devspace--get-port-forwards)))
    (with-current-buffer (get-buffer-create "*devspace-ports*")
      (erase-buffer)
      (insert "DevSpace Port Forwards\n")
      (insert "======================\n\n")
      (insert (ansi-color-apply ports))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer "*devspace-ports*")))

;;; Transient Menu

(transient-define-infix devspace--namespace ()
  :class 'transient-lisp-variable
  :variable 'devspace--last-namespace
  :description "Namespace"
  :key "-n"
  :reader (lambda (_prompt _init _hist)
            (read-string "Namespace: " devspace--last-namespace)))

(transient-define-infix devspace--context ()
  :class 'transient-lisp-variable
  :variable 'devspace--last-context
  :description "Kube context"
  :key "-c"
  :reader (lambda (_prompt _init _hist)
            (read-string "Kube context: " devspace--last-context)))

;;;###autoload
(transient-define-prefix devspace ()
  "DevSpace commands."
  :value '()
  ["Options"
   (devspace--namespace)
   (devspace--context)]
  ["Commands"
   ("d" "Dev mode" devspace-dev)
   ("r" "Reset pods" devspace-reset-pods)
   ("l" "Logs (follow)" devspace-logs)
   ("e" "Enter (shell)" devspace-enter)]
  ["Pod Picker (kubectl)"
   ("E" "Enter pod..." devspace-enter-pod)
   ("L" "Logs pod..." devspace-logs-pod)]
  ["Info"
   ("p" "List ports" devspace-list-ports)
   ("g" "Go to project" devspace-goto-project)])

(defun devspace-goto-project ()
  "Open dired in devspace project root."
  (interactive)
  (dired (devspace--ensure-project)))

(provide 'devspace)
;;; devspace.el ends here

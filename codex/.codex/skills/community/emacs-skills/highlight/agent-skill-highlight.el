(eval-when-compile
  (require 'cl-lib))
(require 'hi-lock)

(defvar-local agent-skill-highlight--overlays nil)
(defvar-local agent-skill-highlight--was-read-only nil)

(defun agent-skill-highlight--remove-overlays ()
  (mapc #'delete-overlay agent-skill-highlight--overlays)
  (setq agent-skill-highlight--overlays nil))

(defun agent-skill-highlight-exit ()
  (interactive)
  (agent-skill-highlight-mode -1))

(define-minor-mode agent-skill-highlight-mode
  "Temporary read-only mode with highlighted regions. Press q to exit."
  :lighter " Highlight"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'agent-skill-highlight-exit)
            map)
  (if agent-skill-highlight-mode
      (progn
        (setq agent-skill-highlight--was-read-only buffer-read-only)
        (read-only-mode 1)
        (message "Press q to exit highlights"))
    (agent-skill-highlight--remove-overlays)
    (unless agent-skill-highlight--was-read-only
      (read-only-mode -1))))

(cl-defun agent-skill-highlight (&key files)
  "Highlight regions in FILES.

FILES is a list of (FILE-PATH :regions REGIONS) where REGIONS is
a list of (:start LINE :lines COUNT)."
  (dolist (file-spec files)
    (let ((file-path (car file-spec))
          (regions (plist-get (cdr file-spec) :regions)))
      (find-file file-path)
      (dolist (region regions)
        (let* ((start-line (plist-get region :start))
               (num-lines (plist-get region :lines))
               (ov (make-overlay
                    (progn (goto-char (point-min))
                           (forward-line (1- start-line))
                           (point))
                    (progn (forward-line num-lines)
                           (point)))))
          (overlay-put ov 'face 'hi-yellow)
          (push ov agent-skill-highlight--overlays)))
      (agent-skill-highlight-mode 1))))

(provide 'agent-skill-highlight)

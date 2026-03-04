(require 'cl-lib)

(cl-defun agent-skill-select (&key selections)
  "Open files in Emacs and select a region in each.

SELECTIONS is a list of (FILE :start LINE :end LINE)."
  (dolist (sel selections)
    (let ((file (car sel))
          (start (plist-get (cdr sel) :start))
          (end (plist-get (cdr sel) :end)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- start))
      (set-mark (point))
      (forward-line (- end start))
      (end-of-line)
      (activate-mark))))

(provide 'agent-skill-select)

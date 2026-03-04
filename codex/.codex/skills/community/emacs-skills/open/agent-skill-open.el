(require 'cl-lib)

(cl-defun agent-skill-open (&key files)
  "Open FILES in Emacs buffers.

FILES is a list of file specs.  Each spec is either a string
\(file path) or a plist (:file PATH :line LINE)."
  (dolist (spec files)
    (if (stringp spec)
        (find-file spec)
      (let ((file (plist-get spec :file))
            (line (plist-get spec :line)))
        (find-file file)
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))))))

(provide 'agent-skill-open)

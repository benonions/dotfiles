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

(provide 'launchers)

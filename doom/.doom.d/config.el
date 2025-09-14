;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "Ben Onions"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face




;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)
(use-package! verb
  :commands verb-mode
  :mode ("\\.\\(http\\|rest\\)\\'" . verb-mode))
;;; Apply transparency to all new frames
                                        ;(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
                                        ;
;;; Apply transparency to current frame (esp. GUI startup)
                                        ;(when (display-graphic-p)
                                        ;  (set-frame-parameter (selected-frame) 'alpha '(90 . 90)))
                                        ;
;;; Ensure future frames also get transparency when using daemon
                                        ;(add-hook 'after-make-frame-functions
                                        ;          (lambda (f)
                                        ;            (when (display-graphic-p f)
                                        ;              (set-frame-parameter f 'alpha '(90 . 90)))))


(doom/set-frame-opacity 93) ;; Set to 90% opacity (adjust as desired)
(set-frame-parameter nil 'alpha-background 90) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")
(setq org-noter-notes-search-path '("~/org/notes/"))
(setq org-roam-directory (file-truename "~/org/notes"))

(org-roam-db-autosync-mode)
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r u") #'org-roam-ui-mode))

(setq auth-sources '("~/.authinfo"))
(setq jiralib-url "https://nepgroup.atlassian.net")
(global-set-key (kbd "C-c t") #'+vterm/toggle)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how

(after! flycheck
  (require 'flycheck-golangci-lint)
  (flycheck-golangci-lint-setup))

;; Optional: run only this checker for Go
(after! go-mode
  (setq flycheck-checker 'golangci-lint))
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


(after! projectile
  ;; Only disable require-root for TRAMP (not globally)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t)

  (setq projectile-project-search-path
        '("~/code/"
          "~/org"))
  ;; Ignore vendor
  (add-to-list 'projectile-globally-ignored-directories "vendor")

  ;; Handle remote project separately
  (add-to-list 'projectile-known-projects "/ssh:droplet:/root/")

  (add-to-list 'projectile-known-projects "/ssh:home-worker:/home/ubuntu/")


  (add-to-list 'projectile-known-projects "/ssh:ben-devbox:/home/ben/lawo-homeapps")

  ;; TRAMP-specific caching
  (defun +projectile-enable-remote-caching ()
    (when (file-remote-p default-directory)
      (setq-local projectile-enable-caching t)
      (setq-local projectile-require-project-root nil)))

  (add-hook 'find-file-hook #'+projectile-enable-remote-caching)

  ;; Run this once at startup to discover local projects
  (projectile-discover-projects-in-search-path))

;; 2. Tell lsp-mode (if you’re using :tools lsp) not to watch vendor/
(after! lsp-mode
  ;; ignore any directory named “vendor”
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor$")

  ;; optional: disable the watcher‐count warning altogether
  (setq lsp-file-watch-threshold nil))


(after! eww
  (set-popup-rule! "^\\*eww\\*" :ignore t))

(after! circe
  ;; Function to fetch secrets via auth-source (Keychain or pass)
  (defun my/fetch-password (&rest params)
    (require 'auth-source)
    (if-let* ((match (car (apply #'auth-source-search params)))
              (secret (plist-get match :secret)))
        (if (functionp secret) (funcall secret) secret)
      (user-error "Password not found for %S" params)))

  ;; Add your IRC server configuration
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :nick "myNick"
      :sasl-username ,(+pass-get-user "irc/libera.chat") ;; if using pass
      :sasl-password ,(+pass-get-secret "irc/libera.chat")
      :channels ("#emacs")))
  )
(setq fancy-splash-image (concat doom-user-dir "splash.png"))

(defun run-devspace-dev ()
  "Open a vterm on the right at the Projectile project root and run 'devspace dev'."
  (interactive)
  (require 'projectile)
  (require 'vterm)
  (let* ((project-root (projectile-project-root))
         (default-directory project-root)
         (buffer-name "*vterm-devspace*"))
    (unless project-root
      (user-error "Not in a Projectile project"))
    (let ((vterm-window (split-window (selected-window) nil 'right)))
      (select-window vterm-window)
      (let ((vterm-buffer (get-buffer-create buffer-name)))
        (with-current-buffer (vterm vterm-buffer)
          (vterm-send-string "devspace dev")
          (vterm-send-return))))))

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)


;; minimal working setup
(use-package! consult-gh
  :after consult                     ; make sure consult is loaded first
  :init
  ;; where repo clones land when you hit "c" to clone
  (setq consult-gh-default-clone-directory "~/code/nep")
  ;; nicer UX (optional)
  (setq consult-gh-show-preview t
        consult-gh-preview-key "C-o")
  :config
  ;; enable out‑of‑the‑box keybindings inside issue/PR buffers
  (consult-gh-enable-default-keybindings))

;; Embark actions (clone/fork/etc.) – uncomment if you installed consult-gh-embark
;; (use-package! consult-gh-embark
;;   :after (consult-gh embark)
;;   :config (consult-gh-embark-mode +1))




(after! org-jira
  (defun my/org-jira-get-assigned-issues ()
    "Fetch Jira issues assigned to me using a custom JQL query."
    (interactive)
    (org-jira-get-issues-from-custom-jql
     (list
      (list :filename "assigned-to-me"
            :jql "assignee = currentUser() AND resolution = Unresolved ORDER BY updated DESC"
            :limit 100))))
  )

;; refresh buffer if file changes on disk (i.e. if I edit it outside emacs)
(global-auto-revert-mode 1)
(setq elfeed-feeds      '("https://this-week-in-rust.org/rss.xml"
                          ))
(load-file "~/.doom.d/lisp/launchers.el")
(setq display-line-numbers-type 'relative)

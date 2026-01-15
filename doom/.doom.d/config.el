;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(add-to-list 'load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")
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
(when (eq system-type 'darwin)
  ;; Make sure we use Homebrew and Nix paths inside Emacs.
  (setenv "PATH"
          (concat "/etc/profiles/per-user/ben/bin:"
                  "/opt/homebrew/bin:/opt/homebrew/sbin:"
                  (getenv "PATH")))
  (add-to-list 'exec-path "/etc/profiles/per-user/ben/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/sbin")

  ;; Force use of Homebrew pkg-config
  (setenv "PKG_CONFIG" "/opt/homebrew/bin/pkg-config")

  ;; *** This is the important bit ***
  ;; Overwrite any MacPorts PKG_CONFIG_PATH with Homebrew's pc dirs.
  (setenv "PKG_CONFIG_PATH"
          "/opt/homebrew/lib/pkgconfig:/opt/homebrew/share/pkgconfig"))



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
(setq doom-theme 'doom-nord)
(use-package! verb
  :commands verb-mode
  :mode ("\\.\\(http\\|rest\\)\\'" . verb-mode))

(doom/set-frame-opacity 95) ;; Set to 90% opacity (adjust as desired)
;; (set-frame-parameter nil 'alpha-background 90) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 95)) ; For all new frames henceforth
                                        ;
;; Ensure transparency applies to frames created by emacsclient
(defun my/restore-transparency (&optional frame)
  (when (display-graphic-p frame)
    ;; Apply both Doom's helper and raw parameter
    (doom/set-frame-opacity 95)
    (set-frame-parameter frame 'alpha-background 95)))

;; Run now for the current frame
(my/restore-transparency (selected-frame))

;; Run in future for any new frames (daemon mode)
(add-hook 'after-make-frame-functions #'my/restore-transparency)

;; Line numbers configured below (display-line-numbers-type 'relative)

;; PlantUML - point to nix-installed jar
(setq plantuml-jar-path (expand-file-name "~/.local/lib/plantuml.jar"))
(setq org-plantuml-jar-path (expand-file-name "~/.local/lib/plantuml.jar"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")
(setq org-noter-notes-search-path '("~/org/notes/"))

(setq org-roam-database-connector 'sqlite3) ;; use emacsql-sqlite3 binaryv
(setq org-roam-directory (file-truename "~/org/notes"))

(defun my/journal-file ()
  "Return today's journal file path."
  (expand-file-name (format-time-string "%Y-%m-%d.org") "~/org/journal/"))

(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("s" "Slack" entry (file+headline "~/org/inbox.org" "Inbox")
           "* %?\n  %a")
          ("j" "Journal" entry (file my/journal-file)
           "* %U\n- What happened that I didn't expect?\n- Why did it happen?\n- What will I do about it?\n- What am I grateful for?\n\n%?"))))

(after! org-roam
  (org-roam-db-autosync-mode))

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

;; Run golangci-lint for Go files only
(add-hook 'go-mode-hook (lambda () (setq-local flycheck-checker 'golangci-lint)))

(after! projectile
  ;; Only disable require-root for TRAMP (not globally)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t)

  (setq projectile-project-search-path
        '("~/code/"
          "~/org"
          "~/.dotfiles/"
          ))
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

;; IRC (Libera.Chat)
;; To set up: 1) Register at https://libera.chat/guides/registration
;;            2) pass insert irc/libera.chat (nick on line 1, password on line 2)
(after! circe
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :nick "onionpatch"
      :sasl-username ,(+pass-get-user "irc/libera.chat")
      :sasl-password ,(+pass-get-secret "irc/libera.chat")
      :channels ("#emacs"))))
(setq fancy-splash-image (concat doom-user-dir "splash.png"))

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

;; atomic-chrome for ghost-text (defer to avoid slowing startup)
(add-hook 'emacs-startup-hook #'atomic-chrome-start-server)


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
(load-file "~/.doom.d/lisp/launcher.el")
(after! transient
  (load! "lisp/devspace"))
(setq display-line-numbers-type 'relative)

(use-package! smudge
  :bind-keymap ("C-c ." . smudge-command-map)
  :custom
  (smudge-player-use-transient-map t)
  :config
  (require 'auth-source)
  (when-let ((auth (car (auth-source-search :host "spotify.com" :max 1))))
    (setq smudge-oauth2-client-id (plist-get auth :user)
          smudge-oauth2-client-secret (let ((secret (plist-get auth :secret)))
                                        (if (functionp secret) (funcall secret) secret))))
  (global-smudge-remote-mode))


(after! mu4e
  (setq mu4e-root-maildir (expand-file-name "~/Maildir")
        mu4e-get-mail-command "true"
        user-mail-address "bonions@nepgroup.com")

  ;; disable composing from emacs
  (defun my/mu4e-compose-disabled (&rest _args)
    (user-error "Compose disabled; use Apple Mail"))
  (advice-add #'mu4e-compose-new :around #'my/mu4e-compose-disabled))

;; Slack
;; Requires cookie stored in url-cookie for websocket (see github issue #555)
(use-package! slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t
        slack-prefer-current-team t)
  :config
  (let* ((token (auth-source-pick-first-password :host "slack.com"))
         (cookie (auth-source-pick-first-password :host "slack.com-cookie"))
         ;; Extract just xoxd-... part for url-cookie-store
         (d-cookie (car (split-string cookie ";"))))
    ;; Store d cookie for websocket
    (url-cookie-store "d" d-cookie nil ".slack.com" "/" t)
    (slack-register-team
     :name "nep-au-hub"
     :token token
     :cookie cookie
     :full-and-display-names t))

  ;; Org integration - store links from slack buffers
  (defun my/slack-get-message-at-point ()
    "Get slack message text at point."
    (when-let ((ts (get-text-property (point) 'ts)))
      (buffer-substring-no-properties
       (previous-single-property-change (1+ (point)) 'ts nil (point-min))
       (next-single-property-change (point) 'ts nil (point-max)))))

  (defun my/slack-store-link ()
    "Store org link to current slack buffer/message."
    (when (derived-mode-p 'slack-message-buffer-mode 'slack-thread-message-buffer-mode)
      (let* ((room (slack-buffer-room slack-current-buffer))
             (team (slack-buffer-team slack-current-buffer))
             (room-name (slack-room-name room team))
             (msg (string-trim (or (my/slack-get-message-at-point) "")))
             (desc (if (> (length msg) 50)
                       (concat (substring msg 0 47) "...")
                     msg)))
        (org-link-store-props
         :type "slack"
         :link (format "slack:%s" room-name)
         :description (format "Slack: #%s - %s" room-name desc)))))

  (org-link-set-parameters "slack" :store #'my/slack-store-link))

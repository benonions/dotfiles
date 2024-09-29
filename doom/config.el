;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq select-enable-clipboard t)
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ben Onions"
      user-mail-address "benonions@nepgroup.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
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
(add-to-list 'default-frame-alist '(alpha . 90))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))



;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(after! org
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (setq org-archive-location "~/org/archive/%s_archive::") ;; Adjust the path as needed
  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "inbox.org" "Tasks")
           "* TODO %?\n  %i\n  %a")

          ("p" "Priority Task" entry
           (file+headline "inbox.org" "Tasks")
           "* TODO [#A] %?\n  %i\n  %a")

          ("n" "Note" entry (file+headline "inbox.org" "Notes")
           "* %?\n  %i\n  %a")

          ("m" "Meeting" entry
           (file+headline "inbox.org" "Meetings")
           "* MEETING with %? :meeting:\n%U\n")

         ("f" "Note in new file" plain
                 (file (lambda () (expand-file-name
                                   (concat
                                    (format-time-string "%Y%m%d%H%M%S-")
                                    (read-string "Name: ")
                                    ".org")
                                   org-roam-directory)))
                 "#+title: %^{Title}\n\n* %?")

          ("s" "Someday Task" entry
           (file+headline "someday.org" "Someday")
           "* %?\n  %i\n  %a")
          ("j" "Jira ticket" entry
           (file+headline "inbox.org" "Jira Tickets")
           "* TODO %^{Ticket ID} - %^{Ticket Description}\n  %?")
          ("e" "Email" entry
           (file+headline "inbox.org" "Email")
           "* TODO %^{Subject}\nFrom: %^{From}\n  %i%?")
          ("l" "Slack message" entry
           (file+headline "inbox.org" "Slack")
           "* %^{From}%^{Topic}\n  %i%?")
         ))
  (setq org-agenda-files '("~/org/inbox.org"
                           "~/org/projects.org"
                           "~/org/someday.org"
                           "~/org/jira/TFC.org"
                           "~/org/next-actions.org"
                           ))


(map! :leader
      :desc "Open inbox" "o i" (lambda () (interactive) (find-file "~/org/inbox.org")))
)

;; Org-jira
(after! org-jira
   (setq jiralib-url "https://nepgroup.atlassian.net")
   (setq org-jira-working-dir "~/org/jira"))


(after! org-roam
  (setq org-roam-directory "~/org/notes"))


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
;; they are implemented.


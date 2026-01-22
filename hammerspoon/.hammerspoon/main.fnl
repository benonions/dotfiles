;; ~/.hammerspoon/main.fnl
;; AeroSpace workspace wrangler + leader modal (Fennel-first)
;;
;; Requires:
;;   - ~/.hammerspoon/fennel.lua (vendored)
;;   - AeroSpace at /opt/homebrew/bin/aerospace
;;
;; init.lua should boot this via: fennel.dofile(hs.configdir .. "/main.fnl")

(local hs _G.hs)
(require :hs.ipc)

;; ------------------------------------------------------------
;; Logging / helpers
;; ------------------------------------------------------------
(local log (hs.logger.new "main" "info"))

(fn info [msg]
  (log.i msg)
  (hs.printf "%s" msg))

;; ------------------------------------------------------------
;; Optional Spoon: FocusHighlight
;; ------------------------------------------------------------
(pcall
 (fn []
   (hs.loadSpoon "FocusHighlight")
   (when (and _G.spoon _G.spoon.FocusHighlight)
     (_G.spoon.FocusHighlight:start))))

;; ------------------------------------------------------------
;; Less obnoxious alerts
;; ------------------------------------------------------------
(set hs.alert.defaultStyle
     {:textSize 16
      :radius 6
      :padding 12
      :atScreenEdge 2
      :fadeInDuration 0.05
      :fadeOutDuration 0.15
      :fillColor {:white 0 :alpha 0.75}
      :textColor {:white 1 :alpha 1}
      :strokeWidth 0})

;; ------------------------------------------------------------
;; AeroSpace: hardcode the known-good path
;; ------------------------------------------------------------
(local AERO "/opt/homebrew/bin/aerospace")

(let [attrs (hs.fs.attributes AERO)]
  (when (not attrs)
    (hs.alert.show (.. "FATAL: AeroSpace not found at:\n" AERO) 6)
    (error (.. "AeroSpace not found at " AERO " (install via brew or update AERO path)")))
  (when (and attrs.mode (not (string.match attrs.mode "x")))
    (hs.alert.show (.. "WARNING: AeroSpace is not executable:\n" AERO) 6)))

;; ------------------------------------------------------------
;; AeroSpace async runner + FIFO queue (prevents process storms)
;; ------------------------------------------------------------
(local q [])
(var running false)

(fn aero-out [args on-done]
  (let [t (hs.task.new AERO
                       (fn [exitCode stdOut stdErr]
                         (when on-done
                           (on-done exitCode (or stdOut "") (or stdErr ""))))
                       args)]
    (if t
        (t:start)
        (when on-done (on-done 127 "" "hs.task.new() returned nil (launch failed)")))))

(fn run-next []
  (when (not running)
    (let [item (table.remove q 1)]
      (when item
        (set running true)
        (aero-out item.args
                  (fn [code out err]
                    (set running false)
                    (when item.onDone (item.onDone code out err))
                    (run-next)))))))

(fn enqueue [args on-done]
  (table.insert q {:args args :onDone on-done})
  (run-next))

;; ------------------------------------------------------------
;; Layouts (data-driven)
;; App names must match AeroSpace `list-windows --all` output.
;; ------------------------------------------------------------
(local layouts
  {:build {:focusWs "1"
           :rules [{:matchApp "emacs"           :ws "2"}
                   {:matchApp "Ghostty"         :ws "2"}
                   {:matchApp "Zen"             :ws "1"}
                   {:matchApp "Spotify"         :ws "5"}
                   {:matchApp "ChatGPT"         :ws "8"}
                   {:matchApp "Slack"           :ws "9"}
                   {:matchApp "Microsoft Teams" :ws "10"}]}

   :review {:rules [{:matchApp "Zen"             :ws "2"}
                    {:matchApp "ChatGPT"         :ws "2"}
                    {:matchApp "emacs"           :ws "4"}
                    {:matchApp "Ghostty"         :ws "3"}
                    {:matchApp "Slack"           :ws "9"}
                    {:matchApp "Microsoft Teams" :ws "10"}
                    {:matchApp "Spotify"         :ws "5"}]}

   :debug {:rules [{:matchApp "Ghostty"         :ws "3"}
                   {:matchApp "emacs"           :ws "3"}
                   {:matchApp "Zen"             :ws "2"}
                   {:matchApp "ChatGPT"         :ws "8"}
                   {:matchApp "Slack"           :ws "9"}
                   {:matchApp "Microsoft Teams" :ws "10"}
                   {:matchApp "Spotify"         :ws "5"}]}})

(fn workspace-for [layout-name app-name]
  (let [layout (. layouts layout-name)]
    (when layout
      (var found nil)
      (each [_ r (ipairs (or layout.rules []))]
        (when (= app-name r.matchApp)
          (set found r.ws)))
      found)))

;; ------------------------------------------------------------
;; AeroSpace window parsing
;; Expected format per line: ID | APP | TITLE
;; ------------------------------------------------------------
(fn parse-windows [out]
  (local wins [])
  (each [line (string.gmatch out "[^\r\n]+")]
    (let [(id app title) (string.match line "^%s*(%d+)%s*|%s*([^|]+)%s*|%s*(.-)%s*$")]
      (when (and id app)
        (table.insert wins {:id (tonumber id)
                            :app (string.gsub app "%s+$" "")
                            :title (or title "")}))))
  wins)

(fn list-windows-async [on-done]
  (enqueue ["list-windows" "--all"]
           (fn [code out err]
             (if (not= code 0)
                 (do
                   (info (string.format "ERROR: aerospace list-windows failed (code=%d)\n%s"
                                        code (or err "")))
                   (on-done []))
                 (on-done (parse-windows (or out "")))))))

;; ------------------------------------------------------------
;; Move + logging (single truth point)
;; ------------------------------------------------------------
(fn move-window-to-workspace [win ws]
  (enqueue ["move-node-to-workspace" ws "--window-id" (tostring win.id) "--fail-if-noop"]
           (fn [code out err]
             (let [e (or err "")]
               (if (= code 0)
                   (info (string.format "[MOVE OK]   %s (%d) -> ws %s"
                                        win.app win.id ws))
                   (if (or (= e "") (string.match e "already") (string.match e "noop"))
                       (info (string.format "[MOVE SKIP] %s (%d) already in ws %s"
                                            win.app win.id ws))
                       (info (string.format "[MOVE FAIL] %s (%d) -> ws %s | %s"
                                            win.app win.id ws (string.gsub e "\n" " ")))))))))

;; ------------------------------------------------------------
;; Apply layout executor
;; ------------------------------------------------------------
(fn focus-workspace [ws]
  (enqueue ["workspace" ws]
           (fn [code out err]
             (let [e (or err "")]
               (if (= code 0)
                   (info (string.format "[FOCUS OK] ws %s" ws))
                   (info (string.format "[FOCUS FAIL] ws %s | %s"
                                        ws (string.gsub e "\n" " "))))))))

(fn apply-layout [layout-name]
  (let [layout (. layouts layout-name)]
    (if (not layout)
        (do
          (hs.alert.show (.. "Unknown layout: " (tostring layout-name)) 1.2)
          (info (.. "applyLayout(): unknown layout " (tostring layout-name))))
        (do
          (hs.alert.show (.. "LAYOUT: " (tostring layout-name)) 0.35)
          (info (string.format "applyLayout(%s)" (tostring layout-name)))

          (list-windows-async
            (fn [wins]
              (info (string.format "Found windows: %d" (length wins)))

              (each [_ w (ipairs wins)]
                (let [ws (workspace-for layout-name w.app)]
                  (when ws
                    (move-window-to-workspace w ws))))

              (when layout.focusWs
                (focus-workspace layout.focusWs))))))))

;; ------------------------------------------------------------
;; Leader modal (Opt+Space)
;; ------------------------------------------------------------
(local leader (hs.hotkey.modal.new ["alt"] "space"))
(var menuAlertId nil)

(set leader.entered
  (fn []
    (set menuAlertId
      (hs.alert.show "MODE MENU\n\nB → Build\nR → Review\nD → Debug\nQ → Exit\n" 999999))))

(set leader.exited
  (fn []
    (when menuAlertId
      (hs.alert.closeSpecific menuAlertId)
      (set menuAlertId nil))))

(leader:bind [] "q"      (fn [] (leader:exit)))
(leader:bind [] "escape" (fn [] (leader:exit)))

(leader:bind [] "b" (fn [] (apply-layout :build) (leader:exit)))
(leader:bind [] "r" (fn [] (apply-layout :review) (leader:exit)))
(leader:bind [] "d" (fn [] (apply-layout :debug) (leader:exit)))

;; ------------------------------------------------------------
;; Sanity hotkeys
;; ------------------------------------------------------------
;; Opt+T prints aerospace version + AERO path
(hs.hotkey.bind ["alt"] "t"
  (fn []
    (hs.alert.show "AeroSpace sanity check…" 0.4)
    (enqueue ["version"]
             (fn [code out err]
               (info (string.format "AERO=%s\nexit=%d\n%s%s"
                                    AERO code (or out "") (or err "")))))))

;; Opt+Y dumps list-windows --all to the Hammerspoon console
(hs.hotkey.bind ["alt"] "y"
  (fn []
    (enqueue ["list-windows" "--all"]
             (fn [code out err]
               (info (string.format "aerospace list-windows exit=%d\n%s%s"
                                    code (or out "") (or err "")))))))

;; ------------------------------------------------------------
;; Export globals (console-friendly)
;; ------------------------------------------------------------
(tset _G "applyLayout" apply-layout)

(tset _G "HS" (or _G.HS {}))
(tset _G.HS "applyLayout" apply-layout)
(tset _G.HS "layouts" layouts)
(tset _G.HS "aeroPath" AERO)

(info "=== main.fnl loaded ===")
(info (string.format "AeroSpace path: %s" AERO))
;; Export Fennel functions as Lua globals callable via `hs -c`
(tset _G "HS" (or _G.HS {}))
(local HS _G.HS)

(fn HS.layout [name]
  ;; call your apply-layout
    (apply-layout name)

  (print (.. "layout: " name))
  (hs.alert.show (.. "layout " name) 0.4))

(print "HS.layout ready")

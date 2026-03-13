#!/usr/bin/env bash

# Shared helpers for scheduled Codex jobs.

setup_codex_job_env() {
  export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/usr/local/bin:${PATH:-/usr/bin:/bin}"
  export NOTIFY_ON_FAILURE="${NOTIFY_ON_FAILURE:-1}"
  export CODEX_MODEL="${CODEX_MODEL:-gpt-5.4}"
  export CODEX_REASONING_EFFORT="${CODEX_REASONING_EFFORT:-xhigh}"
  export CODEX_SERVICE_TIER="${CODEX_SERVICE_TIER:-fast}"
}

notify_failure() {
  local title="$1"
  local message="$2"

  if [ "${NOTIFY_ON_FAILURE:-1}" != "1" ]; then
    return 0
  fi

  if [ "$(uname -s)" != "Darwin" ] || [ ! -x /usr/bin/osascript ]; then
    return 0
  fi

  /usr/bin/osascript - "$title" "$message" <<'APPLESCRIPT' >/dev/null 2>&1 || true
on run argv
  display notification (item 2 of argv) with title (item 1 of argv) sound name "Basso"
end run
APPLESCRIPT
}

resolve_codex() {
  if [ -n "${CODEX_BIN:-}" ] && [ -x "${CODEX_BIN}" ]; then
    return 0
  fi

  if command -v codex >/dev/null 2>&1; then
    CODEX_BIN="$(command -v codex)"
    return 0
  fi

  for candidate in "$HOME/.local/bin/codex" "/opt/homebrew/bin/codex" "/usr/local/bin/codex"; do
    if [ -x "$candidate" ]; then
      CODEX_BIN="$candidate"
      return 0
    fi
  done

  return 1
}

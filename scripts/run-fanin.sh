#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/lib/codex-job-common.sh
. "$SCRIPT_DIR/lib/codex-job-common.sh"
setup_codex_job_env

ORG_DIR="${ORG_DIR:-$HOME/org}"
SLACK_FILE="${SLACK_FILE:-$ORG_DIR/inbox/slack-inbox.org}"
JIRA_FILE="${JIRA_FILE:-$ORG_DIR/inbox/jira-inbox.org}"
MASTER_FILE="${MASTER_FILE:-$ORG_DIR/inbox.org}"
LOG_DIR="${LOG_DIR:-$ORG_DIR/.logs}"
AUTO_COMMIT="${AUTO_COMMIT:-0}"

mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/run-fanin.log"

LOCK_DIR="${TMPDIR:-/tmp}/run-fanin.lockdir"
if ! mkdir "$LOCK_DIR" 2>/dev/null; then
  echo "run-fanin: another run is in progress; exiting."
  exit 0
fi
cleanup() { rmdir "$LOCK_DIR" 2>/dev/null || true; }
trap cleanup EXIT

if [ ! -f "$SLACK_FILE" ] && [ ! -f "$JIRA_FILE" ]; then
  printf "run-fanin: no input files found; expected at least one of:\n  %s\n  %s\n" "$SLACK_FILE" "$JIRA_FILE" | tee -a "$LOG_FILE"
  notify_failure "Codex fan-in failed" "No inbox source files found. See $LOG_FILE."
  exit 1
fi

# shellcheck disable=SC2016 # Intentionally pass literal '$tfc-*' token for Codex prompt expansion.
PROMPT='Run $tfc-org-fanin now. Maintain ~/org/inbox.org as rolling canonical inbox from ~/org/inbox/slack-inbox.org and ~/org/inbox/jira-inbox.org. INTAKE_ONLY. No external writes. Update in place with dedupe by SOURCE+LINK.'

if ! resolve_codex; then
  echo "run-fanin: codex CLI not found; set CODEX_BIN or add codex to PATH." | tee -a "$LOG_FILE"
  notify_failure "Codex fan-in failed" "Codex CLI not found. See $LOG_FILE."
  exit 1
fi

if ! "$CODEX_BIN" exec \
  --cd "$ORG_DIR" \
  --skip-git-repo-check \
  --sandbox workspace-write \
  -m "$CODEX_MODEL" \
  -c "model_reasoning_effort=\"$CODEX_REASONING_EFFORT\"" \
  -c "service_tier=\"$CODEX_SERVICE_TIER\"" \
  -c approval_policy="never" \
  "$PROMPT" >>"$LOG_FILE" 2>&1; then
  echo "run-fanin: codex exec failed; see $LOG_FILE"
  notify_failure "Codex fan-in failed" "codex exec failed. See $LOG_FILE."
  exit 1
fi

if [ "$AUTO_COMMIT" = "1" ] && git -C "$ORG_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  if ! git -C "$ORG_DIR" diff --quiet -- "$MASTER_FILE"; then
    git -C "$ORG_DIR" add -- "$MASTER_FILE"
    git -C "$ORG_DIR" commit -m "inbox: fan-in slack+jira $(date '+%Y-%m-%d %H:%M')" >/dev/null 2>&1 || true
    echo "run-fanin: committed changes to $MASTER_FILE"
  fi
fi

echo "run-fanin: completed"

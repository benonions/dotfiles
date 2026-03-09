#!/usr/bin/env bash
set -euo pipefail

ORG_DIR="${ORG_DIR:-$HOME/org}"
INBOX_DIR="${INBOX_DIR:-$ORG_DIR/inbox}"
OUT_FILE="${OUT_FILE:-$INBOX_DIR/jira-inbox.org}"
LOG_DIR="${LOG_DIR:-$ORG_DIR/.logs}"
mkdir -p "$INBOX_DIR" "$LOG_DIR"

LOCK_DIR="${TMPDIR:-/tmp}/run-jira-intake.lockdir"
if ! mkdir "$LOCK_DIR" 2>/dev/null; then
  echo "run-jira-intake: another run is in progress; exiting."
  exit 0
fi
cleanup() { rmdir "$LOCK_DIR" 2>/dev/null || true; }
trap cleanup EXIT

TMP_FILE="$(mktemp "${TMPDIR:-/tmp}/jira-inbox.XXXXXX.org")"
LOG_FILE="$LOG_DIR/run-jira-intake.log"

PROMPT='Run $tfc-atlassian-chief-of-staff and return org-mode only. Produce an Action Docket with concrete actions, decisions, and delegations (not a notification summary).'

if ! codex exec \
  --cd "$ORG_DIR" \
  --skip-git-repo-check \
  --sandbox workspace-write \
  -c model_verbosity="high" \
  -c approval_policy="never" \
  --output-last-message "$TMP_FILE" \
  "$PROMPT" >>"$LOG_FILE" 2>&1; then
  echo "run-jira-intake: codex exec failed; see $LOG_FILE"
  rm -f "$TMP_FILE"
  exit 1
fi

if [ ! -s "$TMP_FILE" ]; then
  echo "run-jira-intake: empty output; leaving existing file unchanged."
  rm -f "$TMP_FILE"
  exit 1
fi

mv "$TMP_FILE" "$OUT_FILE"
echo "run-jira-intake: wrote $OUT_FILE"

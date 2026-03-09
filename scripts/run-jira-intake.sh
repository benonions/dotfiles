#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/lib/codex-job-common.sh
. "$SCRIPT_DIR/lib/codex-job-common.sh"
setup_codex_job_env

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

# shellcheck disable=SC2016 # Intentionally pass literal '$tfc-*' token for Codex prompt expansion.
PROMPT='Run $tfc-atlassian-chief-of-staff and return org-mode only. Produce an Action Docket with concrete actions, decisions, and delegations (not a notification summary).'

if ! resolve_codex; then
  echo "run-jira-intake: codex CLI not found; set CODEX_BIN or add codex to PATH." | tee -a "$LOG_FILE"
  notify_failure "Codex Jira intake failed" "Codex CLI not found. See $LOG_FILE."
  rm -f "$TMP_FILE"
  exit 1
fi

if ! "$CODEX_BIN" exec \
  --cd "$ORG_DIR" \
  --skip-git-repo-check \
  --sandbox workspace-write \
  -m "$CODEX_MODEL" \
  -c "model_reasoning_effort=\"$CODEX_REASONING_EFFORT\"" \
  -c "service_tier=\"$CODEX_SERVICE_TIER\"" \
  -c model_verbosity="high" \
  -c approval_policy="never" \
  --output-last-message "$TMP_FILE" \
  "$PROMPT" >>"$LOG_FILE" 2>&1; then
  echo "run-jira-intake: codex exec failed; see $LOG_FILE"
  notify_failure "Codex Jira intake failed" "codex exec failed. See $LOG_FILE."
  rm -f "$TMP_FILE"
  exit 1
fi

if [ ! -s "$TMP_FILE" ]; then
  echo "run-jira-intake: empty output; leaving existing file unchanged."
  notify_failure "Codex Jira intake failed" "Codex returned empty output. See $LOG_FILE."
  rm -f "$TMP_FILE"
  exit 1
fi

mv "$TMP_FILE" "$OUT_FILE"
echo "run-jira-intake: wrote $OUT_FILE"

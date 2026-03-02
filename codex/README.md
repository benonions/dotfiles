# Codex dotfiles

This Stow package manages stable Codex configuration in:

- `~/.codex/config.toml`
- `~/.codex/rules/default.rules`
- `~/.codex/global/anti_corruption.md`

The baseline is derived from hardened drafts in:

- `/Users/ben/code/nep/nep-platform/broadcast-refresh/artifacts/state/drafts/codex/`

Included profiles:

- `ben` (default)
- `safe-local`
- `harness-gate`

## Intentionally not source-controlled

Runtime and machine-local Codex state should remain outside this repo, including:

- `~/.codex/auth.json`
- `~/.codex/history.jsonl`
- `~/.codex/state_*.sqlite*`
- `~/.codex/sessions/`
- `~/.codex/log/`
- `~/.codex/tmp/`
- `~/.codex/shell_snapshots/`
- cache files (`models_cache.json`, `cloud-requirements-cache.json`, etc.)

`./install` will stow the `codex/` package and link only tracked files.

# Community skill sources (vendored)

These skills are copied locally for review/audit.

## Upstreams

### melodic-software/claude-code-plugins
- Repository: https://github.com/melodic-software/claude-code-plugins
- Commit pinned: `e7f64a91cae81597d68d946d0c6e737687693e80`
- Commit date: 2026-02-16

Copied paths:

- `plugins/formal-specification/skills/asyncapi-design`
- `plugins/spec-driven-development/skills/asyncapi-authoring`

### xenodium/emacs-skills

- Repository: https://github.com/xenodium/emacs-skills
- Commit pinned: `de7adccbc4aef5f4e1e7ebc7a487bdcd7f95509a`
- Commit date: 2026-03-02

Copied paths:

- `skills/d2`
- `skills/describe`
- `skills/dired`
- `skills/emacsclient`
- `skills/file-links`
- `skills/gnuplot`
- `skills/highlight`
- `skills/mermaid`
- `skills/open`
- `skills/plantuml`
- `skills/select`

Local patch:

- `skills/mermaid/SKILL.md`: replaced hardcoded `/opt/homebrew/bin/chromium` with dynamic Chromium resolution for Linux/macOS compatibility.

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

### obra/superpowers

- Repository: https://github.com/obra/superpowers
- Commit pinned: `33e55e60b2efcb69509bc233dfc128112012b2c8`
- Commit date: 2026-03-09

Copied paths:

- `skills/brainstorming`
- `skills/dispatching-parallel-agents`
- `skills/executing-plans`
- `skills/finishing-a-development-branch`
- `skills/receiving-code-review`
- `skills/requesting-code-review`
- `skills/subagent-driven-development`
- `skills/systematic-debugging`
- `skills/test-driven-development`
- `skills/using-git-worktrees`
- `skills/using-superpowers`
- `skills/verification-before-completion`
- `skills/writing-plans`
- `skills/writing-skills`

Local patches:

- None.

### anthropics/knowledge-work-plugins
- Repository: https://github.com/anthropics/knowledge-work-plugins
- Commit pinned: `1316b6508366108158cf08503363d4b4cbc699e5`
- Commit date: 2026-03-10

Copied roots:

- `bio-research`
- `cowork-plugin-management`
- `customer-support`
- `data`
- `design`
- `engineering`
- `enterprise-search`
- `finance`
- `human-resources`
- `legal`
- `marketing`
- `operations`
- `partner-built/apollo`
- `partner-built/brand-voice`
- `partner-built/common-room`
- `partner-built/slack`
- `product-management`
- `productivity`
- `sales`

Local notes:

- Vendored full plugin roots, not just individual skill folders, so skill-local references like `../../CONNECTORS.md` continue to resolve under Codex.
- Renamed duplicate skill frontmatter names for Codex uniqueness:
  - `account-research` in `partner-built/common-room` → `common-room-account-research`
  - `call-prep` in `partner-built/common-room` → `common-room-call-prep`
  - `code-review` in `engineering` → `engineering-code-review`
  - `competitive-analysis` in `marketing` → `marketing-competitive-analysis`
  - `competitive-analysis` in `product-management` → `product-management-competitive-analysis`
  - `prospect` in `partner-built/apollo` → `apollo-prospect`
  - `prospect` in `partner-built/common-room` → `common-room-prospect`

### hashicorp/agent-skills
- Repository: https://github.com/hashicorp/agent-skills
- Commit pinned: `876a095c71079e4c112fb1de2f1a66dca0ffea23`
- Commit date: 2026-03-04

Copied roots:

- `README.md`
- `LICENSE`
- `packer`
- `terraform`

Local notes:

- Vendored the product roots under `community/hashicorp-agent-skills/` so skill-local `references/`, `scripts/`, `assets/`, and plugin metadata continue to resolve unchanged.
- No frontmatter renames were needed; all imported skill names were unique in this repo.

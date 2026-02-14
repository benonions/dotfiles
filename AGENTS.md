# Agent Instructions

## Tooling Policy
- Always support both Linux and macOS.
- Prefer shared/common cross-platform configuration first, then add the minimal platform-specific overrides required for Darwin or Linux.
- Prefer Nix and declarative tooling by default.
- Do not suggest Homebrew or ad-hoc/non-declarative installs unless the user explicitly requests them.
- Before suggesting or adding any package, check the nixpkgs version pinned by this repo/user and use that version for package availability/attributes.
- On macOS, verify Darwin compatibility for the package/module before proposing changes.
- Determine and edit the correct target `.nix` file for the host/setup (for example, system-level Darwin config vs Home Manager user config) instead of making generic `.nix` changes.
- If no suitable Nix installation target is available after those checks, Homebrew may be used as a fallback.
- Homebrew fallback is allowed only after explicitly verifying the package exists in Homebrew.
- Even when using Homebrew fallback, keep configuration managed through the existing Home Manager workflow where applicable.

# .dotfiles

Personal dotfiles using a hybrid **Nix + Stow** approach for cross-platform configuration.

## Prerequisites

- [Git](https://git-scm.com/)
- [Nix](https://nixos.org/) (with flakes enabled)
- [home-manager](https://github.com/nix-community/home-manager)
- [zsh](https://www.zsh.org/)
- [GNU Stow](https://www.gnu.org/software/stow/)

## Installation

Clone to `~/.dotfiles`:

```bash
git clone https://github.com/yourusername/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
```

### macOS (nix-darwin)

```bash
darwin-rebuild switch --flake .#<hostname>
```

### NixOS

```bash
nixos-rebuild switch --flake .#<hostname>
```

### Ubuntu (standalone home-manager)

```bash
home-manager switch --flake .#<username>
```

### Stow configs

For any platform, stow individual app configs:

```bash
cd ~/.dotfiles
stow nvim tmux zsh starship  # etc.
```

### Post-install

First-time tmux setup - install plugins with `prefix + I` (Ctrl-b + I).

## Architecture

| Layer | Platform | Purpose |
|-------|----------|---------|
| **nix-darwin** | macOS | System packages, Homebrew, macOS defaults |
| **NixOS** | Linux | Full system configuration |
| **home-manager** | All | User packages and configs |
| **Stow** | All | App configs (nvim, tmux, zsh, etc.) |

### Why Stow for some configs?

Configs like nvim, tmux, and zsh use Stow instead of home-manager because:

1. **Immediate edits** - Changes apply instantly without a rebuild
2. **Complex structures** - Neovim's lua/ tree is easier to manage as plain files
3. **Editor tooling** - LSP, linters, and plugins expect standard paths
4. **Portability** - Works on systems without Nix (quick setup on a remote server)

## What's Included

- **Shell**: zsh + starship prompt
- **Editors**: neovim, doom emacs, helix
- **Terminals**: ghostty, alacritty
- **Multiplexer**: tmux (with TPM)
- **Window managers**: aerospace (macOS), i3 (Linux)
- **Dev tools**: lazygit, k9s, fzf, ripgrep, bat, eza, zoxide

## Machine Configs

macOS machines are defined in `nix-darwin/flake.nix`.

## License

Feel free to copy anything you want.

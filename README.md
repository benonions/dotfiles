# .dotfiles

Personal dotfiles using a hybrid **Nix + Stow** approach for cross-platform configuration.

## Fresh Machine Bootstrap

### 1. Install Nix

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

### 2. Clone dotfiles

```bash
git clone https://github.com/yourusername/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
```

### 3. Get secrets from 1Password

```bash
mkdir -p ~/.sops
op read "op://Private/dotfiles age key/notesPlain" > ~/.sops/key.txt
chmod 600 ~/.sops/key.txt
```

### 4. Apply Nix config

This installs all packages (including stow, sops) and configures the system.

```bash
# macOS (installs nix-darwin + applies config)
nix run nix-darwin -- switch --flake ~/.dotfiles#$(scutil --get LocalHostName)

# Ubuntu (standalone home-manager)
nix run home-manager/release-25.05 -- switch --flake ~/.dotfiles#$USER

# NixOS
sudo nixos-rebuild switch --flake ~/.dotfiles#$(hostname -s)
```

### 5. Stow configs + decrypt secrets

```bash
./install
```

After this step, the `nrs`/`nrb`/`nrt` aliases are available for daily use.

### 6. Post-install

- Open a new terminal to load zsh config
- Tmux plugins are installed automatically by `./install`

## Daily Usage

| Command | What it does |
|---------|--------------|
| `nrs` | Nix rebuild switch (auto-detects OS) |
| `nrb` | Nix rebuild build (test without switching) |
| `nrt` | Nix rebuild test (dry-run) |
| `./install` | Re-stow configs + decrypt secrets |

## Architecture

```
flake.nix                    # Root flake for all platforms
modules/
  home/common.nix            # Shared packages (all platforms)
  home/darwin.nix            # macOS-specific
  home/linux.nix             # Linux-specific
  darwin/configuration.nix   # macOS system config + Homebrew
```

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

Defined in `flake.nix`:
- `darwinConfigurations.Benjamins-MacBook-Pro-2`
- `darwinConfigurations.Bens-BlackBook-Pro`
- `homeConfigurations.ben` (Ubuntu/Linux standalone)

## License

Feel free to copy anything you want.
